# Build (name, downloads, repo_url) tables for PyPI, filtered to packages with
# a resolvable GitHub repo URL.

library(httr2)
library(jsonlite)
library(purrr)
library(dplyr)
library(readr)
library(stringr)
library(tibble)

# ---- config ----------------------------------------------------------

WORKING_SAMPLE_TAIL_SIZE <- 40000L # random draw size, per registry, outside the known head
TOP_N_HEAD <- 15000L # deterministic head inclusion, per registry
REQS_PER_SEC_META <- 8 # per-package repo-metadata lookups (pypi.org, registry.npmjs.org)
NPM_DOWNLOAD_BATCH <- 128L
CLICKHOUSE_URL <- "https://sql-clickhouse.clickhouse.com"
CLICKHOUSE_PAGE_SIZE <- 100000L # server-enforced max rows per query on the public `demo` user
OUT_DIR <- "repo-data-out"
dir.create(OUT_DIR, showWarnings = FALSE)

# ---- shared helpers ----------------------------------------------------

#' GET a URL as parsed JSON, with retry/backoff on 429/5xx.
req_json <- function(url, headers = list(), reqs_per_sec = NULL) {
    req <- request(url) |>
        req_retry(max_tries = 5, backoff = \(i) 2^i) |>
        req_error(is_error = \(resp) FALSE) # inspect status ourselves
    for (nm in names(headers)) req <- req_headers(req, !!nm := headers[[nm]])
    if (!is.null(reqs_per_sec)) req <- req_throttle(req, rate = reqs_per_sec)
    resp <- req_perform(req)
    if (resp_status(resp) >= 400) {
        return(NULL)
    }
    resp_body_json(resp, simplifyVector = FALSE)
}

github_url_re <- "github\\.com[/:]([^/\\s\"']+)/([^/\\s\"'#]+)"
github_shorthand_re <- "^github:([^/\\s\"']+)/([^/\\s\"'#]+)$"

#' Normalize any github.com URL, or a "github:owner/repo" shorthand
#' (npm's `repository` field allows this), to https://github.com/<owner>/<repo>
normalize_github_url <- function(url) {
    if (is.null(url) || is.na(url) || !nzchar(url)) {
        return(NA_character_)
    }
    m <- str_match(url, github_url_re)
    if (is.na(m[1, 1])) {
        m <- str_match(url, github_shorthand_re)
    }
    if (is.na(m[1, 1])) {
        return(NA_character_)
    }
    owner <- m[1, 2]
    repo <- str_remove(m[1, 3], "\\.git$")
    str_glue("https://github.com/{owner}/{repo}")
}

#' First github.com URL found among a set of candidate URL strings, or NA.
find_github_url <- function(candidates) {
    candidates <- candidates[!vapply(candidates, is.null, logical(1))]
    candidates <- unlist(candidates, use.names = FALSE)
    for (url in candidates) {
        norm <- normalize_github_url(url)
        if (!is.na(norm)) {
            return(norm)
        }
    }
    NA_character_
}

#' Run a read-only SQL query against ClickHouse's public playground (the
#' `demo` user), which mirrors the same PyPI downloads dataset BigQuery's
#' `bigquery-public-data.pypi.file_downloads` does, updated monthly. This
#' replaces pypistats.org, which rate-limits per-package polling far too
#' aggressively to poll tens of thousands of packages (confirmed by testing:
#' repeated 429s within seconds, backoff never catching up). hugovk's own
#' top-pypi-packages generator itself switched to querying this endpoint
#' (see https://github.com/hugovk/top-pypi-packages/blob/main/clickhouse.py).
#' The `demo` user caps any single query at CLICKHOUSE_PAGE_SIZE rows, so
#' pypi_downloads_full() below pages through with LIMIT/OFFSET.
#' Returns the result as a character matrix (ClickHouse's JSONCompact
#' encodes all values as strings to avoid UInt64/Int64 precision loss, so
#' there's no point asking jsonlite for anything fancier). simplifyVector
#' matters a lot here — the naive per-row list parse is ~100x slower at
#' 100k+ rows and was the actual bottleneck in early testing, not the network.
clickhouse_query <- function(sql) {
    resp <- request(CLICKHOUSE_URL) |>
        req_url_query(user = "demo", default_format = "JSONCompact") |>
        req_body_raw(sql) |>
        req_retry(max_tries = 5, backoff = \(i) 2^i) |>
        req_perform()
    resp_body_json(resp, simplifyVector = TRUE)$data
}

#' 1a/1b combined: full PyPI download-count population (~870k packages, last
#' complete calendar month), paginated in chunks of CLICKHOUSE_PAGE_SIZE.
#' Typically ~9 requests, well under a minute, no rate limiting encountered.
pypi_downloads_full <- function() {
    base_sql <- "
    SELECT SUM(count) AS downloads, project
    FROM pypi.pypi_downloads_per_month
    WHERE month = (
      SELECT max(month) FROM pypi.pypi_downloads_per_month
      WHERE month < toStartOfMonth(now())
    )
    GROUP BY project
    ORDER BY downloads DESC
    LIMIT %d OFFSET %d"

    pages <- list()
    offset <- 0L
    repeat {
        rows <- clickhouse_query(sprintf(base_sql, CLICKHOUSE_PAGE_SIZE, offset))
        n <- if (is.matrix(rows)) nrow(rows) else length(rows) # length(rows) == 0 for an empty result
        if (n == 0) break
        pages[[length(pages) + 1]] <- tibble(
            downloads = as.numeric(rows[, 1]),
            name = rows[, 2]
        )
        if (n < CLICKHOUSE_PAGE_SIZE) break
        offset <- offset + CLICKHOUSE_PAGE_SIZE
    }
    bind_rows(pages)
}

#' Repo URLs for PyPI: info.project_urls (free-text keys) + info.home_page.
pypi_repo_url_one <- function(name) {
    body <- req_json(
        str_glue("https://pypi.org/pypi/{URLencode(name)}/json"),
        reqs_per_sec = REQS_PER_SEC_META
    )
    if (is.null(body)) {
        return(NA_character_)
    }
    info <- body$info
    candidates <- c(unlist(info$project_urls, use.names = FALSE), info$home_page)
    find_github_url(candidates)
}

#' Build pypi table ...
cli::cli_alert_info ("PyPI: fetching full download-count population via ClickHouse (fast)...")
downloads_tbl <- pypi_downloads_full()

cli::cli_alert_info ("PyPI: building working sample (head + random tail)...")
head_tbl <- downloads_tbl |> slice_max(downloads, n = TOP_N_HEAD)
tail_pool <- downloads_tbl |> anti_join(head_tbl, by = "name")
tail_tbl <- tail_pool |> slice_sample(n = min(WORKING_SAMPLE_TAIL_SIZE, nrow(tail_pool)))
working_sample <- bind_rows(head_tbl, tail_tbl)

cli::cli_alert_info ("PyPI: resolving GitHub repo URLs for working sample (slow)...")
pypi_tbl <- working_sample |>
    mutate(repo_url = map_chr(name, pypi_repo_url_one, .progress = TRUE)) |>
    filter(!is.na(repo_url)) |>
    select(name, downloads, repo_url)
write_csv(pypi_tbl, file.path(OUT_DIR, "pypi.csv"))
