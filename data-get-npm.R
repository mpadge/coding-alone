#!/usr/bin/env Rscript
#
# Build (name, downloads, repo_url) tables for PyPI and npm, filtered to
# packages with a resolvable GitHub repo URL. See repo-data.md for the
# rationale and sizing (working sample of tens of thousands per registry).
#
# No BigQuery, no ecosyste.ms — plain REST/HTTP only. PyPI downloads come
# from ClickHouse's public SQL playground rather than pypistats.org, which
# rate-limits per-package polling too aggressively to be usable at this scale.

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


#' list of npm package names (~4.3M), via all-the-package-names.
npm_all_names <- function() {
    meta <- req_json("https://registry.npmjs.org/all-the-package-names/latest")
    tarball_url <- meta$dist$tarball
    tmp_tgz <- tempfile(fileext = ".tgz")
    tmp_dir <- tempfile()
    dir.create(tmp_dir)
    req_perform(request(tarball_url), path = tmp_tgz)
    untar(tmp_tgz, exdir = tmp_dir)
    names_json <- list.files(tmp_dir, pattern = "names\\.json$", recursive = TRUE, full.names = TRUE)[1]
    fromJSON(names_json)
}

#' download counts, up to 128 names per request.
npm_downloads_batch <- function(names_chunk) {
    qs <- paste(names_chunk, collapse = ",")
    body <- req_json(str_glue("https://api.npmjs.org/downloads/point/last-month/{qs}"))
    if (is.null(body)) {
        return(tibble(name = names_chunk, downloads = NA_real_))
    }
    map2_dfr(names(body), body, \(nm, v) {
        tibble(name = nm, downloads = if (is.null(v$downloads)) NA_real_ else as.numeric(v$downloads))
    })
}

npm_downloads_many <- function(names_vec) {
    chunks <- split(names_vec, ceiling(seq_along(names_vec) / NPM_DOWNLOAD_BATCH))
    map_dfr(chunks, npm_downloads_batch, .progress = TRUE)
}

#' Repo URL for one npm package: full registry doc's repository/homepage.
#' (Must use the full doc, not the abbreviated `install-v1+json` metadata,
#' which omits `repository`.)
npm_repo_url_one <- function(name) {
    body <- req_json(
        str_glue("https://registry.npmjs.org/{URLencode(name)}"),
        reqs_per_sec = REQS_PER_SEC_META
    )
    if (is.null(body)) {
        return(NA_character_)
    }
    repo <- body$repository
    repo_url <- if (is.list(repo)) repo$url else repo
    find_github_url(c(repo_url, body$homepage))
}

# Build npm table ...
cli::cli_alert_info ("npm: fetching full name list...")
all_names <- npm_all_names()

cli::cli_alert_info ("npm: fetching downloads for full population in batches of 128 (slow)...")
downloads_tbl <- npm_downloads_many(all_names) |> filter(!is.na(downloads))

cli::cli_alert_info ("npm: building working sample (head + random tail)...")
head_tbl <- downloads_tbl |> slice_max(downloads, n = TOP_N_HEAD)
tail_pool <- downloads_tbl |> anti_join(head_tbl, by = "name")
tail_tbl <- tail_pool |> slice_sample(n = min(WORKING_SAMPLE_TAIL_SIZE, nrow(tail_pool)))
working_sample <- bind_rows(head_tbl, tail_tbl)

cli::cli_alert_info ("npm: resolving GitHub repo URLs for working sample (slow)...")
npm_tbl <- working_sample |>
    mutate(repo_url = map_chr(name, npm_repo_url_one, .progress = TRUE)) |>
    filter(!is.na(repo_url)) |>
    select(name, downloads, repo_url)
write_csv(npm_tbl, file.path(OUT_DIR, "npm.csv"))
