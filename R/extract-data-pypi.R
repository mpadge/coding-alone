# Build (name, downloads, repo_url) table for PyPI, filtered to packages with
# a resolvable GitHub repo URL.

# ---- config ----------------------------------------------------------

WORKING_SAMPLE_TAIL_SIZE <- 40000L # random draw size, outside the known head
TOP_N_HEAD <- 15000L # deterministic head inclusion
MAX_ACTIVE_META <- 40L # concurrent repo-metadata requests to pypi.org
CLICKHOUSE_URL <- "https://sql-clickhouse.clickhouse.com"
CLICKHOUSE_PAGE_SIZE <- 100000L # server-enforced max rows per query on the public `demo` user
OUT_DIR <- "repo-data-out"
dir.create(OUT_DIR, showWarnings = FALSE)

# ---- shared helpers ----------------------------------------------------


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
#' @noRd
clickhouse_query <- function(sql) {
    resp <- httr2::request(CLICKHOUSE_URL) |>
        httr2::req_url_query(user = "demo", default_format = "JSONCompact") |>
        httr2::req_body_raw(sql) |>
        httr2::req_retry(max_tries = 5, backoff = \(i) 2^i) |>
        httr2::req_perform()
    httr2::resp_body_json(resp, simplifyVector = TRUE)$data
}

#' Full PyPI download-count population (~870k packages, last complete
#' calendar month), paginated in chunks of CLICKHOUSE_PAGE_SIZE. Typically
#' ~9 requests, well under a minute, no rate limiting encountered.
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
        pages[[length(pages) + 1]] <- tibble::tibble(
            downloads = as.numeric(rows[, 1]),
            name = rows[, 2]
        )
        if (n < CLICKHOUSE_PAGE_SIZE) break
        offset <- offset + CLICKHOUSE_PAGE_SIZE
    }
    dplyr::bind_rows(pages)
}

#' Repo URLs for many PyPI packages at once (concurrent requests).
#' info.project_urls (free-text keys) + info.home_page.
pypi_repo_urls_many <- function(names_vec) {
    urls <- stringr::str_glue("https://pypi.org/pypi/{URLencode(names_vec)}/json")
    bodies <- perform_json_parallel(urls)
    purrr::map_chr(bodies, \(body) {
        if (is.null(body)) {
            return(NA_character_)
        }
        info <- body$info
        find_github_url(c(unlist(info$project_urls, use.names = FALSE), info$home_page))
    })
}

# ---- build table ----------------------------------------------------------

if (sys.nframe() == 0) {
    cli::cli_alert_info("PyPI: fetching full download-count population via ClickHouse (fast)...")
    downloads_tbl <- pypi_downloads_full()

    cli::cli_alert_info("PyPI: building working sample (head + random tail)...")
    head_tbl <- downloads_tbl |> dplyr::slice_max(downloads, n = TOP_N_HEAD)
    tail_pool <- downloads_tbl |> dplyr::anti_join(head_tbl, by = "name")
    tail_tbl <- tail_pool |> dplyr::slice_sample(n = min(WORKING_SAMPLE_TAIL_SIZE, nrow(tail_pool)))
    working_sample <- dplyr::bind_rows(head_tbl, tail_tbl)

    cli::cli_alert_info("PyPI: resolving GitHub repo URLs for {nrow(working_sample)} packages (parallel, max_active={MAX_ACTIVE_META})...")
    pypi_tbl <- working_sample |>
        dplyr::mutate(repo_url = pypi_repo_urls_many(name)) |>
        dplyr::filter(!is.na(repo_url)) |>
        dplyr::select(name, downloads, repo_url)
    readr::write_csv(pypi_tbl, file.path(OUT_DIR, "pypi.csv"))
    cli::cli_alert_success("PyPI: wrote {nrow(pypi_tbl)} rows to {file.path(OUT_DIR, 'pypi.csv')}")
}
