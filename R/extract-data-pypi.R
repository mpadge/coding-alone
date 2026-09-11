# Functions to build a (name, downloads, repo_url) table for PyPI, filtered
# to packages with a resolvable GitHub repo URL. See README.Rmd for the
# script that drives these to actually build the table.

CLICKHOUSE_URL <- "https://sql-clickhouse.clickhouse.com"
# server-enforced max rows per query on the public `demo` user
CLICKHOUSE_PAGE_SIZE <- 100000L

clickhouse_page_size <- function () {

    if (identical (Sys.getenv ("PEERREVIEW_TESTS"), "true")) {
        5L
    } else {
        CLICKHOUSE_PAGE_SIZE
    }
}

#' Run a read-only SQL query against ClickHouse's public playground (the
#' `demo` user), which mirrors the same PyPI downloads dataset BigQuery's
#' `bigquery-public-data.pypi.file_downloads` does, updated monthly. This
#' replaces pypistats.org, which rate-limits per-package polling far too
#' aggressively to poll tens of thousands of packages. hugovk's own
#' top-pypi-packages generator itself switched to querying this endpoint
#' (see https://github.com/hugovk/top-pypi-packages/blob/main/clickhouse.py).
#'
#' The `demo` user caps any single query at CLICKHOUSE_PAGE_SIZE rows, so
#' pypi_downloads_full() below pages through with LIMIT/OFFSET.
#' Returns the result as a character matrix (ClickHouse's JSONCompact
#' encodes all values as strings to avoid UInt64/Int64 precision loss, so
#' there's no point asking jsonlite for anything fancier). simplifyVector
#' matters a lot here — the naive per-row list parse is ~100x slower at
#' 100k+ rows.
#' @noRd
clickhouse_query <- function (sql) {

    resp <- httr2::request (CLICKHOUSE_URL) |>
        httr2::req_url_query (user = "demo", default_format = "JSONCompact") |>
        httr2::req_body_raw (sql) |>
        httr2::req_retry (max_tries = 5, backoff = \ (i) 2^i) |>
        httr2::req_perform ()

    httr2::resp_body_json (resp, simplifyVector = TRUE)$data
}

#' Full PyPI download-count population (~870k packages, last complete
#' calendar month), paginated in chunks of CLICKHOUSE_PAGE_SIZE. Typically
#' ~9 requests, well under a minute, no rate limiting encountered.
#'
#' @return A table of all PyPI packages.
#'
#' @examples
#' \dontrun{
#' pypi_tbl <- pypi_downloads_full ()
#' }
#' @export
pypi_downloads_full <- function () {

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

    page_size <- clickhouse_page_size ()
    single_page_only <- identical (Sys.getenv ("PEERREVIEW_TESTS"), "true")

    pages <- list ()
    offset <- 0L

    repeat {
        rows <- clickhouse_query (sprintf (base_sql, page_size, offset))
        # length(rows) == 0 for an empty result
        n <- if (is.matrix (rows)) nrow (rows) else length (rows)
        if (n == 0) break
        pages [[length (pages) + 1]] <- tibble::tibble (
            downloads = as.numeric (rows [, 1]),
            name = rows [, 2]
        )
        if (single_page_only || n < page_size) break
        offset <- offset + page_size
    }

    dplyr::bind_rows (pages)
}

#' Repo URLs for many PyPI packages at once (concurrent requests).
#' info.project_urls (free-text keys) + info.home_page.
#' @param names_vec Character vector of PyPI package names.
#' @noRd
pypi_repo_urls_many <- function (names_vec) {

    registry_repo_urls_many (
        names_vec,
        url_fn = \ (names_vec) {
            stringr::str_glue (
                "https://pypi.org/pypi/{URLencode(names_vec)}/json"
            )
        },
        extract_candidates = \ (body) {
            c (
                unlist (body$info$project_urls, use.names = FALSE),
                body$info$home_page
            )
        }
    )
}
