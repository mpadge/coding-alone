# Functions to build a (name, downloads, repo_url) table for npm, filtered
# to packages with a resolvable GitHub repo URL. See README.Rmd for the
# script that drives these to actually build the table.

#' Fetch the `download-counts` npm package's latest metadata (version and
#' tarball URL).
#' @noRd
npm_download_counts_meta <- function () {

    httr2::request ("https://registry.npmjs.org/download-counts/latest") |>
        httr2::req_perform () |>
        httr2::resp_body_json (simplifyVector = FALSE)
}

#' Fetch the `download-counts` npm package's latest metadata and tarball
#' bytes.
#'
#' @return A list with `version` (the `download-counts` package version, for
#' the "using download-counts@..." status message) and `tarball` (the raw
#' bytes of its tarball).
#' @noRd
npm_download_counts_fetch <- function () {

    meta <- npm_download_counts_meta ()
    tarball <- httr2::request (meta$dist$tarball) |>
        httr2::req_perform () |>
        httr2::resp_body_raw ()

    list (version = meta$version, tarball = tarball)
}

#' Full npm monthly download-count population (~3.77M packages), via the
#' `download-counts` npm package: https://www.npmjs.com/package/download-counts
#' — a single static JSON object, republished monthly, mapping package name
#' to last-month download count. This is npm's practical equivalent of
#' PyPI's ClickHouse/BigQuery dataset: there is no direct npm counterpart of
#' BigQuery's public PyPI download-log dataset.
#'
#' @return A table of all npm packages.
#'
#' @examples
#' \dontrun{
#' npm_tbl <- npm_downloads_full ()
#' }
#' @export
npm_downloads_full <- function () {

    fetched <- npm_download_counts_fetch ()
    msg <- stringr::str_glue (
        "npm: using download-counts@{fetched$version} ",
        "(monthly snapshot, may be a few months old)"
    )
    cli::cli_alert_info (msg)

    tmp_tgz <- tempfile (fileext = ".tgz")
    tmp_dir <- tempfile ()
    dir.create (tmp_dir)
    writeBin (fetched$tarball, tmp_tgz)
    utils::untar (tmp_tgz, exdir = tmp_dir)
    counts_json <- list.files (
        tmp_dir,
        pattern = "counts\\.json$", recursive = TRUE, full.names = TRUE
    ) [1]
    counts <- jsonlite::fromJSON (counts_json)

    tibble::tibble (
        name = names (counts),
        downloads = as.numeric (unlist (counts, use.names = FALSE))
    )
}

#' Repo URL for many npm packages at once (concurrent requests). Uses the
#' `/latest` endpoint (the latest version's package.json-equivalent, which
#' still carries `repository` and `homepage`), not the full registry doc —
#' the full doc includes every published version ever, and for
#' heavily-versioned packages that's enormous.
#'
#' @param names_vec Character vector of npm package names.
#' @noRd
npm_repo_urls_many <- function (names_vec) {

    registry_repo_urls_many (
        names_vec,
        url_fn = \ (names_vec) {
            stringr::str_glue (
                "https://registry.npmjs.org/{URLencode(names_vec)}/latest"
            )
        },
        extract_candidates = \ (body) {
            repo <- body$repository
            repo_url <- if (is.list (repo)) repo$url else repo
            c (repo_url, body$homepage)
        }
    )
}
