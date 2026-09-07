# Functions to build a (name, downloads, repo_url) table for npm, filtered
# to packages with a resolvable GitHub repo URL. See README.Rmd for the
# script that drives these to actually build the table.

#' Full npm monthly download-count population (~3.77M packages), via the
#' `download-counts` npm package: https://www.npmjs.com/package/download-counts
#' — a single static JSON object, republished monthly, mapping package name
#' to last-month download count. This is npm's practical equivalent of
#' PyPI's ClickHouse/BigQuery dataset: there is no direct npm counterpart of
#' BigQuery's public PyPI download-log dataset (confirmed — npm's raw logs
#' are never exported anywhere public; only pre-aggregated counts are
#' exposed, via the REST API), so this precomputed community package is the
#' fast path instead of paginating api.npmjs.org's 128-per-request bulk
#' endpoint across all ~4.3M names (~34,000 sequential requests, the actual
#' cause of this script's slowness before, not any lack of a bulk endpoint
#' — that endpoint just isn't feasible to walk over the entire registry).
#' Verified: downloads (~28MB tarball) in ~1.5s, parses in R in ~20-30s.
#' NB: it's only as fresh as its monthly build job — fine for a relative-
#' popularity proxy, but check `version` (encodes the build date) if exact
#' recency matters.
npm_downloads_full <- function() {
    meta <- httr2::resp_body_json(
        httr2::req_perform(httr2::request("https://registry.npmjs.org/download-counts/latest")),
        simplifyVector = FALSE
    )
    cli::cli_alert_info("npm: using download-counts@{meta$version} (monthly snapshot, may be a few months old)")

    tmp_tgz <- tempfile(fileext = ".tgz")
    tmp_dir <- tempfile()
    dir.create(tmp_dir)
    httr2::req_perform(httr2::request(meta$dist$tarball), path = tmp_tgz)
    untar(tmp_tgz, exdir = tmp_dir)
    counts_json <- list.files(tmp_dir, pattern = "counts\\.json$", recursive = TRUE, full.names = TRUE)[1]
    counts <- jsonlite::fromJSON(counts_json)
    tibble::tibble(name = names(counts), downloads = as.numeric(unlist(counts, use.names = FALSE)))
}

#' Repo URL for many npm packages at once (concurrent requests). Uses the
#' full registry doc, not the abbreviated `install-v1+json` metadata, which
#' omits `repository` entirely.
npm_repo_urls_many <- function(names_vec) {
    registry_repo_urls_many(
        names_vec,
        url_fn = \(names_vec) stringr::str_glue("https://registry.npmjs.org/{URLencode(names_vec)}"),
        extract_candidates = \(body) {
            repo <- body$repository
            repo_url <- if (is.list(repo)) repo$url else repo
            c(repo_url, body$homepage)
        }
    )
}
