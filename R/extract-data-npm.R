# Build (name, downloads, repo_url) table for npm, filtered to packages with
# a resolvable GitHub repo URL.

# ---- config ----------------------------------------------------------

WORKING_SAMPLE_TAIL_SIZE <- 40000L # random draw size, outside the known head
TOP_N_HEAD <- 15000L # deterministic head inclusion
MAX_ACTIVE_META <- 40L # concurrent repo-metadata requests to registry.npmjs.org
OUT_DIR <- "repo-data-out"
dir.create(OUT_DIR, showWarnings = FALSE)

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
    urls <- stringr::str_glue("https://registry.npmjs.org/{URLencode(names_vec)}")
    bodies <- perform_json_parallel(urls)
    purrr::map_chr(bodies, \(body) {
        if (is.null(body)) {
            return(NA_character_)
        }
        repo <- body$repository
        repo_url <- if (is.list(repo)) repo$url else repo
        find_github_url(c(repo_url, body$homepage))
    })
}

# ---- build table ----------------------------------------------------------

if (sys.nframe() == 0) {
    cli::cli_alert_info("npm: fetching full download-count population via download-counts package (fast)...")
    downloads_tbl <- npm_downloads_full()

    cli::cli_alert_info("npm: building working sample (head + random tail)...")
    head_tbl <- downloads_tbl |> dplyr::slice_max(downloads, n = TOP_N_HEAD)
    tail_pool <- downloads_tbl |> dplyr::anti_join(head_tbl, by = "name")
    tail_tbl <- tail_pool |> dplyr::slice_sample(n = min(WORKING_SAMPLE_TAIL_SIZE, nrow(tail_pool)))
    working_sample <- dplyr::bind_rows(head_tbl, tail_tbl)

    cli::cli_alert_info("npm: resolving GitHub repo URLs for {nrow(working_sample)} packages (parallel, max_active={MAX_ACTIVE_META})...")
    npm_tbl <- working_sample |>
        dplyr::mutate(repo_url = npm_repo_urls_many(name)) |>
        dplyr::filter(!is.na(repo_url)) |>
        dplyr::select(name, downloads, repo_url)
    readr::write_csv(npm_tbl, file.path(OUT_DIR, "npm.csv"))
    cli::cli_alert_success("npm: wrote {nrow(npm_tbl)} rows to {file.path(OUT_DIR, 'npm.csv')}")
}
