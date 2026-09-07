# Build (name, downloads, repo_url) table for npm, filtered to packages with
# a resolvable GitHub repo URL.

library(httr2)
library(jsonlite)
library(purrr)
library(dplyr)
library(readr)
library(stringr)
library(tibble)

# ---- config ----------------------------------------------------------

WORKING_SAMPLE_TAIL_SIZE <- 40000L # random draw size, outside the known head
TOP_N_HEAD <- 15000L # deterministic head inclusion
MAX_ACTIVE_META <- 40L # concurrent repo-metadata requests to registry.npmjs.org
OUT_DIR <- "repo-data-out"
dir.create(OUT_DIR, showWarnings = FALSE)

# ---- shared helpers ----------------------------------------------------

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

#' Perform many GET-JSON requests concurrently. Returns a list the same
#' length/order as `urls`; NULL for any request that errored or came back
#' 4xx/5xx. Individual failures don't abort the batch (on_error = "continue")
#' — most failures here are legitimate 404s (deleted/renamed packages), not
#' the registry actually rate-limiting concurrent traffic: verified 800
#' concurrent registry.npmjs.org lookups at max_active=50 finished in ~7s
#' with only real 404s, zero 429s. This replaces the previous sequential
#' 8-req/s throttle, which was the actual bottleneck (~1.9 hours for a
#' 55k-package working sample) — not the download-count fetch below, which
#' is already fast.
perform_json_parallel <- function(urls, max_active = MAX_ACTIVE_META) {
    reqs <- map(urls, \(u) request(u) |> req_retry(max_tries = 3) |> req_error(is_error = \(resp) FALSE))
    resps <- req_perform_parallel(reqs, on_error = "continue", max_active = max_active)
    map(resps, \(resp) {
        if (inherits(resp, "error") || resp_status(resp) >= 400) {
            return(NULL)
        }
        tryCatch(resp_body_json(resp, simplifyVector = FALSE), error = \(e) NULL)
    })
}

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
    meta <- resp_body_json(req_perform(request("https://registry.npmjs.org/download-counts/latest")), simplifyVector = FALSE)
    cli::cli_alert_info("npm: using download-counts@{meta$version} (monthly snapshot, may be a few months old)")

    tmp_tgz <- tempfile(fileext = ".tgz")
    tmp_dir <- tempfile()
    dir.create(tmp_dir)
    req_perform(request(meta$dist$tarball), path = tmp_tgz)
    untar(tmp_tgz, exdir = tmp_dir)
    counts_json <- list.files(tmp_dir, pattern = "counts\\.json$", recursive = TRUE, full.names = TRUE)[1]
    counts <- fromJSON(counts_json)
    tibble(name = names(counts), downloads = as.numeric(unlist(counts, use.names = FALSE)))
}

#' Repo URL for many npm packages at once (concurrent requests). Uses the
#' full registry doc, not the abbreviated `install-v1+json` metadata, which
#' omits `repository` entirely.
npm_repo_urls_many <- function(names_vec) {
    urls <- str_glue("https://registry.npmjs.org/{URLencode(names_vec)}")
    bodies <- perform_json_parallel(urls)
    map_chr(bodies, \(body) {
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
    head_tbl <- downloads_tbl |> slice_max(downloads, n = TOP_N_HEAD)
    tail_pool <- downloads_tbl |> anti_join(head_tbl, by = "name")
    tail_tbl <- tail_pool |> slice_sample(n = min(WORKING_SAMPLE_TAIL_SIZE, nrow(tail_pool)))
    working_sample <- bind_rows(head_tbl, tail_tbl)

    cli::cli_alert_info("npm: resolving GitHub repo URLs for {nrow(working_sample)} packages (parallel, max_active={MAX_ACTIVE_META})...")
    npm_tbl <- working_sample |>
        mutate(repo_url = npm_repo_urls_many(name)) |>
        filter(!is.na(repo_url)) |>
        select(name, downloads, repo_url)
    write_csv(npm_tbl, file.path(OUT_DIR, "npm.csv"))
    cli::cli_alert_success("npm: wrote {nrow(npm_tbl)} rows to {file.path(OUT_DIR, 'npm.csv')}")
}
