#' Extract repo data for an r-universe
#'
#' @description Build a (package, title, package_url, repo_url) table for every
#' package currently published in a given r-universe.
#'
#' @param universe Which r-universe to query: `"ropensci"` (default, the
#' rOpenSci r-universe) or `"cran"` (the CRAN mirror r-universe). Only the
#' rOpenSci r-universe carries software-review metadata, so `reviewed`/
#' `review_id` are only present in the result when `universe == "ropensci"`.
#' Also, `RemoteUrl` is only a meaningful repo-URL candidate for packages
#' actually hosted on an r-universe (as `ropensci` packages are); for
#' `"cran"`, where it instead reflects CRAN's own build infrastructure, it
#' is excluded from the URL search.
#' @return A tibble with one row per r-universe package: `package`,
#' `repo_url`, `downloads` (`_downloads$count`, last-month CRAN downloads),
#' and `stars` (`_stars`, GitHub stargazer count); plus, for
#' `universe == "ropensci"` only, a `reviewed` flag
#' (`_metadata$review$status == "reviewed"`) and, for those, the
#' `review_id` of its rOpenSci software review (`NA` otherwise).
#' @export
build_runiv_table <- function (universe = c ("ropensci", "cran")) {
    universe <- match.arg (universe)
    if (universe == "cran") {
        cli::cli_abort (
            "'cran' does not currently work, because the r-universe \
            API fails to return full CRAN dump. Use 'build_cran_table()' \
            instead."
        )
    }

    host <- stringr::str_glue ("{universe}.r-universe.dev")

    message ("Fetching ", universe, " r-universe package dump...")
    resp <- httr2::request (stringr::str_glue ("https://{host}/api/packages")) |>
        httr2::req_url_query (limit = 100000L) |>
        httr2::req_retry (max_tries = 5, backoff = \ (i) 2^i) |>
        httr2::req_perform ()
    pkgs <- httr2::resp_body_json (resp, simplifyVector = FALSE)

    message ("Extracting repo URLs from ", length (pkgs), " packages...")
    tbl <- purrr::map_dfr (pkgs, \ (p) {
        url_candidates <- c (p$URL, p$BugReports)
        if (universe == "ropensci") {
            url_candidates <- c (url_candidates, p$RemoteUrl)
        }

        out <- tibble::tibble (
            package = p$Package,
            repo_url = find_github_url (url_candidates),
            downloads = p$`_downloads`$count,
            stars = p$`_stars`
        )
        if (universe == "ropensci") {
            review <- p$`_metadata`$review
            reviewed <- isTRUE (review$status == "reviewed")
            out$reviewed <- reviewed
            out$review_id <- if (reviewed) review$id else NA_integer_
        }
        out
    })

    n_missing <- sum (is.na (tbl$repo_url))
    if (n_missing > 0) {
        message (stringr::str_glue (
            "Warning: {n_missing} of {nrow(tbl)} packages had no repo URL extracted."
        ))
    }
    tbl
}

#' Alternative way to get CRAN data, until r-universe full CRAN dump works.
#'
#' @export
build_cran_table <- function () {

    cran_data_pkgstats () |>
        cran_data_downloads ()
}

#' Get GitHub URLs for all CRAN packages which have them
#' @noRd
cran_data_pkgstats <- function () {

    u <- paste0 (
        "https://github.com/ropensci-review-tools/pkgstats/",
        "releases/download/v0.1.6/pkgstats-CRAN-current.Rds"
    )
    f <- fs::path (fs::path_temp (), basename (u))
    if (!file.exists (f)) {
        download.file (u, f)
    }

    x <- readRDS (f) |>
        dplyr::group_by (package) |>
        dplyr::slice_max (date, n = 1, with_ties = FALSE)

    # Reduce to packages with GitHub URLs only:
    index <- which (vapply (
        x$urls,
        function (i) grepl ("github.com", i, fixed = TRUE),
        logical (1L)
    ))
    x <- x [index, ]
    gh_urls <- vapply (
        x$urls,
        function (i) {
            j <- strsplit (i, ",(\\n|\\s)") [[1]]
            grepv ("github.com", j, value = TRUE) [1]
        },
        character (1L),
        USE.NAMES = FALSE
    )
    # Then reduce again only to resolvable ones:
    lens <- vapply (fs::path_split (gh_urls), length, integer (1L))
    index <- which (lens == 4L)
    x <- x [index, ]
    gh_urls <- gh_urls [index]

    tibble::tibble (package = x$package, version = x$version, repo_url = gh_urls)
}

#' Get download data for all CRAN packages
#'
#' @param dat Result of 'cran_data_pkgstats()' call.
#' @return Modified version of input 'dat' with additional "downloads" column.
#' Downloads are for month prior.
#'
#' @noRd
cran_data_downloads <- function (dat) {

    # Then get total downloads per package over last month.
    pkgs <- dat$package
    chunk_size <- 100
    chunks <- unname (split (pkgs, ceiling (seq_along (pkgs) / chunk_size)))

    urls <- vapply (
        chunks,
        function (p) {
            paste0 (
                "https://cranlogs.r-pkg.org/downloads/total/last-month/",
                paste (p, collapse = ",")
            )
        },
        character (1L)
    )

    reqs <- lapply (urls, httr2::request)
    resps <- httr2::req_perform_parallel (reqs, on_error = "continue")

    dl <- do.call (rbind, lapply (resps, function (r) {
        if (!inherits (r, "httr2_response") || httr2::resp_is_error (r)) {
            return (NULL)
        }
        j <- httr2::resp_body_json (r)
        data.frame (
            package = vapply (j, function (i) i$package, character (1L)),
            downloads = vapply (j, function (i) i$downloads, integer (1L))
        )
    }))
    dplyr::left_join (dat, dl, by = "package")
}
