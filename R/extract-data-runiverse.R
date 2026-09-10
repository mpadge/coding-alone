runiv_packages_limit <- function () {
    if (identical (Sys.getenv ("LONGTAIL_TESTS"), "true")) 5L else 100000L
}

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
        httr2::req_url_query (limit = runiv_packages_limit ()) |>
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
cran_data_full <- function () {

    requireNamespace ("mongolite", quietly = TRUE)
    u <- "https://cran.r-universe.dev/api/dbdump"
    cran <- mongolite::read_bson (u)

    package <- vapply (cran, function (p) p$Package, character (1L))
    version <- vapply (cran, function (p) p$Version, character (1L))
    downloads <- vapply (cran, function (p) {
        ifelse (
            length (p$`_downloads`$count) == 0L,
            0L,
            p$`_downloads`$count
        )
    }, integer (1L))
    url <- vapply (cran, function (p) {
        ifelse (
            length (p$`_devurl`) == 0L,
            NA_character_,
            p$`_devurl`
        )
    }, character (1L))

    dat <- tibble::tibble (
        package = package,
        version = version,
        repo_url = url,
        downloads = downloads
    )

    # Reduce to packages with GitHub URLs only:
    index <- which (vapply (
        dat$repo_url,
        function (i) grepl ("github.com", i, fixed = TRUE),
        logical (1L)
    ))
    dat <- dat [index, ]

    orgs_to_rm <- c ("r-forge")
    orgs <- vapply (
        dat$repo_url,
        function (u) fs::path_split (u) [[1]] [3],
        character (1L)
    )
    dat [which (!orgs %in% orgs_to_rm), ]
}
