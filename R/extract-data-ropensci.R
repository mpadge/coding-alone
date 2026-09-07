#' Extract repo data for rOpenScis
#'
#' @description Build a (package, title, package_url, repo_url) table for every
#' package currently published in the rOpenSci r-universe.
#'
#' @return A tibble with one row per rOpenSci r-universe package, including a
#' `reviewed` flag (`_metadata$review$status == "reviewed"`) and, for those,
#' the `review_id` of its rOpenSci software review (`NA` otherwise); plus
#' `downloads` (`_downloads$count`, last-month CRAN downloads) and `stars`
#' (`_stars`, GitHub stargazer count).
#' @export
build_ropensci_table <- function () {
    message ("Fetching rOpenSci r-universe package dump...")
    resp <- httr2::request ("https://ropensci.r-universe.dev/api/packages") |>
        httr2::req_retry (max_tries = 5, backoff = \ (i) 2^i) |>
        httr2::req_perform ()
    pkgs <- httr2::resp_body_json (resp, simplifyVector = FALSE)

    message ("Extracting repo URLs from ", length (pkgs), " packages...")
    tbl <- purrr::map_dfr (pkgs, \ (p) {
        review <- p$`_metadata`$review
        reviewed <- isTRUE (review$status == "reviewed")
        tibble::tibble (
            package = p$Package,
            repo_url = find_github_url (c (p$RemoteUrl, p$URL, p$BugReports)),
            reviewed = reviewed,
            review_id = if (reviewed) review$id else NA_integer_,
            downloads = p$`_downloads`$count,
            stars = p$`_stars`
        )
    })

    n_missing <- sum (is.na (tbl$repo_url))
    if (n_missing > 0) {
        message (stringr::str_glue (
            "Warning: {n_missing} of {nrow(tbl)} packages had no repo URL extracted."
        ))
    }
    tbl
}
