#' Generate a working sample from a full population.
#'
#' @description Build a working sample from a full (name, downloads)
#' population, stratified by download popularity: a deterministic head of the
#' `top_n_head` most-downloaded packages, plus a random draw of up to
#' `tail_size` packages from the remaining long tail.
#'
#' @param downloads_tbl A tibble with at least `name` and `downloads` columns,
#' as returned by `pypi_downloads_full()` or `npm_downloads_full()`.
#' @param top_n_head Number of most-downloaded packages to include
#' deterministically.
#' @param tail_size Number of packages to randomly draw from the remaining long
#' tail (outside the head). Capped at the size of that tail.
#' @param label Optional string used to prefix a `cli` status message (e.g.
#' `"PyPI"` or `"npm"`); no message is printed if `NULL`.
#'
#' @return A tibble combining the head and tail samples, with the same columns
#' as `downloads_tbl`.
#' @export
build_working_sample <- function (downloads_tbl = NULL,
                                  top_n_head = 15000L,
                                  tail_size = 40000L,
                                  label = NULL) {

    if (!is.null (label)) {
        cli::cli_alert_info ("{label}: building working sample (head + random tail)...")
    }

    head_tbl <- downloads_tbl |> dplyr::slice_max (downloads, n = top_n_head)
    tail_pool <- downloads_tbl |> dplyr::anti_join (head_tbl, by = "name")
    tail_tbl <- tail_pool |> dplyr::slice_sample (n = min (tail_size, nrow (tail_pool)))
    dplyr::bind_rows (head_tbl, tail_tbl)
}
utils::globalVariables ("downloads")
