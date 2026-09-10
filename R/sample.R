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
#'
#' @examples
#' downloads_tbl <- tibble::tibble (
#'     name = paste0 ("pkg", seq_len (100)),
#'     downloads = round (100 * exp (-seq_len (100) / 15))
#' )
#' sample_tbl <- build_working_sample (
#'     downloads_tbl,
#'     top_n_head = 10L,
#'     tail_size = 20L
#' )
#' nrow (sample_tbl)
#' @export
build_working_sample <- function (downloads_tbl = NULL,
                                  top_n_head = 15000L,
                                  tail_size = 40000L,
                                  label = NULL) {

    # suppress no visible binding notes
    downloads <- NULL

    if (!is.null (label)) {
        msg <- stringr::str_glue (
            "{label}: building working sample (head + random tail)..."
        )
        cli::cli_alert_info (msg)
    }

    head_tbl <- downloads_tbl |> dplyr::slice_max (downloads, n = top_n_head)
    tail_pool <- downloads_tbl |> dplyr::anti_join (head_tbl, by = "name")
    tail_tbl <- tail_pool |>
        dplyr::slice_sample (n = min (tail_size, nrow (tail_pool)))

    dplyr::bind_rows (head_tbl, tail_tbl)
}

#' Resolve GitHub repo URLs for a working sample, and filter down to the
#' packages for which one was found.
#'
#' @description Applies `repo_urls_fn` to the `name` column of
#' `working_sample` (e.g. `pypi_repo_urls_many()` or `npm_repo_urls_many()`),
#' then drops rows for which no GitHub repo URL could be resolved. Shared
#' post-processing step for both the PyPI and npm working samples.
#'
#' @param working_sample A tibble with at least `name` and `downloads`
#' columns, as returned by `build_working_sample()`.
#' @param repo_urls_fn A function taking a character vector of package names
#' and returning a character vector of the same length, with a resolved
#' GitHub repo URL or `NA` for each.
#'
#' @return A tibble with `name`, `downloads`, and `repo_url` columns,
#' filtered to rows with a non-missing `repo_url`.
#'
#' @examples
#' working_sample <- tibble::tibble (
#'     name = c ("pkg1", "pkg2", "pkg3"),
#'     downloads = c (300, 200, 100)
#' )
#' fake_repo_urls_fn <- function (names_vec) {
#'     ifelse (
#'         names_vec == "pkg2",
#'         NA_character_,
#'         paste0 ("https://github.com/org/", names_vec)
#'     )
#' }
#' resolve_repo_urls (working_sample, fake_repo_urls_fn)
#' @export
resolve_repo_urls <- function (working_sample = NULL, repo_urls_fn = NULL) {

    # suppress no visible binding notes
    downloads <- name <- repo_url <- NULL

    working_sample |>
        dplyr::mutate (repo_url = repo_urls_fn (name)) |>
        dplyr::filter (!is.na (repo_url)) |>
        dplyr::select (name, downloads, repo_url)
}
