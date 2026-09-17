#' Combine output tables into a single table of repository-specific data.
#'
#' Combines locally-stored tables of data for PyPI, npm, CRAN, JOSS, and
#' rOpenSci into a single table of names, GitHub URLs, and
#' download/star metrics.
#'
#' @param out_dir Directory holding the source CSVs.
#' @return A tibble with columns `name`, `repo_url`, `downloads`, `stars`,
#' `source`.
#'
#' @examples
#' \dontrun{
#' repo_tbl <- build_repo_tbl ("path/to/repo-data-out")
#' }
#' @export
build_repo_tbl <- function (out_dir) {

    source_patterns <- c (
        ropensci = "ropensci.csv",
        joss = "joss.csv",
        cran = "cran.csv",
        pypi = "pypi.csv",
        npm = "npm.csv"
    )

    source_for_file <- function (path) {
        fname <- fs::path_file (path)
        hit <- fname == source_patterns
        names (source_patterns) [hit]
    }

    read_one <- function (path) {

        tbl <- readr::read_csv (path, show_col_types = FALSE, progress = FALSE)
        name_col <- intersect (c ("name", "package", "title"), names (tbl)) [1]

        name <- tbl [[name_col]]
        if (name_col == "title") {
            name <- sub ("^\\[REVIEW\\]:\\s*", "", name)
        }

        tibble::tibble (
            name = name,
            repo_url = tbl$repo_url,
            downloads = if ("downloads" %in% names (tbl)) {
                tbl$downloads
            } else {
                NA_integer_
            },
            stars = if ("stars" %in% names (tbl)) tbl$stars else NA_integer_,
            source = source_for_file (path)
        )
    }

    flist <- fs::dir_ls (out_dir, glob = "*.csv")
    flist <- flist [lengths (lapply (flist, source_for_file)) > 0]

    purrr::map_dfr (flist, read_one)
}

#' Left-join `repo_tbl`'s per-repo metadata (name, downloads, stars, source)
#' onto an issue-authors table by `repo_url`.
#'
#' @inheritParams issue_rate_tbl
#' @return `issue_authors_tbl` with `repo_tbl`'s columns attached.
#'
#' @examples
#' issue_authors_tbl <- tibble::tibble (
#'     repo_url = c (
#'         "https://github.com/org/pkg1", "https://github.com/org/pkg2"
#'     ),
#'     issue_number = c (1L, 1L)
#' )
#' repo_tbl <- tibble::tibble (
#'     repo_url = c (
#'         "https://github.com/org/pkg1", "https://github.com/org/pkg2"
#'     ),
#'     name = c ("pkg1", "pkg2"),
#'     downloads = c (100, 200),
#'     stars = c (5, 10),
#'     source = c ("pypi", "npm")
#' )
#' join_repo_metadata (issue_authors_tbl, repo_tbl)
#' @export
join_repo_metadata <- function (issue_authors_tbl, repo_tbl) {

    # suppress no visible binding notes:
    repo_url <- NULL

    repo_tbl_unique <- dplyr::distinct (repo_tbl, repo_url, .keep_all = TRUE)
    dplyr::left_join (issue_authors_tbl, repo_tbl_unique, by = "repo_url")
}
