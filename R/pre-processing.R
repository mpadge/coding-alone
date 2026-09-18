# Functions to pre-process data for vignettes

#' Function to pre-process data for vignettes
#'
#' @param out_dir Directory containing all raw data
#' @param f_name Name of file in which pre-processed data are to be saved (in
#' `out_dir`).
#' @return Full path to saved file containing all pre-processed data.
#' @export
pre_process_coding_alone <- function (out_dir = NULL, f_name = "pre-processed") {

    stopifnot (dir.exists (out_dir))

    repo_tbl <- build_repo_tbl (out_dir)
    issue_authors_tbl <- readr::read_csv (
        file.path (out_dir, "issue-authors.csv"),
        show_col_types = FALSE,
        progress = FALSE
    )
    commit_counts_tbl <- readr::read_csv (
        file.path (out_dir, "commit-counts.csv"),
        show_col_types = FALSE,
        progress = FALSE
    )

    primary_sources <- unique (repo_tbl$source)

    commit_rates <- purrr::map_dfr (primary_sources, \ (src) {
        commit_rate_tbl (commit_counts_tbl, issue_authors_tbl, repo_tbl, src) |>
            dplyr::mutate (src = src)
    })
    repo_creation_rates <- purrr::map_dfr (primary_sources, \ (src) {
        repo_creation_tbl (issue_authors_tbl, repo_tbl, src) |>
            dplyr::mutate (src = src)
    })

    res <- list (
        repo_tbl = repo_tbl,
        issue_authors_tbl = issue_authors_tbl,
        commit_counts_tbl = commit_counts_tbl,
        commit_rates = commit_rates,
        repo_creation_rates = repo_creation_rates
    )

    f <- fs::path (out_dir, paste0 (f_name, ".Rds"))
    saveRDS (res, f)
    return (f)
}

# Estimate step-change endpoints from linear regression.
step_change_regression <- function (tbl,
                                    value_col,
                                    ref_date = as.Date ("2021-01-01")) {

    tbl <- dplyr::filter (tbl, month >= ref_date)
    latest_month <- max (tbl$month)
    fit <- stats::lm (
        value ~ month_num,
        data = data.frame (
            value = tbl [[value_col]],
            month_num = as.numeric (tbl$month)
        )
    )
    pred <- stats::predict (
        fit,
        newdata = data.frame (
            month_num = as.numeric (c (ref_date, latest_month))
        )
    )
    list (
        ref = unname (pred [1]),
        latest = unname (pred [2]),
        latest_month = latest_month
    )
}
