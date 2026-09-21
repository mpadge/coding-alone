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
    cli::cli_alert_success ("Commit rates")
    repo_creation_rates <- purrr::map_dfr (primary_sources, \ (src) {
        repo_creation_tbl (issue_authors_tbl, repo_tbl, src) |>
            dplyr::mutate (src = src)
    })
    cli::cli_alert_success ("Repo creation rates")

    thresholds <- c (0.01, 1)
    ad <- lapply (thresholds, function (thr) {
        cli::cli_alert_info ("Author densities for ctb threshold = {thr}")
        pre_process_author_densities (issue_authors_tbl, repo_tbl, 0.01)
    })
    sc <- lapply (thresholds, function (thr) {
        cli::cli_alert_info ("Author step changes for ctb threshold = {thr}")
        author_density_step_change_tbl (
            issue_authors_tbl, repo_tbl, primary_sources,
            contrib_threshold = thr
        )
    })
    cmts <- num_comments_step_change_tbl (
        issue_authors_tbl, repo_tbl, primary_sources
    )
    cli::cli_alert_success ("Issue comment rates")


    res <- list (
        repo_tbl = repo_tbl,
        issue_authors_tbl = issue_authors_tbl,
        commit_counts_tbl = commit_counts_tbl,
        commit_rates = commit_rates,
        repo_creation_rates = repo_creation_rates,
        author_densities_ctb001 = ad [[1]],
        author_densities_ctb100 = ad [[2]],
        author_dens_step_change001 = sc [[1]],
        author_dens_step_change100 = sc [[2]],
        issue_comments_step_change = cmts
    )

    f <- fs::path (out_dir, paste0 (f_name, ".Rds"))
    saveRDS (res, f)
    return (f)
}

pre_process_author_densities <- function (issue_authors, repos, contrib_threshold = 0.01) {

    # Suppress no visible binding notes:
    issue_authors_tbl <- repo_tbl <- NULL

    primary_sources <- unique (repos$source)

    purrr::map_dfr (primary_sources, \ (src) {
        author_density_tbl (
            issue_authors,
            repos,
            source_name = src,
            contrib_threshold = contrib_threshold
        ) |>
            dplyr::mutate (src = src, contrib_threshold = contrib_threshold)
    })
}

# Estimate step-change endpoints from linear regression.
step_change_regression <- function (tbl,
                                    value_col,
                                    ref_date = as.Date ("2021-01-01")) {

    # Suppress no visible binding notes:
    month <- NULL

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
