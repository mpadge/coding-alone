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

    popularity_authors_tbl <-
        build_popularity_authors_tbl (issue_authors_tbl, repo_tbl)

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
        pre_process_author_densities (issue_authors_tbl, repo_tbl, thr)
    })
    sc <- lapply (thresholds, function (thr) {
        cli::cli_alert_info ("Author step changes for ctb threshold = {thr}")
        dplyr::bind_rows (
            author_density_step_change_tbl (
                issue_authors_tbl,
                repo_tbl,
                primary_sources,
                n_strata = 4L,
                contrib_threshold = thr
            ),
            author_density_step_change_tbl (
                issue_authors_tbl,
                repo_tbl,
                primary_sources,
                n_strata = 1L, # All strata together
                contrib_threshold = thr
            )
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
        popularity_authors_tbl = popularity_authors_tbl,
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
        dplyr::bind_rows (
            author_density_tbl (
                issue_authors,
                repos,
                source_name = src,
                n_strata = 4L,
                contrib_threshold = contrib_threshold
            ) |>
                dplyr::mutate (src = src, contrib_threshold = contrib_threshold),
            author_density_tbl (
                issue_authors,
                repos,
                source_name = src,
                n_strata = 1L,
                contrib_threshold = contrib_threshold
            ) |>
                dplyr::mutate (src = src, contrib_threshold = contrib_threshold)
        )
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

build_popularity_authors_tbl <- function (issue_authors_tbl, repo_tbl) {

    repo_created <- issue_authors_tbl |>
        dplyr::filter (!is.na (repo_created_at)) |>
        dplyr::distinct (repo_url, repo_created_at) |>
        dplyr::mutate (repo_created_at = as.Date (repo_created_at))

    n_authors_tbl <- issue_authors_tbl |>
        dplyr::distinct (repo_url, author) |>
        dplyr::count (repo_url, name = "n_authors")

    popularity_authors_tbl <- purrr::map_dfr (names (POPULARITY_METRIC), \ (src) {

        metric_col <- unname (POPULARITY_METRIC [src])

        repo_tbl |>
            dplyr::filter (source == src) |>
            dplyr::distinct (repo_url, .keep_all = TRUE) |>
            dplyr::mutate (popularity = .data [[metric_col]], source = src) |>
            dplyr::inner_join (repo_created, by = "repo_url") |>
            dplyr::inner_join (n_authors_tbl, by = "repo_url") |>
            dplyr::mutate (
                lifespan_years = as.numeric (
                    difftime (Sys.Date (), repo_created_at, units = "days")
                ) / 365.25
            ) |>
            dplyr::filter (!is.na (popularity), popularity > 0, lifespan_years > 0)
    })

    # One log-log lm per source: `log10 (popularity) ~ log10 (lifespan_years) +
    # log10 (n_authors)`. `popularity_adj` re-expresses each repo's popularity
    # at the source's median lifespan, using the fitted lifespan coefficient to
    # shift it off its own actual lifespan.
    popularity_authors_tbl |>
        dplyr::group_by (source) |>
        dplyr::group_modify (\ (tbl, ...) {

            fit <- stats::lm (
                log10 (popularity) ~ log10 (lifespan_years) + log10 (n_authors),
                data = tbl
            )
            lifespan_coef <- unname (stats::coef (fit) ["log10(lifespan_years)"])
            median_log_lifespan <- log10 (stats::median (tbl$lifespan_years))

            tbl$popularity_adj <- 10^(
                log10 (tbl$popularity) -
                    lifespan_coef * (log10 (tbl$lifespan_years) - median_log_lifespan)
            )
            tbl
        }) |>
        dplyr::ungroup ()
}
