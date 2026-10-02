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
    ) |>
        canonicalise_repo_urls (repo_tbl)
    pr_authors_tbl <- readr::read_csv (
        file.path (out_dir, "pr-authors.csv"),
        show_col_types = FALSE,
        progress = FALSE
    ) |>
        canonicalise_repo_urls (repo_tbl)
    commit_counts_tbl <- readr::read_csv (
        file.path (out_dir, "commit-counts.csv"),
        show_col_types = FALSE,
        progress = FALSE
    ) |>
        canonicalise_repo_urls (repo_tbl)

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
    ad_issues <- lapply (thresholds, function (thr) {
        out <- pre_process_author_densities (issue_authors_tbl, repo_tbl, thr)
        cli::cli_alert_success ("Issue author densities for ctb threshold = {thr}")
        out
    })
    ad_prs <- lapply (thresholds, function (thr) {
        out <- pre_process_author_densities (pr_authors_tbl, repo_tbl, thr)
        cli::cli_alert_success ("PR author densities for ctb threshold = {thr}")
        out
    })
    sc_issues <- lapply (thresholds, function (thr) {
        out <- dplyr::bind_rows (
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
        cli::cli_alert_success ("Issue author step changes for ctb threshold = {thr}")
        out
    })
    sc_prs <- lapply (thresholds, function (thr) {
        out <- dplyr::bind_rows (
            author_density_step_change_tbl (
                pr_authors_tbl,
                repo_tbl,
                primary_sources,
                n_strata = 4L,
                contrib_threshold = thr
            ),
            author_density_step_change_tbl (
                pr_authors_tbl,
                repo_tbl,
                primary_sources,
                n_strata = 1L, # All strata together
                contrib_threshold = thr
            )
        )
        cli::cli_alert_success ("PR author step changes for ctb threshold = {thr}")
        out
    })
    cmts <- num_comments_step_change_tbl (
        issue_authors_tbl, repo_tbl, primary_sources
    )
    cli::cli_alert_success ("Issue comment rates")

    pr_to_issues <- pr_to_issues_ratio (repo_tbl, issue_authors_tbl, pr_authors_tbl)
    cli::cli_alert_success ("PR-to-issue ratio")

    res <- list (
        repo_tbl = repo_tbl,
        issue_authors_tbl = issue_authors_tbl,
        pr_authors_tbl = pr_authors_tbl,
        commit_counts_tbl = commit_counts_tbl,
        commit_rates = commit_rates,
        repo_creation_rates = repo_creation_rates,
        popularity_authors_tbl = popularity_authors_tbl,
        author_issue_densities_ctb001 = ad_issues [[1]],
        author_issue_densities_ctb100 = ad_issues [[2]],
        author_pr_densities_ctb001 = ad_prs [[1]],
        author_pr_densities_ctb100 = ad_prs [[2]],
        author_dens_issues_step_change001 = sc_issues [[1]],
        author_dens_issues_step_change100 = sc_issues [[2]],
        author_dens_prs_step_change001 = sc_prs [[1]],
        author_dens_prs_step_change100 = sc_prs [[2]],
        issue_comments_step_change = cmts,
        pr_to_issues = pr_to_issues
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

    repo_created_at <- repo_url <- author <- .data <-
        popularity <- lifespan_years <- NULL

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

# Count per repo per month, then sum over all repos for each month.
#
# Returns one series for each `repo_tbl$source`, plus aggregated values over all
# repos with `source = "all"`.
count_by_month <- function (x, id_col, count_name, repo_tbl) {

    # Suppress no visible binding notes:
    repo_url <- created_at <- month <- n <- source <- NULL

    per_repo <- x |>
        dplyr::distinct (repo_url, .data [[id_col]], .keep_all = TRUE) |>
        dplyr::mutate (month = as.Date (format (created_at, "%Y-%m-01"))) |>
        dplyr::group_by (repo_url, month) |>
        dplyr::summarise (n = dplyr::n (), .groups = "drop")

    repo_sources <- dplyr::distinct (repo_tbl, repo_url, source)

    by_source <- per_repo |>
        dplyr::inner_join (
            repo_sources,
            by = "repo_url",
            relationship = "many-to-many"
        ) |>
        dplyr::group_by (source, month) |>
        dplyr::summarise (!!count_name := sum (n), .groups = "drop")

    all_sources <- per_repo |>
        dplyr::group_by (month) |>
        dplyr::summarise (!!count_name := sum (n), .groups = "drop") |>
        dplyr::mutate (source = "all", .before = 1L)

    dplyr::bind_rows (by_source, all_sources)
}

pr_to_issues_ratio <- function (repo_tbl, issue_authors_tbl, pr_authors_tbl) {

    # Suppress no visible binding notes:
    source <- month <- num_issues <- num_prs <- NULL

    issue_counts <- count_by_month (
        issue_authors_tbl, "issue_number", "num_issues", repo_tbl
    )
    pr_counts <- count_by_month (
        pr_authors_tbl, "pr_number", "num_prs", repo_tbl
    )

    dplyr::full_join (issue_counts, pr_counts, by = c ("source", "month")) |>
        dplyr::mutate (dplyr::across (
            c (num_issues, num_prs),
            \(n) tidyr::replace_na (n, 0L)
        )) |>
        dplyr::arrange (source, month) |>
        dplyr::mutate (ratio = num_prs / num_issues)
}
