#' Plot repo-creation rate and commit rate, across sources
#'
#' Two-panel figure: each source's percentage share of repos created (top,
#' from `repo_creation_tbl()`) and commits per repo-month (bottom, from
#' `commit_rate_tbl()`), both `window`-month trailing sums/averages, one
#' line per source. Unlike `plot_new_author_rate()`/`plot_author_interval()`,
#' sources aren't faceted apart and lines aren't split by popularity
#' stratum - all sources are overlaid on the same axes in each panel.
#'
#' The repo-creation panel plots each source's percentage share of that
#' month's total repos for each source .
#'
#' @param commit_counts_tbl As returned by `fetch_repo_commits()`.
#' @param issue_authors_tbl As returned by `fetch_issue_authors()`.
#' @param repo_tbl As returned by `build_repo_tbl()`.
#' @param source_display Named character vector as in `plot_fold_change()`.
#' @param window,date_start,date_end Passed to each source's
#' `commit_rate_tbl()`/`repo_creation_tbl()` call.
#' @param start_year Optional year to crop the plotted window to, as in
#' `plot_activity()` - display-only, doesn't affect the underlying
#' repo-months/rate calculations.
#' @return A `patchwork` object (two stacked ggplot panels).
#'
#' @examples
#' \dontrun{
#' plot_commit_rate (commit_counts_tbl, issue_authors_tbl, repo_tbl, SOURCE_DISPLAY_NAME)
#' }
#' @export
plot_commit_rate <- function (commit_counts_tbl,
                              issue_authors_tbl,
                              repo_tbl,
                              source_display = NULL,
                              window = 12L,
                              date_start = as.Date ("2015-01-01"),
                              date_end = NULL,
                              start_year = NULL) {

    month <- rate <- source <- n_created <- total_created <- pct_created <- NULL

    sources <- names (POPULARITY_METRIC)

    relabel_source <- function (tbl) {
        if (!is.null (source_display)) {
            lab <- unname (source_display [tbl$source])
            tbl$source <- ifelse (is.na (lab), tbl$source, lab)
        }
        tbl$source <- factor (tbl$source, levels = unique (tbl$source))
        tbl
    }

    commit_tbl <- purrr::map_dfr (sources, \ (src) {
        commit_rate_tbl (
            commit_counts_tbl, issue_authors_tbl, repo_tbl, src,
            window = window, date_start = date_start, date_end = date_end
        ) |>
            dplyr::mutate (source = src)
    })
    creation_tbl <- purrr::map_dfr (sources, \ (src) {
        repo_creation_tbl (
            issue_authors_tbl, repo_tbl, src,
            window = window, date_start = date_start, date_end = date_end
        ) |>
            dplyr::mutate (source = src)
    })

    if (!is.null (start_year)) {
        start_date <- as.Date (stringr::str_glue ("{start_year}-01-01"))
        commit_tbl <- dplyr::filter (commit_tbl, month >= start_date)
        creation_tbl <- dplyr::filter (creation_tbl, month >= start_date)
    }

    # Rescale each month's per-source counts to a percentage share of that
    # month's total across all sources.
    creation_tbl <- creation_tbl |>
        dplyr::group_by (source) |>
        dplyr::mutate (
            total_created = sum (n_created),
            pct_created = 100 * n_created / total_created
        ) |>
        dplyr::ungroup ()

    commit_tbl <- relabel_source (commit_tbl)
    creation_tbl <- relabel_source (creation_tbl)

    p_creation <- ggplot2::ggplot (
        creation_tbl,
        ggplot2::aes (month, pct_created, colour = source)
    ) +
        ggplot2::geom_line (linewidth = 0.8, alpha = 0.9) +
        ggplot2::scale_colour_brewer (palette = "Set2", drop = FALSE) +
        ggplot2::labs (
            x = NULL,
            y = "New repos created per month (% of total)",
            colour = "Source"
        ) +
        ggplot2::theme_minimal () +
        ggplot2::theme (legend.position = "top")

    p_commit <- ggplot2::ggplot (
        dplyr::filter (commit_tbl, !is.na (rate)),
        ggplot2::aes (month, rate, colour = source)
    ) +
        ggplot2::geom_line (linewidth = 0.8, alpha = 0.9) +
        ggplot2::scale_colour_brewer (palette = "Set2", drop = FALSE) +
        ggplot2::labs (
            x = NULL,
            y = "Commits per repo-month",
            colour = "Source"
        ) +
        ggplot2::theme_minimal () +
        ggplot2::theme (legend.position = "none")

    patchwork::wrap_plots (p_creation, p_commit, ncol = 1)
}
