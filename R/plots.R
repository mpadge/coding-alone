# Plotting functions used by the "review-dividend" vignette, matched
# one-to-one with the table-generation functions in R/tables.R: each of
# these plots the tibble produced by one of that file's functions. Kept
# separate from R/analyse-activity.R's `plot_activity()`/
# `plot_activity_by_source()`, which plot `issue_rate_tbl()` output
# directly rather than one of these higher-level comparison tables.

# ---- cross-source fold-change -----------------------------------------------

#' Plot fold-change in non-core rate across sources and strata
#'
#' Bar chart of `fold_change_tbl()` output: one column per source, one bar
#' per popularity stratum, `fold_change` on a log y-axis (so a halving and a
#' doubling are visually symmetric) with a reference line at 1 (no change).
#' If `tbl` carries a `metric` column (e.g. issues vs. comments, row-bound
#' from two `fold_change_tbl()` calls), an extra facet row splits on it.
#'
#' @param tbl As returned by `fold_change_tbl()`, optionally with an added
#' `metric` column.
#' @param source_display Named character vector mapping internal source
#' names to display labels, e.g. `SOURCE_DISPLAY_NAME`.
#' @param ref_date The same `ref_date` passed to `fold_change_tbl()`, used
#' only to label the plot.
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' fc <- fold_change_tbl (issue_authors_tbl, repo_tbl, c ("cran", "npm"))
#' plot_fold_change (fc, SOURCE_DISPLAY_NAME)
#' }
#' @export
plot_fold_change <- function (tbl, source_display = NULL,
                              ref_date = as.Date ("2021-01-01")) {

    popularity_stratum <- fold_change <- source <- NULL

    if (!is.null (source_display)) {
        lab <- unname (source_display [tbl$source])
        tbl$source <- ifelse (is.na (lab), tbl$source, lab)
    }

    ref_lab <- format (ref_date, "%b %Y")

    p <- ggplot2::ggplot (
        tbl,
        ggplot2::aes (popularity_stratum, fold_change, fill = popularity_stratum)
    ) +
        ggplot2::geom_col () +
        ggplot2::geom_hline (yintercept = 1, linetype = 2, colour = "grey40") +
        ggplot2::scale_y_log10 (
            breaks = c (0.1, 0.25, 0.5, 1, 2),
            labels = scales::percent (c (0.1, 0.25, 0.5, 1, 2))
        ) +
        ggplot2::scale_fill_brewer (palette = "RdYlBu", direction = -1) +
        ggplot2::labs (
            x = "Popularity stratum (Q1 = least popular)",
            y = stringr::str_glue ("Rate now, as % of rate in {ref_lab}"),
            fill = "Stratum"
        ) +
        ggplot2::theme_minimal () +
        ggplot2::theme (legend.position = "none")

    if ("metric" %in% names (tbl)) {
        p <- p + ggplot2::facet_grid (metric ~ source)
    } else {
        p <- p + ggplot2::facet_wrap (~source, nrow = 1)
    }

    p
}

# ---- matched-cohort age-vs-calendar-time analysis --------------------------

#' Plot non-core rate by creation-year cohort and fixed repo age
#'
#' Plot `cohort_age_rate_tbl()` output: rate (log10 y-axis) against
#' creation-year cohort, one line per fixed age, faceted by source with a
#' free y-scale per facet (sources sit on very different absolute rate
#' scales, and unlike the main cross-source plots this one isn't trying to
#' compare sources against each other - only cohorts within a source).
#'
#' @param tbl As returned by `cohort_age_rate_tbl()`.
#' @param source_display Named character vector as in `plot_fold_change()`.
#' @param min_repo_months Cells with less exposure than this are dropped
#' before plotting, since a handful of repo-months makes for an unstable
#' rate estimate.
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' cohort_tbl <- cohort_age_rate_tbl (issue_authors_tbl, repo_tbl, "cran")
#' plot_cohort_age (cohort_tbl, SOURCE_DISPLAY_NAME)
#' }
#' @export
plot_cohort_age <- function (tbl, source_display = NULL, min_repo_months = 30) {

    cohort_year <- rate <- age_year <- age_label <- source <-
        n_repo_months <- NULL

    tbl <- dplyr::filter (tbl, n_repo_months >= min_repo_months, !is.na (rate), rate > 0)

    if (!is.null (source_display)) {
        lab <- unname (source_display [tbl$source])
        tbl$source <- ifelse (is.na (lab), tbl$source, lab)
    }
    tbl$age_label <- factor (
        paste ("Year", tbl$age_year + 1, "of repo life"),
        levels = paste ("Year", sort (unique (tbl$age_year)) + 1, "of repo life")
    )

    ggplot2::ggplot (
        tbl,
        ggplot2::aes (cohort_year, rate, colour = age_label)
    ) +
        ggplot2::geom_line (linewidth = 0.9) +
        ggplot2::geom_point (size = 1.6) +
        ggplot2::facet_wrap (~source, scales = "free_y") +
        ggplot2::scale_y_log10 () +
        ggplot2::scale_colour_brewer (palette = "Dark2") +
        ggplot2::labs (
            x = "Repo creation-year cohort",
            y = "Non-core issues per repo-month, during that fixed age (log scale)",
            colour = NULL
        ) +
        ggplot2::theme_minimal () +
        ggplot2::theme (legend.position = "top")
}

# ---- rOpenSci: reviewed vs. non-reviewed packages ---------------------------

#' Plot rOpenSci non-core rate over time, by review status
#'
#' Time-trend line plot of `ropensci_reviewed_activity_tbl()` output: rate
#' over calendar time, one line for reviewed and one for non-reviewed
#' repos, faceted by popularity stratum (columns) and, if `tbl` carries a
#' `metric` column (issues vs. comments, row-bound from two calls), by
#' metric (rows) as well.
#'
#' @param tbl As returned by `ropensci_reviewed_activity_tbl()`, optionally
#' row-bound across metrics with an added `metric` column.
#' @param start_year Optional year to crop the plotted window to.
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' ros <- ropensci_reviewed_activity_tbl (issue_authors_tbl, repo_tbl, ropensci_raw)
#' plot_ropensci_reviewed_trend (ros, start_year = 2016)
#' }
#' @export
plot_ropensci_reviewed_trend <- function (tbl, start_year = NULL) {

    month <- rate <- metric <- popularity_stratum <- reviewed <-
        reviewed_label <- NULL

    if (!is.null (start_year)) {
        start_date <- as.Date (stringr::str_glue ("{start_year}-01-01"))
        tbl <- dplyr::filter (tbl, month >= start_date)
    }

    tbl$popularity_stratum <- label_stratum_extremes (tbl$popularity_stratum)
    tbl$reviewed_label <- ifelse (tbl$reviewed, "Formally reviewed", "Not reviewed")

    p <- ggplot2::ggplot (
        dplyr::filter (tbl, !is.na (rate)),
        ggplot2::aes (month, rate, colour = reviewed_label)
    ) +
        ggplot2::geom_line (linewidth = 0.8, alpha = 0.9) +
        ggplot2::scale_colour_manual (
            values = c (
                "Formally reviewed" = "#c0392b",
                "Not reviewed" = "#7f8c8d"
            )
        ) +
        ggplot2::labs (
            x = NULL,
            y = "Non-core rate per repo-month (12-month trailing avg)",
            colour = NULL
        ) +
        ggplot2::theme_minimal () +
        ggplot2::theme (legend.position = "top")

    # `facet_grid(scales = "free_y")` only frees the y scale per row, not
    # per panel - with strata spanning orders of magnitude, that leaves Q1
    # squashed flat against Q4's scale in the same row. `facet_wrap()` on
    # both variables gives every panel its own scale instead, at the cost
    # of losing facet_grid's shared row/column strip labels.
    if ("metric" %in% names (tbl)) {
        p <- p + ggplot2::facet_wrap (
            ggplot2::vars (metric, popularity_stratum),
            scales = "free_y", nrow = 2
        )
    } else {
        p <- p + ggplot2::facet_wrap (~popularity_stratum, scales = "free_y")
    }

    p
}

#' Plot fold-change in rOpenSci non-core rate, by review status
#'
#' Bar chart of `ropensci_reviewed_fold_change_tbl()` output: one bar for
#' reviewed, one for non-reviewed, faceted by popularity stratum. If `tbl`
#' carries a `metric` column (e.g. issues vs. comments, row-bound from two
#' `ropensci_reviewed_fold_change_tbl()` calls), it is filtered down to the
#' single `metric` requested before plotting - one call produces one
#' single-metric plot, so issues and comments are two separate figures
#' rather than two facet rows of the same one.
#'
#' @param tbl As returned by `ropensci_reviewed_fold_change_tbl()`,
#' optionally with an added `metric` column.
#' @param metric Which metric to plot: `"issues"` (default) or
#' `"comments"`. Only used to filter `tbl` down to one metric when it
#' carries a `metric` column (matched case-insensitively against that
#' column's values, e.g. `"Issues"`/`"Comments"`); otherwise `tbl` is
#' assumed to already be single-metric, and this only sets the plot title.
#' @param ref_date As in `plot_fold_change()`, used only to label the plot.
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' ros <- ropensci_reviewed_activity_tbl (issue_authors_tbl, repo_tbl, ropensci_raw)
#' ros_fc <- ropensci_reviewed_fold_change_tbl (ros)
#' plot_ropensci_reviewed_fold_change (ros_fc, metric = "issues")
#' }
#' @export
plot_ropensci_reviewed_fold_change <- function (tbl, metric = c ("issues", "comments"),
                                                ref_date = as.Date ("2021-01-01")) {

    metric <- match.arg (metric)

    popularity_stratum <- fold_change <- reviewed <- reviewed_label <-
        rate_latest <- rate_ref <- NULL

    if ("metric" %in% names (tbl)) {
        tbl <- tbl [tolower (as.character (tbl$metric)) == metric, ]
    }

    tbl$reviewed_label <- ifelse (tbl$reviewed, "Formally\nreviewed", "Not\nreviewed")
    ref_lab <- format (ref_date, "%b %Y")

    ggplot2::ggplot (
        tbl,
        ggplot2::aes (reviewed_label, fold_change, fill = reviewed_label)
    ) +
        ggplot2::geom_col () +
        ggplot2::geom_hline (yintercept = 1, linetype = 2, colour = "grey40") +
        ggplot2::scale_y_log10 (
            breaks = c (0.25, 0.5, 1, 2, 4),
            labels = scales::percent (c (0.25, 0.5, 1, 2, 4))
        ) +
        ggplot2::scale_fill_manual (
            values = c (
                "Formally\nreviewed" = "#c0392b",
                "Not\nreviewed" = "#95a5a6"
            )
        ) +
        ggplot2::labs (
            x = NULL,
            y = stringr::str_glue ("Rate now, as % of rate in {ref_lab}"),
            fill = NULL,
            title = stringr::str_to_title (metric)
        ) +
        ggplot2::theme_minimal () +
        ggplot2::theme (legend.position = "none") +
        ggplot2::facet_wrap (~popularity_stratum, nrow = 1)
}
