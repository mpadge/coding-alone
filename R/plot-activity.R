# Plots of the table-activity.R rate tables: one line per popularity
# stratum (plot_activity()) or one line per source (plot_activity_by_source()).

#' Range of fitted values across each group's loess smooth, fit with the
#' same formula/defaults as `geom_smooth()` (span 0.75, degree 2), so a
#' plot's y-axis can be sized relative to the smoothed curves rather than
#' the noisier raw points. Groups with too few non-NA points to fit a
#' loess (`geom_smooth()` would silently skip these too) are ignored.
#'
#' @param rate_tbl As returned by `issue_rate_tbl()` (or a row-bound
#' combination of several, as in `plot_activity_by_source()`).
#' @param group_col Column to fit one loess per group on - the line/colour
#' grouping of whichever plot this is sizing.
#' @return Length-2 numeric vector, `c(min, max)` of fitted values pooled
#' across all groups.
#' @noRd
loess_range <- function (rate_tbl, group_col = "popularity_stratum") {

    rate <- NULL # rm no visible binding note

    rate_tbl <- dplyr::filter (rate_tbl, !is.na (rate))

    fitted <- lapply (split (rate_tbl, rate_tbl [[group_col]]), \ (df) {
        if (nrow (df) < 5) {
            return (NULL)
        }
        fit <- tryCatch (
            stats::loess (rate ~ as.numeric (month), data = df),
            error = function (e) NULL
        )
        if (is.null (fit)) NULL else stats::predict (fit)
    })
    fitted <- unlist (fitted)

    c (min (fitted, na.rm = TRUE), max (fitted, na.rm = TRUE))
}

#' Default y-axis label for a given `issue_rate_tbl()` `metric`/`window`/
#' `contrib_threshold`. Both the numerator and the repo-months denominator
#' are `window`-month trailing sums (see `issue_rate_tbl()`), so the value
#' stays a per-repo-month rate rather than becoming a `window`-month total
#' - just a trailing average of that rate rather than one raw month's
#' value. The label says so explicitly, since a plain "per repo-month"
#' label reads as single-month data. `contrib_threshold` is reported as
#' the literal cutoff value rather than the more informal "non-contributor
#' authors", since what counts as "non-contributor" depends entirely on
#' that value.
#' @noRd
activity_metric_label <- function (metric = c ("issues", "comments"),
                                   window = 12L,
                                   contrib_threshold = 0.01) {

    metric <- match.arg (metric)
    verb <- if (metric == "issues") "Issues opened" else "Comments received"

    stringr::str_glue (
        "{verb} per repo-month ({window}-month trailing avg, ",
        "contrib-threshold={contrib_threshold})"
    )
}

#' Common y-axis + theme layers shared by `plot_activity()` and
#' `plot_activity_by_source()`: zoomed (not filtered - `coord_cartesian()`,
#' not `ylim()`/`scale_y_continuous()`, so the loess fits themselves aren't
#' distorted by dropping out-of-range points) so it's sized relative to the
#' smoothed curves rather than the raw points: capped at `1.25 *` the
#' fitted loess curves' own max, and floored at zero only if some fitted
#' loess value actually dips below it (a loess smooth over near-zero rates
#' can do this even though the raw rate never goes negative) - otherwise
#' left at ggplot2's own default lower limit.
#' @noRd
activity_plot_layers <- function (rate_tbl, group_col, y_lab = NULL) {

    rng <- loess_range (rate_tbl, group_col)
    lower <- if (rng [1] < 0) 0 else NA
    upper <- 1.25 * rng [2]

    list (
        ggplot2::geom_line (alpha = 0.9, lty = 2),
        ggplot2::geom_smooth (se = FALSE, method = "loess", formula = y ~ x),
        ggplot2::coord_cartesian (ylim = c (lower, upper)),
        ggplot2::labs (x = NULL, y = y_lab),
        ggplot2::theme_minimal ()
    )
}

#' Plot the trailing-window rate (`issue_rate_tbl()`'s `rate` column - see
#' its `metric` param for whether that's issues or comments per repo-month)
#' over time, one line per popularity stratum.
#'
#' @param rate_tbl As returned by `issue_rate_tbl()` - its `metric`,
#' `window`, `contrib_threshold`, and `source_name` attributes are read
#' straight off it, the first three to label the y-axis and `source_name`
#' to annotate the plot panel directly (top-right corner), rather than
#' needing to be passed in again.
#' @param start_year Optional year (e.g. `2018`) to start the plotted
#' window from; `NULL` (default) plots `rate_tbl`'s full window. Only
#' crops the display - `rate_tbl` isn't refetched, so this can't extend
#' the window beyond what `issue_rate_tbl()` was already called with.
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' rate_tbl <- issue_rate_tbl (issue_authors_tbl, repo_tbl, "pypi")
#' plot_activity (rate_tbl)
#' }
#' @export
plot_activity <- function (rate_tbl, src_name = NULL, start_year = NULL) {

    month <- rate <- popularity_stratum <- NULL # rm no visible binding notes

    if (!is.null (start_year)) {
        start_date <- as.Date (stringr::str_glue ("{start_year}-01-01"))
        rate_tbl <- dplyr::filter (rate_tbl, month >= start_date)
    }
    rate_tbl$popularity_stratum <-
        label_stratum_extremes (rate_tbl$popularity_stratum)

    p <- ggplot2::ggplot (
        rate_tbl,
        ggplot2::aes (month, rate, colour = popularity_stratum)
    ) +
        activity_plot_layers (rate_tbl, "popularity_stratum")
    ggplot2::labs (colour = "Popularity\nstratum") +
        ggplot2::guides (colour = ggplot2::guide_legend (reverse = TRUE))

    if (!is.null (src_name)) {

        p <- p + ggplot2::annotate (
            "text",
            x = structure (Inf, class = "Date"),
            y = Inf,
            label = src_name,
            hjust = 1.1,
            vjust = 1.5,
            fontface = "bold",
            size = 8
        )
    }

    p
}

#' Compare monthly issue rate across all four sources (`pypi`, `npm`,
#' `joss`, `ropensci`), for one popularity stratum. Note that "stratum" is
#' relative to each source's own distribution (see `issue_rate_tbl()`/
#' `popularity_strata()`) - e.g. pypi's Q4 download count and joss's Q4
#' star count aren't the same absolute popularity, just each source's own
#' top quarter. A source with no data yet for the requested window (e.g.
#' not fully fetched - see `analysis-plan.md`) just contributes no line,
#' rather than erroring.
#'
#' @inheritParams issue_rate_tbl
#' @param stratum Integer popularity stratum to compare (`1` = lowest
#' popularity, `n_strata` = highest), matching one of `issue_rate_tbl()`'s
#' `popularity_stratum` levels (`"Q<stratum>"`).
#' @param n_strata,contrib_threshold,metric,window Passed to each source's
#' `issue_rate_tbl()` call; `n_strata` must be the same one `stratum` is a
#' level of.
#' @param relative If `TRUE` (default), rescale each source by its own
#' mean before plotting - sources sit on very different absolute rate
#' scales (e.g. pypi's raw issue traffic dwarfs ropensci's), which would
#' otherwise squash the smaller sources' trends to flat lines near zero.
#' Puts every line at a comparable "around 1 = that source's own average"
#' scale, so trends are comparable even though absolute rates aren't. Set
#' `FALSE` to plot absolute rates instead.
#' @param start_year Optional year (e.g. `2018`) to start the analysis
#' from - passed straight through as each `issue_rate_tbl()` call's
#' `date_start`, so it also governs the repo-months/rate calculations
#' themselves, not just the plotted range. `NULL` (default) starts from
#' 2015-01-01. There is no equivalent end-date control - analyses always
#' run up to the current month.
#' @param ros_joss_mult Multiplier applied to the final (post-`relative`)
#' `rate` values for the `"ropensci"` and `"joss"` sources only, after
#' every other calculation. Default 10. This is a display-only scaling of
#' those two sources relative to `"pypi"`/`"npm"` - the plot's y-axis label
#' is annotated whenever it's not 1, so the scaling isn't silently hidden
#' from anyone reading the plot.
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' plot_activity_by_source (issue_authors_tbl, repo_tbl, stratum = 4L)
#' }
#' @export
plot_activity_by_source <- function (issue_authors_tbl, repo_tbl, stratum,
                                     n_strata = 4L,
                                     contrib_threshold = 0.01,
                                     metric = c ("issues", "comments"),
                                     window = 12L,
                                     relative = TRUE,
                                     start_year = NULL,
                                     ros_joss_mult = 20) {
    # rm no visible binding notes
    month <- rate <- source_name <- popularity_stratum <- NULL

    metric <- match.arg (metric)

    date_start <- if (is.null (start_year)) {
        as.Date ("2015-01-01")
    } else {
        as.Date (stringr::str_glue ("{start_year}-01-01"))
    }
    stratum_label <- paste0 ("Q", stratum)
    sources <- names (POPULARITY_METRIC)

    rate_tbl <- purrr::map_dfr (sources, \ (src) {
        issue_rate_tbl (
            issue_authors_tbl, repo_tbl, src,
            n_strata = n_strata, contrib_threshold = contrib_threshold,
            metric = metric, window = window,
            date_start = date_start
        ) |>
            dplyr::filter (popularity_stratum == stratum_label) |>
            dplyr::mutate (source_name = src)
    })
    rate_tbl$source_name <- factor (rate_tbl$source_name, levels = sources)

    attr (rate_tbl, "metric") <- metric
    attr (rate_tbl, "window") <- window
    attr (rate_tbl, "contrib_threshold") <- contrib_threshold

    y_lab <- activity_metric_label (metric, window, contrib_threshold)

    if (relative) {

        rate_tbl <- rate_tbl |>
            dplyr::group_by (source_name) |>
            dplyr::mutate (rate = rate / mean (rate, na.rm = TRUE)) |>
            dplyr::ungroup ()
        y_lab <- paste (y_lab, "- relative to each source's own mean")
    }

    if (ros_joss_mult != 1) {

        mult_these <- c ("ropensci", "joss", "cran")
        rate_tbl <- dplyr::mutate (
            rate_tbl,
            rate = dplyr::if_else (
                source_name %in% mult_these, rate * ros_joss_mult, rate
            )
        )
        y_lab <- paste (
            y_lab,
            stringr::str_glue ("- ropensci/joss/cran shown at {ros_joss_mult}x")
        )
    }

    ggplot2::ggplot (
        rate_tbl,
        ggplot2::aes (month, rate, colour = source_name)
    ) +
        # activity_plot_layers (rate_tbl, "source_name", y_lab) +
        activity_plot_layers (rate_tbl, "source_name") +
        ggplot2::labs (
            colour = "Source",
            title = stringr::str_glue (
                "Popularity stratum {stratum} of {n_strata}"
            )
        )
}
