# For each source, bin non-contributor issues by month, stratify repos by
# popularity (downloads where it exists for the source, stars otherwise), and
# compute a rate normalized by true repo-months exposure (from each repo's
# GitHub creation date).

POPULARITY_METRIC <- c (
    pypi = "downloads",
    npm = "downloads",
    joss = "stars",
    ropensci = "stars",
    cran = "downloads"
)

# Proper-cased display forms of each source's internal (lowercase)
# `POPULARITY_METRIC`/`repo_tbl$source` key, for anywhere a source name is
# shown to a reader rather than matched against data (e.g. plot
# annotations). "npm" is genuinely lowercase as a name, not an
# abbreviation, so it's left as-is.
SOURCE_DISPLAY_NAME <- c (
    pypi = "PyPI",
    npm = "npm",
    joss = "JOSS",
    ropensci = "rOpenSci",
    cran = "CRAN"
)

#' First-of-month for a Date/date-like vector.
#' @noRd
floor_month <- function (x) as.Date (format (as.Date (x), "%Y-%m-01"))

#' Split a popularity metric into quantile strata on its log10 scale.
#' Falls back to fewer strata than requested if ties in the data collapse some
#' quantile breaks together.
#'
#' @param x Numeric vector of popularity metric values (`downloads` or
#' `stars`).
#' @param n_strata Number of quantile strata.
#' @return An ordered factor, one level per stratum, lowest-popularity first.
#' @noRd
popularity_strata <- function (x, n_strata = 4L) {
    breaks <- stats::quantile (
        log10 (x + 1),
        probs = seq (0, 1, length.out = n_strata + 1),
        na.rm = TRUE
    )
    breaks <- unique (breaks)
    breaks [1] <- -Inf
    breaks [length (breaks)] <- Inf
    labels <- paste0 ("Q", seq_len (length (breaks) - 1))
    cut (log10 (x + 1), breaks = breaks, labels = labels, ordered_result = TRUE)
}

#' Relabel a `popularity_strata()` factor's lowest and highest levels as
#' "(low)"/"(high)" for legend display (e.g. "Q1" -> "Q1 (low)"),
#' leaving every level in between as its bare "Q<n>" label. Only renames
#' levels in place - grouping/ordering is untouched, so this is safe to
#' apply purely for display right before plotting.
#' @noRd
label_stratum_extremes <- function (x) {
    lv <- levels (x)
    if (length (lv) >= 2) {
        lv [1] <- paste (lv [1], "(low)")
        lv [length (lv)] <- paste (lv [length (lv)], "(high)")
    }
    levels (x) <- lv
    x
}

#' Trailing rolling sum: `out[i]` is the sum of `x[(i - window + 1):i]`, or
#' of just `x[1:i]` (a shorter, partial window) while `i < window` - so the
#' first `window - 1` entries are under-weighted rather than dropped or NA.
#' `x` is assumed already ordered along the dimension (e.g. month) the
#' window rolls over.
#' @noRd
trailing_roll_sum <- function (x, window) {
    cx <- cumsum (x)
    cx - dplyr::lag (cx, n = window, default = 0)
}

#' Build the (popularity stratum-x-month) issue-rate table for one source:
#' a chosen `metric` from non-contributor issues, aggregated over a
#' trailing rolling `window` of months and normalized by repo-months of
#' exposure, where a repo's exposure begins at its GitHub creation date.
#' "Non-contributor" here means `contribution <= contrib_threshold` (see
#' `github_issue_authors()` for how `contribution` - each author's
#' fractional share of all commits ever landed on the repo - is computed).
#' `repo_created_at` lives on `issue_authors_tbl` (fetched alongside each
#' repo's issues by `github_issue_authors()`/`fetch_issue_authors()`), not
#' `repo_tbl`, so a repo only contributes exposure once it's been fetched
#' at least once - repos with zero issues fetched (either not yet fetched
#' at all, or fetched and genuinely having none) don't have a
#' `repo_created_at` on file and are excluded here rather than analyzed.
#'
#' Each reported month is a trailing aggregate over `window` months (that
#' month and the `window - 1` preceding it), not a single month's own
#' count - smoothing month-to-month noise at the cost of some lag, and of
#' treating the `window - 1` months at the very start of the series as a
#' shorter, partial window rather than dropping them.
#'
#' @param issue_authors_tbl As returned by `fetch_issue_authors()`.
#' @param repo_tbl As returned by `build_repo_tbl()`.
#' @param source_name One of `repo_tbl$source` (`"pypi"`, `"npm"`, `"joss"`,
#' `"ropensci"`).
#' @param n_strata Number of popularity strata.
#' @param contrib_threshold Issues whose author's `contribution` is at or
#' below this value count as "non-contributor" issues. Default 0.01 (allow
#' a small nonzero commit share and still call it "non-contributor").
#' @param metric Which per-issue quantity to aggregate: `"issues"` (default)
#' counts qualifying issues; `"comments"` sums qualifying issues'
#' `n_comments` instead.
#' @param window Trailing aggregation window, in months. Default 12: each
#' reported month's `n_metric`/`n_repo_months` sum that month and the
#' preceding 11.
#' @param date_start,date_end Date bounds on the analysis window;
#' `date_end` defaults to the start of the current month.
#' @return A tibble with one row per (popularity stratum, month):
#' `popularity_stratum`, `month` (Date, first-of-month), `n_metric`,
#' `n_repo_months`, `rate` - the latter two already `window`-month trailing
#' sums, not single-month counts. Also carries `metric`, `window`,
#' `contrib_threshold`, and `source_name` as attributes, so `plot_activity()`
#' can label its y-axis correctly without being told them again.
#' @export
issue_rate_tbl <- function (issue_authors_tbl,
                            repo_tbl,
                            source_name,
                            n_strata = 4L,
                            contrib_threshold = 0.01,
                            metric = c ("issues", "comments"),
                            window = 12L,
                            date_start = as.Date ("2015-01-01"),
                            date_end = NULL) {

    metric <- match.arg (metric)

    # rm no visible binding notes
    source <- repo_url <- .data <- month <- metric_val <-
        popularity_stratum <- n_metric <- n_repo_months <- contribution <-
        created_at <- repo_created_at <- n_comments <- NULL

    metric_col <- unname (POPULARITY_METRIC [source_name])
    if (is.na (metric_col)) {
        stop ("Unknown source: ", source_name, call. = FALSE)
    }

    if (is.null (date_end)) {
        date_end <- floor_month (Sys.Date ())
    }

    repo_created_tbl <- issue_authors_tbl |>
        dplyr::filter (!is.na (repo_created_at)) |>
        dplyr::distinct (repo_url, repo_created_at)

    repos <- repo_tbl |>
        dplyr::filter (source == source_name) |>
        dplyr::distinct (repo_url, .keep_all = TRUE) |>
        dplyr::inner_join (repo_created_tbl, by = "repo_url") |>
        dplyr::mutate (
            repo_created_at = floor_month (repo_created_at),
            metric_val = .data [[metric_col]]
        ) |>
        dplyr::filter (!is.na (metric_val), !is.na (repo_created_at))

    if (nrow (repos) == 0) {
        empty <- tibble::tibble (
            popularity_stratum = factor (ordered = TRUE),
            month = as.Date (character ()),
            n_metric = double (),
            n_repo_months = integer (),
            rate = double ()
        )
        attr (empty, "metric") <- metric
        attr (empty, "window") <- window
        attr (empty, "contrib_threshold") <- contrib_threshold
        attr (empty, "source_name") <- source_name
        return (empty)
    }

    repos$popularity_stratum <- popularity_strata (repos$metric_val, n_strata)
    stratum_levels <- levels (repos$popularity_stratum)

    months <- seq (date_start, date_end, by = "month")

    exposure <- dplyr::cross_join (
        tibble::tibble (repo_url = repos$repo_url),
        tibble::tibble (month = months)
    ) |>
        dplyr::inner_join (
            dplyr::select (
                repos, repo_url, repo_created_at, popularity_stratum
            ),
            by = "repo_url"
        ) |>
        dplyr::filter (month >= pmax (repo_created_at, date_start)) |>
        dplyr::count (popularity_stratum, month, name = "n_repo_months")

    filtered_issues <- issue_authors_tbl |>
        dplyr::filter (repo_url %in% repos$repo_url, contribution <= contrib_threshold) |>
        dplyr::mutate (month = floor_month (created_at)) |>
        dplyr::filter (month >= date_start, month <= date_end) |>
        dplyr::inner_join (
            dplyr::select (repos, repo_url, popularity_stratum),
            by = "repo_url"
        )

    issues <- if (metric == "issues") {
        dplyr::count (filtered_issues, popularity_stratum, month, name = "n_metric")
    } else {
        filtered_issues |>
            dplyr::group_by (popularity_stratum, month) |>
            dplyr::summarise (n_metric = sum (n_comments), .groups = "drop")
    }

    # Full (stratum x month) grid, so the trailing rolling sum below has no
    # gaps in either dimension to silently skip over.
    grid <- dplyr::cross_join (
        tibble::tibble (
            popularity_stratum = factor (stratum_levels, levels = stratum_levels, ordered = TRUE)
        ),
        tibble::tibble (month = months)
    )

    result <- grid |>
        dplyr::left_join (exposure, by = c ("popularity_stratum", "month")) |>
        dplyr::left_join (issues, by = c ("popularity_stratum", "month")) |>
        dplyr::mutate (
            n_metric = dplyr::coalesce (n_metric, 0),
            n_repo_months = dplyr::coalesce (n_repo_months, 0L)
        ) |>
        dplyr::arrange (popularity_stratum, month) |>
        dplyr::group_by (popularity_stratum) |>
        dplyr::mutate (
            n_metric = trailing_roll_sum (n_metric, window),
            n_repo_months = trailing_roll_sum (n_repo_months, window)
        ) |>
        dplyr::ungroup () |>
        dplyr::mutate (
            rate = dplyr::if_else (n_repo_months > 0, n_metric / n_repo_months, NA_real_)
        )

    attr (result, "metric") <- metric
    attr (result, "window") <- window
    attr (result, "contrib_threshold") <- contrib_threshold
    attr (result, "source_name") <- source_name
    result
}

#' Fit the quasi-Poisson GLM described in analysis-plan.md: does the
#' month-over-month trend in issue rate differ across popularity strata?
#' The `month_num:popularity_stratum` interaction is the term of interest -
#' it's what tests whether the long tail is trending differently from the
#' popular head, rather than just reporting one global trend line.
#'
#' @param rate_tbl As returned by `issue_rate_tbl()`.
#' @return A fitted `glm` object.
#' @export
fit_activity_model <- function (rate_tbl) {
    n_repo_months <- NULL # rm no visible binding note

    rate_tbl <- dplyr::filter (rate_tbl, n_repo_months > 0)
    rate_tbl$month_num <-
        as.numeric (rate_tbl$month - min (rate_tbl$month)) / 365.25

    stats::glm (
        n_metric ~ month_num * popularity_stratum +
            offset (log (n_repo_months)),
        data = rate_tbl,
        family = stats::quasipoisson ()
    )
}

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
activity_metric_label <- function (metric = c ("issues", "comments"), window = 12L, contrib_threshold = 0.01) {
    metric <- match.arg (metric)
    verb <- if (metric == "issues") "Issues opened" else "Comments received"
    stringr::str_glue (
        "{verb} per repo-month ({window}-month trailing avg, contrib-threshold={contrib_threshold})"
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
activity_plot_layers <- function (rate_tbl, group_col, y_lab) {
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
#' @export
plot_activity <- function (rate_tbl, start_year = NULL) {
    month <- rate <- popularity_stratum <- NULL # rm no visible binding notes
    metric <- attr (rate_tbl, "metric")
    if (is.null (metric)) metric <- "issues"
    window <- attr (rate_tbl, "window")
    if (is.null (window)) window <- 12L
    contrib_threshold <- attr (rate_tbl, "contrib_threshold")
    if (is.null (contrib_threshold)) contrib_threshold <- 0.01
    source_name <- attr (rate_tbl, "source_name")

    if (!is.null (start_year)) {
        rate_tbl <- dplyr::filter (rate_tbl, month >= as.Date (stringr::str_glue ("{start_year}-01-01")))
    }
    rate_tbl$popularity_stratum <- label_stratum_extremes (rate_tbl$popularity_stratum)

    p <- ggplot2::ggplot (
        rate_tbl,
        ggplot2::aes (month, rate, colour = popularity_stratum)
    ) +
        activity_plot_layers (
            rate_tbl, "popularity_stratum",
            activity_metric_label (metric, window, contrib_threshold)
        ) +
        ggplot2::labs (colour = "Popularity\nstratum") +
        ggplot2::guides (colour = ggplot2::guide_legend (reverse = TRUE))

    if (!is.null (source_name)) {
        display_name <- unname (SOURCE_DISPLAY_NAME [source_name])
        if (is.na (display_name)) display_name <- source_name
        p <- p + ggplot2::annotate (
            "text",
            x = structure (Inf, class = "Date"),
            y = Inf,
            label = display_name,
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
#' @param issue_authors_tbl As returned by `fetch_issue_authors()`.
#' @param repo_tbl As returned by `build_repo_tbl()`.
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
#' @export
plot_activity_by_source <- function (issue_authors_tbl, repo_tbl, stratum,
                                     n_strata = 4L,
                                     contrib_threshold = 0.01,
                                     metric = c ("issues", "comments"),
                                     window = 12L,
                                     relative = TRUE,
                                     start_year = NULL,
                                     ros_joss_mult = 20) {
    month <- rate <- source_name <- popularity_stratum <- NULL # rm no visible binding notes
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
        y_lab <- paste (y_lab, stringr::str_glue ("- ropensci/joss/cran shown at {ros_joss_mult}x"))
    }

    ggplot2::ggplot (
        rate_tbl,
        ggplot2::aes (month, rate, colour = source_name)
    ) +
        activity_plot_layers (rate_tbl, "source_name", y_lab) +
        ggplot2::labs (
            colour = "Source",
            title = stringr::str_glue ("Popularity stratum {stratum} of {n_strata}")
        )
}
