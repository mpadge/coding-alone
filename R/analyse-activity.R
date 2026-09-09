# For each source, bin non-contributor issues by month, stratify repos by
# popularity (downloads where it exists for the source, stars otherwise), and
# compute a rate normalized by true repo-months exposure (from each repo's
# GitHub creation date).

POPULARITY_METRIC <- c (
    pypi = "downloads",
    npm = "downloads",
    joss = "stars",
    ropensci = "stars"
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

#' Build the (popularity stratum-x-month) issue-rate table for one source:
#' non-contributor issues opened per repo-month of exposure, where a repo's
#' exposure begins at its GitHub creation date. "Non-contributor" here means
#' `contribution <= contribution_threshold` (see `github_issue_authors()`
#' for how `contribution` - each author's fractional share of all commits
#' ever landed on the repo - is computed). `repo_created_at` lives on
#' `issue_authors_tbl` (fetched alongside each repo's issues by
#' `github_issue_authors()`/`fetch_issue_authors()`), not `repo_tbl`, so a
#' repo only contributes exposure once it's been fetched at least once -
#' repos with zero issues fetched (either not yet fetched at all, or
#' fetched and genuinely having none) don't have a `repo_created_at` on
#' file and are excluded here rather than analyzed.
#'
#' @param issue_authors_tbl As returned by `fetch_issue_authors()`.
#' @param repo_tbl As returned by `build_repo_tbl()`.
#' @param source_name One of `repo_tbl$source` (`"pypi"`, `"npm"`, `"joss"`,
#' `"ropensci"`).
#' @param n_strata Number of popularity strata.
#' @param contribution_threshold Issues whose author's `contribution` is at
#' or below this value count as "non-contributor" issues. Default 0 (only
#' authors with no recorded commits at all), matching the previous exact
#' `is_contributor` flag; raise it to also exclude issues from authors with
#' a small-but-nonzero commit share.
#' @param date_start,date_end Date bounds on the analysis window;
#' `date_end` defaults to the start of the current month.
#' @return A tibble with one row per (popularity stratum, month):
#' `popularity_stratum`, `month` (Date, first-of-month), `n_issues`,
#' `n_repo_months`, `rate`.
#' @export
issue_rate_tbl <- function (issue_authors_tbl,
                            repo_tbl,
                            source_name,
                            n_strata = 4L,
                            contribution_threshold = 0,
                            date_start = as.Date ("2015-01-01"),
                            date_end = NULL) {

    # rm no visible binding notes
    source <- repo_url <- .data <- month <- metric <-
        popularity_stratum <- n_issues <- n_repo_months <- contribution <-
        created_at <- repo_created_at <- NULL

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
            metric = .data [[metric_col]]
        ) |>
        dplyr::filter (!is.na (metric), !is.na (repo_created_at))

    if (nrow (repos) == 0) {
        return (tibble::tibble (
            popularity_stratum = factor (),
            month = as.Date (character ()),
            n_issues = integer (),
            n_repo_months = integer (),
            rate = double ()
        ))
    }

    repos$popularity_stratum <- popularity_strata (repos$metric, n_strata)

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

    issues <- issue_authors_tbl |>
        dplyr::filter (repo_url %in% repos$repo_url, contribution <= contribution_threshold) |>
        dplyr::mutate (month = floor_month (created_at)) |>
        dplyr::filter (month >= date_start, month <= date_end) |>
        dplyr::inner_join (
            dplyr::select (repos, repo_url, popularity_stratum),
            by = "repo_url"
        ) |>
        dplyr::count (popularity_stratum, month, name = "n_issues")

    dplyr::full_join (
        exposure,
        issues,
        by = c ("popularity_stratum", "month")
    ) |>
        dplyr::mutate (
            n_issues = dplyr::coalesce (n_issues, 0L),
            n_repo_months = dplyr::coalesce (n_repo_months, 0L),
            rate = dplyr::if_else (
                n_repo_months > 0, n_issues / n_repo_months, NA_real_
            )
        ) |>
        dplyr::arrange (popularity_stratum, month)
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
        n_issues ~ month_num * popularity_stratum +
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
activity_plot_layers <- function (rate_tbl, group_col) {
    rng <- loess_range (rate_tbl, group_col)
    lower <- if (rng [1] < 0) 0 else NA
    upper <- 1.25 * rng [2]

    list (
        ggplot2::geom_line (alpha = 0.3),
        ggplot2::geom_smooth (se = FALSE, method = "loess", formula = y ~ x),
        ggplot2::coord_cartesian (ylim = c (lower, upper)),
        ggplot2::labs (x = NULL, y = "Issues opened per repo-month (non-contributor authors)"),
        ggplot2::theme_minimal ()
    )
}

#' Plot monthly issue rate (non-contributor issues per repo-month) over
#' time, one line per popularity stratum.
#'
#' @param rate_tbl As returned by `issue_rate_tbl()`.
#' @param start_year Optional year (e.g. `2018`) to start the plotted
#' window from; `NULL` (default) plots `rate_tbl`'s full window. Only
#' crops the display - `rate_tbl` isn't refetched, so this can't extend
#' the window beyond what `issue_rate_tbl()` was already called with.
#' @return A ggplot object.
#' @export
plot_activity <- function (rate_tbl, start_year = NULL) {
    month <- rate <- popularity_stratum <- NULL # rm no visible binding notes

    if (!is.null (start_year)) {
        rate_tbl <- dplyr::filter (rate_tbl, month >= as.Date (stringr::str_glue ("{start_year}-01-01")))
    }

    ggplot2::ggplot (
        rate_tbl,
        ggplot2::aes (month, rate, colour = popularity_stratum)
    ) +
        activity_plot_layers (rate_tbl, "popularity_stratum") +
        ggplot2::labs (colour = "Popularity\nstratum")
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
#' @param n_strata,date_start,date_end Passed to each source's
#' `issue_rate_tbl()` call; must be the same `n_strata` `stratum` is a
#' level of.
#' @param relative If `TRUE` (default), rescale each source by its own
#' mean before plotting - sources sit on very different absolute rate
#' scales (e.g. pypi's raw issue traffic dwarfs ropensci's), which would
#' otherwise squash the smaller sources' trends to flat lines near zero.
#' Puts every line at a comparable "around 1 = that source's own average"
#' scale, so trends are comparable even though absolute rates aren't. Set
#' `FALSE` to plot absolute rates instead.
#' @param start_year Optional year (e.g. `2018`) to start the plotted
#' window from; `NULL` (default) plots the full `date_start`-`date_end`
#' window.
#' @return A ggplot object.
#' @export
plot_activity_by_source <- function (issue_authors_tbl, repo_tbl, stratum,
                                     n_strata = 4L,
                                     date_start = as.Date ("2015-01-01"),
                                     date_end = NULL,
                                     relative = TRUE,
                                     start_year = NULL) {
    month <- rate <- source_name <- popularity_stratum <- NULL # rm no visible binding notes

    if (is.null (date_end)) {
        date_end <- floor_month (Sys.Date ())
    }
    stratum_label <- paste0 ("Q", stratum)
    sources <- names (POPULARITY_METRIC)

    rate_tbl <- purrr::map_dfr (sources, \ (src) {
        issue_rate_tbl (
            issue_authors_tbl, repo_tbl, src,
            n_strata = n_strata, date_start = date_start, date_end = date_end
        ) |>
            dplyr::filter (popularity_stratum == stratum_label) |>
            dplyr::mutate (source_name = src)
    })
    rate_tbl$source_name <- factor (rate_tbl$source_name, levels = sources)

    # Rescaling (when requested) happens before the start_year crop below,
    # so the scale factor doesn't shift depending on what window is
    # displayed.
    y_lab <- "Issues opened per repo-month (non-contributor authors)"
    if (relative) {
        rate_tbl <- rate_tbl |>
            dplyr::group_by (source_name) |>
            dplyr::mutate (rate = rate / mean (rate, na.rm = TRUE)) |>
            dplyr::ungroup ()
        y_lab <- paste (y_lab, "- relative to each source's own mean")
    }

    if (!is.null (start_year)) {
        rate_tbl <- dplyr::filter (rate_tbl, month >= as.Date (stringr::str_glue ("{start_year}-01-01")))
    }

    ggplot2::ggplot (
        rate_tbl,
        ggplot2::aes (month, rate, colour = source_name)
    ) +
        activity_plot_layers (rate_tbl, "source_name") +
        ggplot2::labs (
            y = y_lab,
            colour = "Source",
            title = stringr::str_glue ("Popularity stratum {stratum} of {n_strata}")
        )
}
