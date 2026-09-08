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
#' exposure begins at its GitHub creation date.
#'
#' @param issue_authors_tbl As returned by `fetch_issue_authors()`.
#' @param repo_tbl As returned by `build_repo_tbl()`.
#' @param repo_created_tbl As returned by `fetch_repo_created_at()`.
#' @param source_name One of `repo_tbl$source` (`"pypi"`, `"npm"`, `"joss"`,
#' `"ropensci"`).
#' @param n_strata Number of popularity strata.
#' @param window_start,window_end Date bounds on the analysis window;
#' `window_end` defaults to the start of the current month.
#' @return A tibble with one row per (popularity stratum, month):
#' `popularity_stratum`, `month` (Date, first-of-month), `n_issues`,
#' `n_repo_months`, `rate`.
#' @export
issue_rate_tbl <- function (issue_authors_tbl,
                            repo_tbl,
                            repo_created_tbl,
                            source_name,
                            n_strata = 4L,
                            window_start = as.Date ("2015-01-01"),
                            window_end = NULL) {

    # rm no visible binding notes
    source <- repo_url <- month <- popularity_stratum <-
        n_issues <- n_repo_months <- is_contributor <-
        created_at <- repo_created_at <- NULL

    metric_col <- unname (POPULARITY_METRIC [source_name])
    if (is.na (metric_col)) {
        stop ("Unknown source: ", source_name, call. = FALSE)
    }

    if (is.null (window_end)) {
        window_end <- floor_month (Sys.Date ())
    }

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

    months <- seq (window_start, window_end, by = "month")

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
        dplyr::filter (month >= pmax (repo_created_at, window_start)) |>
        dplyr::count (popularity_stratum, month, name = "n_repo_months")

    issues <- issue_authors_tbl |>
        dplyr::filter (repo_url %in% repos$repo_url, !is_contributor) |>
        dplyr::mutate (month = floor_month (created_at)) |>
        dplyr::filter (month >= window_start, month <= window_end) |>
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

#' Plot monthly issue rate (non-contributor issues per repo-month) over
#' time, one line per popularity stratum.
#'
#' @param rate_tbl As returned by `issue_rate_tbl()`.
#' @return A ggplot object.
#' @export
plot_activity <- function (rate_tbl) {
    month <- rate <- popularity_stratum <- NULL # rm no visible binding notes

    ggplot2::ggplot (
        rate_tbl,
        ggplot2::aes (month, rate, colour = popularity_stratum)
    ) +
        ggplot2::geom_line (alpha = 0.3) +
        ggplot2::geom_smooth (se = FALSE, method = "loess", formula = y ~ x) +
        ggplot2::labs (
            x = NULL,
            y = "Issues opened per repo-month (non-contributor authors)",
            colour = "Popularity\nstratum"
        ) +
        ggplot2::theme_minimal ()
}
