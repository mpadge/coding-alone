# Shared low-level helpers used across the table-/analyse-/plot- files: the
# popularity-stratum machinery, month-flooring, and the trailing-window
# rolling sum that every rate table in this package is built on.

POPULARITY_METRIC <- c (
    pypi = "downloads",
    npm = "downloads",
    joss = "stars",
    ropensci = "stars",
    cran = "downloads"
)

#' Proper-cased display forms of each source's internal (lowercase)
#' `POPULARITY_METRIC`/`repo_tbl$source` key, for anywhere a source name is
#' shown to a reader rather than matched against data (e.g. plot
#' annotations). "npm" is genuinely lowercase as a name, not an
#' abbreviation, so it's left as-is.
#' @format A named character vector, one entry per source.
#' @export
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
