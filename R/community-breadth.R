# Table-generation functions backing the "coding-alone" vignette.
# These answer a narrower question than `issue_rate_tbl()`'s event rate:
# not "how many non-core issues land per repo-month" but "how many distinct
# people file them" - a repository could in principle hold a falling issue
# rate steady by getting fewer, more repetitive strangers rather than fewer
# strangers altogether, and the two aren't the same failure mode. Built as
# a close structural analogue of `issue_rate_tbl()`/`fold_change_tbl()` so
# the same reference-month fold-change framing applies to headcount as it
# does to event counts.

# ---- distinct non-core author density ---------------------------------------

#' Distinct non-core authors per repo-month, by source and popularity stratum
#'
#' For one source, count *distinct* non-core issue authors active per
#' calendar month (not issue events - a single prolific outsider filing ten
#' issues in a month counts once), normalised by repo-months of exposure the
#' same way `issue_rate_tbl()` is, and reported as a 12-month trailing sum
#' of that monthly headcount over the same trailing repo-months denominator.
#' The numerator is a trailing *sum* of monthly distinct-author counts
#' rather than a single window-wide distinct count, so a person active in
#' more than one month within the trailing window is counted once per month
#' they're active, not once for the whole window - a "distinct-author-months"
#' density, comparable in construction to `issue_rate_tbl()`'s "issues" rate
#' rather than a strict lifetime-unique headcount (see the vignette's
#' caveats for what that does and doesn't mean).
#'
#' `contrib_min`/`contrib_threshold` together select which authors count,
#' via `contrib_min < contribution <= contrib_threshold`: the defaults
#' (`-Inf`, `0.01`) reproduce the original non-core-only headcount, but the
#' same function also answers the two questions that motivate it -
#' `contrib_min = 0.01, contrib_threshold = Inf` counts *core* authors
#' only (people over the non-core line), and `contrib_min = -Inf,
#' contrib_threshold = Inf` counts *everyone* who filed an issue,
#' core and non-core alike - "all contributors", in this dataset's only
#' available sense of that phrase (see the vignette's caveats for what
#' that does and doesn't capture, pending direct commit-history-based
#' contributor timelines).
#'
#' @inheritParams issue_rate_tbl
#' @param contrib_min Lower bound (exclusive) on `contribution`; authors
#' are counted when `contrib_min < contribution <= contrib_threshold`.
#' Default `-Inf` (no lower bound, i.e. the non-core-vs-core split is
#' governed by `contrib_threshold` alone, as in the original non-core
#' headcount).
#' @return A tibble with one row per (popularity stratum, month):
#' `popularity_stratum`, `month`, `n_metric` (trailing sum of distinct
#' authors active that month whose `contribution` fell in
#' `(contrib_min, contrib_threshold]`), `n_repo_months`, `rate`. Carries
#' `metric = "issues"`, `window`, `contrib_threshold`, and `source_name` as
#' attributes - `metric` is deliberately set to `"issues"` rather than
#' something like `"authors"` so the result can be passed straight into
#' `plot_activity()`, whose y-axis label should then be overridden (e.g.
#' via `+ ggplot2::labs(y = ...)`) since the value isn't actually an issue
#' rate.
#'
#' @examples
#' \dontrun{
#' ad <- author_density_tbl (issue_authors_tbl, repo_tbl, "pypi")
#' plot_activity (ad) + ggplot2::labs (y = "Distinct non-core authors")
#'
#' # All contributors, core and non-core alike:
#' ad_all <- author_density_tbl (
#'     issue_authors_tbl, repo_tbl, "pypi",
#'     contrib_min = -Inf, contrib_threshold = Inf
#' )
#' }
#' @export
author_density_tbl <- function (issue_authors_tbl,
                                repo_tbl,
                                source_name,
                                n_strata = 4L,
                                contrib_threshold = 0.01,
                                contrib_min = -Inf,
                                window = 12L,
                                date_start = as.Date ("2015-01-01"),
                                date_end = NULL) {

    # rm no visible binding notes
    source <- repo_url <- .data <- month <- metric_val <-
        popularity_stratum <- n_metric <- n_repo_months <- contribution <-
        created_at <- repo_created_at <- author <- NULL

    if (is.null (date_end)) {
        date_end <- floor_month (Sys.Date ())
    }

    metric_col <- unname (POPULARITY_METRIC [source_name])
    if (is.na (metric_col)) {
        stop ("Unknown source: ", source_name, call. = FALSE)
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

    repos$popularity_stratum <- popularity_strata (repos$metric_val, n_strata)
    stratum_levels <- levels (repos$popularity_stratum)

    months <- seq (date_start, date_end, by = "month")

    exposure <- dplyr::cross_join (
        tibble::tibble (repo_url = repos$repo_url),
        tibble::tibble (month = months)
    ) |>
        dplyr::inner_join (
            dplyr::select (repos, repo_url, repo_created_at, popularity_stratum),
            by = "repo_url"
        ) |>
        dplyr::filter (month >= pmax (repo_created_at, date_start)) |>
        dplyr::count (popularity_stratum, month, name = "n_repo_months")

    authors <- issue_authors_tbl |>
        dplyr::filter (
            repo_url %in% repos$repo_url,
            contribution > contrib_min, contribution <= contrib_threshold
        ) |>
        dplyr::mutate (month = floor_month (created_at)) |>
        dplyr::filter (month >= date_start, month <= date_end) |>
        dplyr::inner_join (
            dplyr::select (repos, repo_url, popularity_stratum),
            by = "repo_url"
        ) |>
        dplyr::distinct (popularity_stratum, month, repo_url, author) |>
        dplyr::count (popularity_stratum, month, name = "n_metric")

    grid <- dplyr::cross_join (
        tibble::tibble (
            popularity_stratum = factor (
                stratum_levels,
                levels = stratum_levels, ordered = TRUE
            )
        ),
        tibble::tibble (month = months)
    )

    result <- grid |>
        dplyr::left_join (exposure, by = c ("popularity_stratum", "month")) |>
        dplyr::left_join (authors, by = c ("popularity_stratum", "month")) |>
        dplyr::mutate (
            n_metric = dplyr::coalesce (n_metric, 0L),
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
            rate = dplyr::if_else (
                n_repo_months > 0, n_metric / n_repo_months, NA_real_
            )
        )

    attr (result, "metric") <- "issues"
    attr (result, "window") <- window
    attr (result, "contrib_threshold") <- contrib_threshold
    attr (result, "source_name") <- source_name

    result
}

#' Fold-change in distinct non-core author density, from a reference month
#' to now
#'
#' The `author_density_tbl()` analogue of `fold_change_tbl()`: for each of
#' several sources, compare each popularity stratum's author-density value
#' at a fixed reference month against its most recent value.
#'
#' @inheritParams fold_change_tbl
#' @return A tibble: `source`, `popularity_stratum`, `rate_ref`,
#' `rate_latest`, `fold_change`, `latest_month`.
#'
#' @examples
#' \dontrun{
#' fc <- author_density_fold_change_tbl (issue_authors_tbl, repo_tbl, c ("cran", "npm"))
#' plot_fold_change (fc, SOURCE_DISPLAY_NAME, metric = "issues") +
#'     ggplot2::labs (title = "Distinct authors")
#' }
#' @param contrib_min As in `author_density_tbl()`.
#' @export
author_density_fold_change_tbl <- function (issue_authors_tbl, repo_tbl, sources,
                                            contrib_threshold = 0.01,
                                            contrib_min = -Inf,
                                            window = 12L,
                                            ref_date = as.Date ("2021-01-01")) {

    popularity_stratum <- rate <- month <- rate_latest <- rate_ref <- NULL

    purrr::map_dfr (sources, \ (src) {

        rate_tbl <- author_density_tbl (
            issue_authors_tbl, repo_tbl, src,
            contrib_threshold = contrib_threshold, contrib_min = contrib_min,
            window = window
        )
        rate_tbl <- dplyr::filter (rate_tbl, !is.na (rate))
        if (nrow (rate_tbl) == 0) {
            return (tibble::tibble (
                source = character (), popularity_stratum = factor (),
                rate_ref = double (), rate_latest = double (),
                fold_change = double (), latest_month = as.Date (character ())
            ))
        }

        latest_month <- max (rate_tbl$month)
        ref <- dplyr::filter (rate_tbl, month == ref_date) |>
            dplyr::select (popularity_stratum, rate_ref = rate)
        latest <- dplyr::filter (rate_tbl, month == latest_month) |>
            dplyr::select (popularity_stratum, rate_latest = rate)

        dplyr::inner_join (ref, latest, by = "popularity_stratum") |>
            dplyr::mutate (
                source = src,
                fold_change = rate_latest / rate_ref,
                latest_month = latest_month
            )
    })
}

# ---- solo repositories: the share of active repos with exactly one person ---

#' Share of "active" repositories interacting with exactly one distinct
#' person, per trailing window
#'
#' The most direct available operationalisation of "coding alone": among
#' repositories with *any* issue-based interaction in a trailing window (an
#' "active" repo), what fraction have that activity concentrated in a
#' single distinct author - core or non-core, no `contrib_threshold`
#' applied - rather than spread across two or more people. Unlike
#' `author_density_tbl()`'s per-repo-month rate, this is a repo-level,
#' not-normalised-by-exposure share, deliberately: the question here isn't
#' "how much less does each repo hear from outsiders" but "how many
#' repositories, out of those that hear from anyone at all, hear from only
#' one person" - a repo-count analogue of a shrinking bowling league's
#' membership rolls, not of its per-lane activity rate.
#'
#' @param issue_authors_tbl As returned by `fetch_issue_authors()`.
#' @param repo_tbl As returned by `build_repo_tbl()`.
#' @param sources Character vector of `source_name` values to pool
#' together (see `issue_rate_tbl()`). Repositories from every requested
#' source are pooled into one combined share per month, not reported
#' separately per source.
#' @param window Trailing window, in months, over which a repository's
#' distinct authors are counted before it's classed as solo/non-solo.
#' Default 12, matching every other trailing-window metric in this
#' package.
#' @param months Optional `Date` vector (first-of-month) of specific
#' months to compute the share for, instead of every month in
#' `date_start:date_end` - useful for a cheap two-point (reference vs.
#' latest) comparison without recomputing the full monthly series.
#' @param date_start,date_end Date bounds for the default monthly
#' sequence; ignored if `months` is supplied. `date_end` defaults to the
#' start of the current month.
#' @return A tibble: `month`, `n_active_repos` (repos with at least one
#' distinct author in the trailing window), `n_solo` (of those, repos with
#' exactly one), `solo_share` (`n_solo / n_active_repos`).
#'
#' @examples
#' \dontrun{
#' solo_tbl <- solo_repo_share_tbl (issue_authors_tbl, repo_tbl, c ("cran", "npm"))
#' }
#' @export
solo_repo_share_tbl <- function (issue_authors_tbl, repo_tbl, sources,
                                 window = 12L,
                                 months = NULL,
                                 date_start = as.Date ("2015-01-01"),
                                 date_end = NULL) {

    source <- repo_url <- month <- author <- n_people <- NULL

    if (is.null (date_end)) date_end <- floor_month (Sys.Date ())
    if (is.null (months)) months <- seq (date_start, date_end, by = "month")

    repo_urls <- unique (repo_tbl$repo_url [repo_tbl$source %in% sources])

    d <- issue_authors_tbl |>
        dplyr::filter (repo_url %in% repo_urls) |>
        dplyr::mutate (month = floor_month (created_at)) |>
        dplyr::distinct (repo_url, author, month)

    purrr::map_dfr (months, \ (m) {

        window_start <- seq (m, by = paste0 ("-", window - 1L, " months"), length.out = 2) [2]
        sub <- dplyr::filter (d, month >= window_start, month <= m)

        per_repo <- sub |>
            dplyr::distinct (repo_url, author) |>
            dplyr::count (repo_url, name = "n_people")

        n_active <- nrow (per_repo)
        n_solo <- sum (per_repo$n_people == 1)

        tibble::tibble (
            month = m,
            n_active_repos = n_active,
            n_solo = n_solo,
            solo_share = if (n_active > 0) n_solo / n_active else NA_real_
        )
    })
}
