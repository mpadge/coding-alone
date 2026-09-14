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
#' @param contrib_threshold,window Passed to `author_density_tbl()`.
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

    source <- repo_url <- created_at <- month <- author <- n_people <- NULL

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

# ---- community expansion: new (non-founding) author arrival rate -----------

#' New (non-founding) authors first appearing per repo-month, by source and
#' popularity stratum
#'
#' Community-expansion analogue of `author_density_tbl()`: rather than "how
#' many distinct authors are active this month", counts how many people are
#' showing up in a repository's issue tracker *for the first time ever* -
#' a direct measure of whether a repo's community of interlocutors is still
#' growing, or has stalled to the same recurring faces. Each repo's very
#' first-ever issue author (typically the maintainer opening the repo's own
#' first issue) is excluded as a "founding" event rather than a new
#' arrival, since it isn't itself community growth. As with
#' `issue_rate_tbl()`/`author_density_tbl()`, the result is normalised by
#' repo-months of exposure and reported as a `window`-month trailing sum,
#' so a single burst of new signups doesn't read as a permanent step
#' change.
#'
#' Author identity here isn't split by `contribution`/`contrib_threshold`
#' as most of this package's other rate tables are - someone who goes on to
#' become a heavy contributor is still a new arrival the month they first
#' show up, so every first-time author counts, core and non-core alike,
#' matching `solo_repo_share_tbl()`'s treatment of contribution rather than
#' `issue_rate_tbl()`'s.
#'
#' @inheritParams issue_rate_tbl
#' @return A tibble with one row per (popularity stratum, month):
#' `popularity_stratum`, `month`, `n_metric` (trailing sum of new,
#' non-founding first-time authors), `n_repo_months`, `rate`. Carries
#' `metric = "issues"`, `window`, and `source_name` as attributes (no
#' `contrib_threshold`, since none applies here) so `plot_activity()`/
#' `plot_new_author_rate()` don't need to be told them again.
#'
#' @examples
#' \dontrun{
#' na <- new_author_rate_tbl (issue_authors_tbl, repo_tbl, "pypi")
#' plot_activity (na) + ggplot2::labs (y = "New (non-founding) authors")
#' }
#' @export
new_author_rate_tbl <- function (issue_authors_tbl,
                                 repo_tbl,
                                 source_name,
                                 n_strata = 4L,
                                 window = 12L,
                                 date_start = as.Date ("2015-01-01"),
                                 date_end = NULL) {

    # rm no visible binding notes
    source <- repo_url <- .data <- month <- metric_val <-
        popularity_stratum <- n_metric <- n_repo_months <-
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

    if (nrow (repos) == 0) {
        empty <- tibble::tibble (
            popularity_stratum = factor (ordered = TRUE),
            month = as.Date (character ()),
            n_metric = double (),
            n_repo_months = integer (),
            rate = double ()
        )
        attr (empty, "metric") <- "issues"
        attr (empty, "window") <- window
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
            dplyr::select (repos, repo_url, repo_created_at, popularity_stratum),
            by = "repo_url"
        ) |>
        dplyr::filter (month >= pmax (repo_created_at, date_start)) |>
        dplyr::count (popularity_stratum, month, name = "n_repo_months")

    # Each repo's first-ever appearance of each author, ordered
    # chronologically within the repo (sorted ascending before `distinct()`
    # so the row it keeps per author is their *earliest*, not latest,
    # issue) - so the first row per repo is that repo's founding author,
    # excluded below as not itself a "new arrival".
    first_appearances <- issue_authors_tbl |>
        dplyr::filter (repo_url %in% repos$repo_url) |>
        dplyr::arrange (repo_url, created_at) |>
        dplyr::distinct (repo_url, author, .keep_all = TRUE)

    repo_grp <- dplyr::consecutive_id (first_appearances$repo_url)
    is_founder <- !duplicated (repo_grp)
    new_arrivals <- first_appearances [!is_founder, ] |>
        dplyr::mutate (month = floor_month (created_at)) |>
        dplyr::filter (month >= date_start, month <= date_end) |>
        dplyr::inner_join (
            dplyr::select (repos, repo_url, popularity_stratum),
            by = "repo_url"
        )

    new_counts <- dplyr::count (
        new_arrivals, popularity_stratum, month,
        name = "n_metric"
    )

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
        dplyr::left_join (new_counts, by = c ("popularity_stratum", "month")) |>
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
    attr (result, "source_name") <- source_name

    result
}

# ---- community expansion: time between consecutive first-time authors -----

#' Time elapsed between consecutive first-time authors, by source and
#' popularity stratum
#'
#' Event-level alternative to `new_author_rate_tbl()`'s repo-month rate:
#' instead of asking "how many new arrivals landed this month, normalised
#' by repo-months of exposure", this asks "how long does a repository wait
#' between one first-time author and the next". For one source, orders
#' each repository's distinct issue authors by their own first-ever
#' appearance in that repo's issue tracker (its founding author first, as
#' identified in `new_author_rate_tbl()` - but *kept* here rather than
#' excluded, since it anchors the very first interval) and computes, for
#' every author after the founder, the elapsed time since the previous
#' first-time author's own first appearance. Each interval is time-stamped
#' at the *arriving* author's own issue, not at the interval's start or
#' midpoint, so a repository that goes quiet for two years and then gains a
#' new contributor logs one long interval dated to the day that contributor
#' actually showed up, not smeared backward across the quiet period. Needs
#' no repo-months exposure denominator at all - a repository with only one
#' author so far simply contributes no interval, rather than a zero.
#'
#' @inheritParams issue_rate_tbl
#' @param date_start,date_end Date bounds on which authors' first
#' appearances are considered; unlike the repo-month rate tables, these
#' bound the raw event timestamps directly rather than a floored calendar
#' month, and `date_end` defaults to `Sys.Date()` (today), not the start of
#' the current month, since there is no partial-month repo-months exposure
#' to worry about truncating here.
#' @return A tibble with one row per (repository, author arrival after the
#' founder): `repo_url`, `popularity_stratum`, `arrival_index` (2 = the
#' first author after the founder, 3 = the second, and so on), `event_time`
#' (the arriving author's own first-issue timestamp - when this interval is
#' "logged"), `interval_days` (elapsed time, in days, since the previous
#' first-time author's own first appearance). Carries `source_name` as an
#' attribute.
#'
#' @examples
#' \dontrun{
#' iv <- author_interval_tbl (issue_authors_tbl, repo_tbl, "pypi")
#' plot_author_interval (issue_authors_tbl, repo_tbl)
#' }
#' @export
author_interval_tbl <- function (issue_authors_tbl,
                                 repo_tbl,
                                 source_name,
                                 n_strata = 4L,
                                 date_start = as.Date ("2015-01-01"),
                                 date_end = NULL) {

    # rm no visible binding notes
    source <- repo_url <- .data <- metric_val <- popularity_stratum <-
        created_at <- repo_created_at <- author <- arrival_index <- NULL

    if (is.null (date_end)) {
        date_end <- Sys.Date ()
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
        dplyr::mutate (metric_val = .data [[metric_col]]) |>
        dplyr::filter (!is.na (metric_val))

    empty <- tibble::tibble (
        repo_url = character (),
        popularity_stratum = factor (ordered = TRUE),
        arrival_index = integer (),
        event_time = as.Date (character ()),
        interval_days = double ()
    )

    if (nrow (repos) == 0) {
        attr (empty, "source_name") <- source_name
        return (empty)
    }

    repos$popularity_stratum <- popularity_strata (repos$metric_val, n_strata)

    # Each repo's distinct authors, ordered by their own first-ever
    # appearance - ascending sort before `distinct()` keeps each author's
    # earliest, not latest, issue.
    first_appearances <- issue_authors_tbl |>
        dplyr::filter (
            repo_url %in% repos$repo_url,
            as.Date (created_at) >= date_start, as.Date (created_at) <= date_end
        ) |>
        dplyr::arrange (repo_url, created_at) |>
        dplyr::distinct (repo_url, author, .keep_all = TRUE) |>
        dplyr::select (repo_url, author, created_at) |>
        dplyr::inner_join (
            dplyr::select (repos, repo_url, popularity_stratum),
            by = "repo_url"
        )

    if (nrow (first_appearances) == 0) {
        attr (empty, "source_name") <- source_name
        return (empty)
    }

    result <- first_appearances |>
        dplyr::arrange (repo_url, created_at) |>
        dplyr::group_by (repo_url) |>
        dplyr::mutate (
            arrival_index = dplyr::row_number (),
            interval_days = as.numeric (
                difftime (created_at, dplyr::lag (created_at), units = "days")
            )
        ) |>
        dplyr::ungroup () |>
        dplyr::filter (arrival_index >= 2) |>
        dplyr::transmute (
            repo_url, popularity_stratum, arrival_index,
            event_time = created_at, interval_days
        )

    attr (result, "source_name") <- source_name

    result
}

#' Rolling geometric-mean wait time between consecutive first-time authors,
#' by source and popularity stratum
#'
#' Aggregates `author_interval_tbl()`'s event-level intervals into a
#' (popularity stratum x month) grid: each interval is binned by the
#' calendar month of its `event_time`, and reported as a `window`-month
#' trailing geometric mean of `interval_days`. A geometric (not arithmetic)
#' mean is used because wait times between authors are heavily
#' right-skewed - in a sparse stratum-month cell, a single repo that went
#' quiet for years would otherwise dominate an arithmetic mean of just a
#' handful of intervals. A handful of near-simultaneous arrivals
#' (`interval_days` at or near 0) are floored at one minute before logging,
#' since `log(0) = -Inf` would otherwise wreck that whole cell's geometric
#' mean rather than just pulling it down.
#'
#' @inheritParams issue_rate_tbl
#' @return A tibble with one row per (popularity stratum, month):
#' `popularity_stratum`, `month`, `n_events` (trailing sum of author
#' arrivals contributing an interval that month), `geo_mean_days` (the
#' `window`-month trailing geometric mean of `interval_days`, `NA` where
#' `n_events` is 0). Carries `window` and `source_name` as attributes.
#'
#' @examples
#' \dontrun{
#' it <- author_interval_trend_tbl (issue_authors_tbl, repo_tbl, "pypi")
#' }
#' @export
author_interval_trend_tbl <- function (issue_authors_tbl,
                                       repo_tbl,
                                       source_name,
                                       n_strata = 4L,
                                       window = 12L,
                                       date_start = as.Date ("2015-01-01"),
                                       date_end = NULL) {

    popularity_stratum <- month <- event_time <- interval_days <-
        sum_log_days <- n_events <- geo_mean_days <- NULL

    iv <- author_interval_tbl (
        issue_authors_tbl, repo_tbl, source_name,
        n_strata = n_strata, date_start = date_start, date_end = date_end
    )

    empty <- tibble::tibble (
        popularity_stratum = factor (ordered = TRUE),
        month = as.Date (character ()),
        n_events = integer (),
        geo_mean_days = double ()
    )

    if (nrow (iv) == 0) {
        attr (empty, "window") <- window
        attr (empty, "source_name") <- source_name
        return (empty)
    }

    monthly <- iv |>
        dplyr::mutate (month = floor_month (event_time)) |>
        dplyr::group_by (popularity_stratum, month) |>
        dplyr::summarise (
            sum_log_days = sum (log (pmax (interval_days, 1 / 1440))),
            n_events = dplyr::n (),
            .groups = "drop"
        )

    stratum_levels <- levels (iv$popularity_stratum)
    months <- seq (min (monthly$month), max (monthly$month), by = "month")

    grid <- tidyr::expand_grid (
        popularity_stratum = factor (
            stratum_levels,
            levels = stratum_levels, ordered = TRUE
        ),
        month = months
    )

    result <- grid |>
        dplyr::left_join (monthly, by = c ("popularity_stratum", "month")) |>
        dplyr::mutate (
            sum_log_days = dplyr::coalesce (sum_log_days, 0),
            n_events = dplyr::coalesce (n_events, 0L)
        ) |>
        dplyr::arrange (popularity_stratum, month) |>
        dplyr::group_by (popularity_stratum) |>
        dplyr::mutate (
            sum_log_days = trailing_roll_sum (sum_log_days, window),
            n_events = trailing_roll_sum (n_events, window)
        ) |>
        dplyr::ungroup () |>
        dplyr::mutate (
            geo_mean_days = dplyr::if_else (
                n_events > 0, exp (sum_log_days / n_events), NA_real_
            )
        ) |>
        dplyr::select (popularity_stratum, month, n_events, geo_mean_days)

    attr (result, "window") <- window
    attr (result, "source_name") <- source_name

    result
}
