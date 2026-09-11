# Table-generation functions used by the "review-dividend" vignette (and
# generally useful for anyone re-running these analyses on their own data).
# These sit alongside the plotting functions in R/plots.R rather than in
# R/analyse-activity.R because they recombine that file's primitives
# (`issue_rate_tbl()`, `popularity_strata()`, etc.) into higher-level
# comparisons - fold-change, cohort/age disentangling, and the rOpenSci
# reviewed-vs-non-reviewed split - rather than computing a rate table
# directly from `issue_authors_tbl`/`repo_tbl`.

# ---- cross-source fold-change (rate now vs. a fixed reference month) -------

#' Fold-change in non-core rate, from a reference month to now
#'
#' For each of several sources, compare each popularity stratum's trailing
#' 12-month rate at a fixed reference month against its most recent value.
#' A fixed reference month (rather than each stratum's own peak) is used
#' deliberately: individual per-stratum peaks are noisy, especially for
#' sparser low-popularity strata where a single active month can dominate a
#' small repo-month denominator, so anchoring on one shared calendar month
#' both avoids cherry-picking and keeps strata/sources comparable.
#'
#' @param issue_authors_tbl As returned by `fetch_issue_authors()`.
#' @param repo_tbl As returned by `build_repo_tbl()`.
#' @param sources Character vector of `source_name` values to include (see
#' `issue_rate_tbl()`).
#' @param metric,contrib_threshold,window Passed to `issue_rate_tbl()`.
#' @param ref_date Reference `Date` (first-of-month) to compare against the
#' latest available month.
#' @return A tibble: `source`, `popularity_stratum`, `rate_ref`,
#' `rate_latest`, `fold_change` (`rate_latest / rate_ref`), `latest_month`.
#'
#' @examples
#' \dontrun{
#' fc <- fold_change_tbl (issue_authors_tbl, repo_tbl, c ("cran", "npm"))
#' plot_fold_change (fc)
#' }
#' @export
fold_change_tbl <- function (issue_authors_tbl, repo_tbl, sources,
                             metric = "issues", contrib_threshold = 0.01,
                             window = 12L,
                             ref_date = as.Date ("2021-01-01")) {

    popularity_stratum <- rate <- month <- NULL

    purrr::map_dfr (sources, \ (src) {

        rate_tbl <- issue_rate_tbl (
            issue_authors_tbl, repo_tbl, src,
            contrib_threshold = contrib_threshold, metric = metric,
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

# ---- matched-cohort age-vs-calendar-time analysis --------------------------

#' Non-core rate by creation-year cohort and fixed repo age
#'
#' For each source, bin non-core issue activity by (creation-year cohort,
#' age-in-years-since-creation) instead of by (popularity stratum, calendar
#' month). This is the age/period/cohort disentangling trick: fixing "age"
#' (e.g. a repo's first or second year of life) while letting "cohort"
#' (the calendar year that age happened to fall in) vary isolates a shared
#' calendar-time effect from a repository's own maturation curve. Pure
#' maturation predicts roughly constant rates across cohorts at a fixed age;
#' a shared calendar-time break instead predicts a monotonic decline in that
#' fixed-age rate as cohort year increases.
#'
#' @param issue_authors_tbl As returned by `fetch_issue_authors()`.
#' @param repo_tbl As returned by `build_repo_tbl()`.
#' @param sources Character vector of `source_name` values to include.
#' @param contrib_threshold,metric As in `issue_rate_tbl()`.
#' @param age_years Integer vector of ages (in whole years since repo
#' creation) to compute rates for. Default `0:1` (a repo's first and second
#' year of life).
#' @param date_start Earliest calendar month considered when accumulating
#' repo-month exposure (not an age cutoff).
#' @return A tibble: `source`, `cohort_year`, `age_year`, `n_metric`,
#' `n_repo_months`, `rate`.
#'
#' @examples
#' \dontrun{
#' cohort_tbl <- cohort_age_rate_tbl (issue_authors_tbl, repo_tbl, "cran")
#' plot_cohort_age (cohort_tbl)
#' }
#' @export
cohort_age_rate_tbl <- function (issue_authors_tbl, repo_tbl, sources,
                                 contrib_threshold = 0.01,
                                 metric = c ("issues", "comments"),
                                 age_years = 0:1,
                                 date_start = as.Date ("2010-01-01")) {

    metric <- match.arg (metric)

    # rm no visible binding notes
    source <- repo_url <- repo_created_at <- month <- cohort_year <-
        age_year <- n_metric <- n_repo_months <- contribution <-
        created_at <- n_comments <- NULL

    repo_created <- issue_authors_tbl |>
        dplyr::filter (!is.na (repo_created_at)) |>
        dplyr::distinct (repo_url, repo_created_at)

    # `issue_authors_tbl` already carries its own `repo_created_at` column
    # (one row per issue, not per repo), so it has to be dropped before
    # joining the repo-level cohort table back on, or the join silently
    # produces `repo_created_at.x`/`.y` instead of erroring.
    issue_authors_tbl <- dplyr::select (issue_authors_tbl, -repo_created_at)

    purrr::map_dfr (sources, \ (src) {

        repos <- repo_tbl |>
            dplyr::filter (source == src) |>
            dplyr::distinct (repo_url, .keep_all = TRUE) |>
            dplyr::inner_join (repo_created, by = "repo_url") |>
            dplyr::mutate (
                repo_created_at = floor_month (repo_created_at),
                cohort_year = as.integer (format (repo_created_at, "%Y"))
            )

        months <- seq (date_start, floor_month (Sys.Date ()), by = "month")
        repo_meta <- dplyr::select (repos, repo_url, repo_created_at, cohort_year)

        age_of <- function (month, created) {
            (as.integer (format (month, "%Y")) * 12L +
                as.integer (format (month, "%m"))) -
                (as.integer (format (created, "%Y")) * 12L +
                    as.integer (format (created, "%m")))
        }

        exposure <- dplyr::cross_join (
            tibble::tibble (repo_url = repos$repo_url),
            tibble::tibble (month = months)
        ) |>
            dplyr::inner_join (repo_meta, by = "repo_url") |>
            dplyr::filter (month >= repo_created_at) |>
            dplyr::mutate (
                age_year = age_of (month, repo_created_at) %/% 12L
            ) |>
            dplyr::filter (age_year %in% age_years) |>
            dplyr::count (cohort_year, age_year, name = "n_repo_months")

        filtered_issues <- issue_authors_tbl |>
            dplyr::filter (
                repo_url %in% repos$repo_url, contribution <= contrib_threshold
            ) |>
            dplyr::mutate (month = floor_month (created_at)) |>
            dplyr::inner_join (repo_meta, by = "repo_url") |>
            dplyr::filter (month >= repo_created_at) |>
            dplyr::mutate (
                age_year = age_of (month, repo_created_at) %/% 12L
            ) |>
            dplyr::filter (age_year %in% age_years)

        issues <- if (metric == "issues") {
            dplyr::count (filtered_issues, cohort_year, age_year, name = "n_metric")
        } else {
            filtered_issues |>
                dplyr::group_by (cohort_year, age_year) |>
                dplyr::summarise (n_metric = sum (n_comments), .groups = "drop")
        }

        dplyr::full_join (exposure, issues, by = c ("cohort_year", "age_year")) |>
            dplyr::mutate (
                n_metric = dplyr::coalesce (n_metric, 0L),
                n_repo_months = dplyr::coalesce (n_repo_months, 0L),
                rate = dplyr::if_else (
                    n_repo_months > 0, n_metric / n_repo_months, NA_real_
                ),
                source = src
            )
    })
}

# ---- rOpenSci: reviewed vs. non-reviewed packages ---------------------------

#' Non-core rate for rOpenSci packages, by review status and popularity
#'
#' Per-(popularity stratum x reviewed-status x month) non-core rate for
#' rOpenSci repositories only - the same trailing-window, exposure-normalised
#' construction as `issue_rate_tbl()`, but with formal-review status (from
#' rOpenSci's own package metadata) crossed with popularity stratum instead
#' of stratifying by popularity alone. Repos with no recorded `reviewed`
#' status are dropped rather than silently pooled into either group.
#'
#' @param issue_authors_tbl As returned by `fetch_issue_authors()`.
#' @param repo_tbl As returned by `build_repo_tbl()`.
#' @param ropensci_raw A tibble of rOpenSci package metadata with at least
#' `repo_url` and `reviewed` (logical) columns, e.g. rOpenSci's own
#' `packages.csv`/`ropensci.csv` listing.
#' @param n_strata,contrib_threshold,metric,window,date_start,date_end As in
#' `issue_rate_tbl()`.
#' @return A tibble: `popularity_stratum`, `reviewed`, `month`, `n_metric`,
#' `n_repo_months`, `rate`. Carries `metric`/`window`/`contrib_threshold` as
#' attributes, as `issue_rate_tbl()` does.
#'
#' @examples
#' \dontrun{
#' ros <- ropensci_reviewed_activity_tbl (issue_authors_tbl, repo_tbl, ropensci_raw)
#' plot_ropensci_reviewed_trend (ros)
#' }
#' @export
ropensci_reviewed_activity_tbl <- function (issue_authors_tbl, repo_tbl, ropensci_raw,
                                            n_strata = 4L,
                                            contrib_threshold = 0.01,
                                            metric = c ("issues", "comments"),
                                            window = 12L,
                                            date_start = as.Date ("2015-01-01"),
                                            date_end = NULL) {

    metric <- match.arg (metric)

    source <- repo_url <- month <- popularity_stratum <- reviewed <-
        n_metric <- n_repo_months <- contribution <- created_at <-
        repo_created_at <- n_comments <- stars <- NULL

    if (is.null (date_end)) date_end <- floor_month (Sys.Date ())

    repo_created_tbl <- issue_authors_tbl |>
        dplyr::filter (!is.na (repo_created_at)) |>
        dplyr::distinct (repo_url, repo_created_at)

    repos <- repo_tbl |>
        dplyr::filter (source == "ropensci") |>
        dplyr::distinct (repo_url, .keep_all = TRUE) |>
        dplyr::inner_join (repo_created_tbl, by = "repo_url") |>
        dplyr::mutate (repo_created_at = floor_month (repo_created_at)) |>
        dplyr::filter (!is.na (stars), !is.na (repo_created_at)) |>
        dplyr::left_join (
            dplyr::select (ropensci_raw, repo_url, reviewed),
            by = "repo_url"
        ) |>
        dplyr::filter (!is.na (reviewed))

    repos$popularity_stratum <- popularity_strata (repos$stars, n_strata)
    stratum_levels <- levels (repos$popularity_stratum)

    months <- seq (date_start, date_end, by = "month")

    exposure <- dplyr::cross_join (
        tibble::tibble (repo_url = repos$repo_url),
        tibble::tibble (month = months)
    ) |>
        dplyr::inner_join (
            dplyr::select (
                repos, repo_url, repo_created_at, popularity_stratum, reviewed
            ),
            by = "repo_url"
        ) |>
        dplyr::filter (month >= pmax (repo_created_at, date_start)) |>
        dplyr::count (popularity_stratum, reviewed, month, name = "n_repo_months")

    filtered_issues <- issue_authors_tbl |>
        dplyr::filter (
            repo_url %in% repos$repo_url, contribution <= contrib_threshold
        ) |>
        dplyr::mutate (month = floor_month (created_at)) |>
        dplyr::filter (month >= date_start, month <= date_end) |>
        dplyr::inner_join (
            dplyr::select (repos, repo_url, popularity_stratum, reviewed),
            by = "repo_url"
        )

    issues <- if (metric == "issues") {
        dplyr::count (
            filtered_issues, popularity_stratum, reviewed, month,
            name = "n_metric"
        )
    } else {
        filtered_issues |>
            dplyr::group_by (popularity_stratum, reviewed, month) |>
            dplyr::summarise (n_metric = sum (n_comments), .groups = "drop")
    }

    grid <- dplyr::cross_join (
        tidyr::expand_grid (
            popularity_stratum = factor (
                stratum_levels,
                levels = stratum_levels, ordered = TRUE
            ),
            reviewed = c (TRUE, FALSE)
        ),
        tibble::tibble (month = months)
    )

    result <- grid |>
        dplyr::left_join (exposure, by = c ("popularity_stratum", "reviewed", "month")) |>
        dplyr::left_join (issues, by = c ("popularity_stratum", "reviewed", "month")) |>
        dplyr::mutate (
            n_metric = dplyr::coalesce (n_metric, 0),
            n_repo_months = dplyr::coalesce (n_repo_months, 0L)
        ) |>
        dplyr::arrange (popularity_stratum, reviewed, month) |>
        dplyr::group_by (popularity_stratum, reviewed) |>
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

    result
}

#' Fold-change in rOpenSci non-core rate, by review status
#'
#' Fold-change (latest 12-month rate vs. a fixed reference month) computed
#' from `ropensci_reviewed_activity_tbl()` output, by (popularity stratum x
#' reviewed status) rather than by (source x popularity stratum) as in
#' `fold_change_tbl()`.
#'
#' @param tbl As returned by `ropensci_reviewed_activity_tbl()`.
#' @param ref_date As in `fold_change_tbl()`.
#' @return A tibble: `popularity_stratum`, `reviewed`, `rate_ref`,
#' `rate_latest`, `fold_change`, `latest_month`.
#'
#' @examples
#' \dontrun{
#' ros <- ropensci_reviewed_activity_tbl (issue_authors_tbl, repo_tbl, ropensci_raw)
#' ros_fc <- ropensci_reviewed_fold_change_tbl (ros)
#' plot_ropensci_reviewed_fold_change (ros_fc)
#' }
#' @export
ropensci_reviewed_fold_change_tbl <- function (tbl, ref_date = as.Date ("2021-01-01")) {

    popularity_stratum <- reviewed <- rate <- month <- NULL

    tbl <- dplyr::filter (tbl, !is.na (rate))
    latest_month <- max (tbl$month)

    ref <- dplyr::filter (tbl, month == ref_date) |>
        dplyr::select (popularity_stratum, reviewed, rate_ref = rate)
    latest <- dplyr::filter (tbl, month == latest_month) |>
        dplyr::select (popularity_stratum, reviewed, rate_latest = rate)

    dplyr::inner_join (ref, latest, by = c ("popularity_stratum", "reviewed")) |>
        dplyr::mutate (
            fold_change = rate_latest / rate_ref,
            latest_month = latest_month
        )
}
