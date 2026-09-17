# ---- commit-based activity: a direct, non-issue-tracker measure ------------

#' Monthly commit rate per repo-month, by source
#'
#' Direct, commit-history-based analogue of `issue_rate_tbl()`/
#' `author_density_tbl()`: rather than counting issue-tracker events or
#' distinct issue authors, counts actual commits landed on each repo's
#' default branch (as fetched by `fetch_repo_commits()`), normalised by
#' repo-months of exposure and reported as a `window`-month trailing sum
#' over the same trailing repo-months denominator. A direct measure of
#' code-level activity to compare against the issue-tracker-based measures
#' elsewhere in this package - it can see contributors who only ever
#' commit and never file an issue, which `author_density_tbl()`'s "all
#' contributors" reading can't (see that function's doc).
#'
#' Unlike every other rate table in this package, this one isn't split by
#' popularity stratum: commit rate shows no material difference between
#' popularity strata, so pooling all of a source's repos into one line
#' loses nothing a stratified version would show and is simpler to read.
#'
#' As in `author_density_tbl()`/`new_author_rate_tbl()`, `repo_created_at`
#' is read off `issue_authors_tbl` (not `commit_counts_tbl`, which has no
#' such column) to compute repo-months exposure, so a repo only
#' contributes exposure once it's been fetched at least once by
#' `fetch_issue_authors()`.
#'
#' @param commit_counts_tbl As returned by `fetch_repo_commits()` (or read
#' straight from `commit-counts.csv`): one row per (repo, month) with
#' `n_commits`.
#' @inheritParams issue_rate_tbl
#' @return A tibble with one row per month: `month`, `n_metric` (trailing
#' sum of commits), `n_repo_months`, `rate`. Carries `window` and
#' `source_name` as attributes.
#'
#' @examples
#' \dontrun{
#' commit_counts_tbl <- readr::read_csv ("repo-data-out/commit-counts.csv")
#' cr <- commit_rate_tbl (commit_counts_tbl, issue_authors_tbl, repo_tbl, "pypi")
#' }
#' @export
commit_rate_tbl <- function (commit_counts_tbl,
                             issue_authors_tbl,
                             repo_tbl,
                             source_name,
                             window = 12L,
                             date_start = as.Date ("2015-01-01"),
                             date_end = NULL) {

    # rm no visible binding notes
    source <- repo_url <- .data <- month <- metric_val <-
        n_metric <- n_repo_months <- repo_created_at <- n_commits <- NULL

    if (is.null (date_end)) {
        date_end <- floor_month (Sys.Date ())
    }

    metric_col <- unname (POPULARITY_METRIC [source_name])
    if (is.na (metric_col)) {
        stop ("Unknown source: ", source_name, call. = FALSE)
    }

    months <- seq (date_start, date_end, by = "month")

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
        result <- tibble::tibble (
            month = months, n_metric = 0, n_repo_months = 0L, rate = NA_real_
        )
        attr (result, "window") <- window
        attr (result, "source_name") <- source_name
        return (result)
    }

    exposure <- dplyr::cross_join (
        tibble::tibble (repo_url = repos$repo_url),
        tibble::tibble (month = months)
    ) |>
        dplyr::inner_join (
            dplyr::select (repos, repo_url, repo_created_at),
            by = "repo_url"
        ) |>
        dplyr::filter (month >= pmax (repo_created_at, date_start)) |>
        dplyr::count (month, name = "n_repo_months")

    commits <- commit_counts_tbl |>
        dplyr::filter (
            repo_url %in% repos$repo_url,
            month >= date_start, month <= date_end
        ) |>
        dplyr::group_by (month) |>
        dplyr::summarise (n_metric = sum (n_commits), .groups = "drop")

    result <- tibble::tibble (month = months) |>
        dplyr::left_join (exposure, by = "month") |>
        dplyr::left_join (commits, by = "month") |>
        dplyr::mutate (
            n_metric = dplyr::coalesce (n_metric, 0),
            n_repo_months = dplyr::coalesce (n_repo_months, 0L)
        ) |>
        dplyr::arrange (month) |>
        dplyr::mutate (
            n_metric = trailing_roll_sum (n_metric, window),
            n_repo_months = trailing_roll_sum (n_repo_months, window),
            rate = dplyr::if_else (
                n_repo_months > 0, n_metric / n_repo_months, NA_real_
            )
        )

    attr (result, "window") <- window
    attr (result, "source_name") <- source_name

    result
}

#' Monthly repo-creation rate, by source
#'
#' Counts how many repositories were created (per GitHub's own
#' `repo_created_at` timestamp, as recorded on `issue_authors_tbl`) each
#' calendar month, for one source, reported as a `window`-month trailing
#' sum in the same way every other rate in this package is - the
#' ecosystem's own raw growth in repo count over time, meant to be read
#' alongside `commit_rate_tbl()`'s per-repo-month commit rate (see
#' `plot_commit_rate()`) so a reader can judge how much of any shift in
#' commit rate reflects more repos existing now rather than a change in
#' per-repo behaviour. Restricted to the same repo population as
#' `commit_rate_tbl()` (repos with a non-`NA` popularity metric and a
#' known creation date), so the two panels describe the same set of
#' repositories.
#'
#' @inheritParams issue_rate_tbl
#' @return A tibble with one row per month: `month`, `n_created`
#' (`window`-month trailing sum of repos created that month). Carries
#' `window` and `source_name` as attributes.
#'
#' @examples
#' \dontrun{
#' rc <- repo_creation_tbl (issue_authors_tbl, repo_tbl, "pypi")
#' }
#' @export
repo_creation_tbl <- function (issue_authors_tbl,
                               repo_tbl,
                               source_name,
                               window = 12L,
                               date_start = as.Date ("2015-01-01"),
                               date_end = NULL) {

    # rm no visible binding notes
    source <- repo_url <- .data <- month <- metric_val <-
        repo_created_at <- n_created <- NULL

    if (is.null (date_end)) {
        date_end <- floor_month (Sys.Date ())
    }

    metric_col <- unname (POPULARITY_METRIC [source_name])
    if (is.na (metric_col)) {
        stop ("Unknown source: ", source_name, call. = FALSE)
    }

    months <- seq (date_start, date_end, by = "month")

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

    created_counts <- repos |>
        dplyr::filter (
            repo_created_at >= date_start, repo_created_at <= date_end
        ) |>
        dplyr::count (month = repo_created_at, name = "n_created")

    result <- tibble::tibble (month = months) |>
        dplyr::left_join (created_counts, by = "month") |>
        dplyr::mutate (n_created = dplyr::coalesce (n_created, 0L)) |>
        dplyr::arrange (month) |>
        dplyr::mutate (n_created = trailing_roll_sum (n_created, window))

    attr (result, "window") <- window
    attr (result, "source_name") <- source_name

    result
}
