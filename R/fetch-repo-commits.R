#' GraphQL query for one page of a repo's default-branch commit history within
#' `[since, until)`, extracting only for each commit's `committedDate`.
#' @noRd
build_commit_history_query <- function (owner, repo, since, until,
                                        cursor = NULL) {

    after <- if (is.null (cursor)) {
        ""
    } else {
        stringr::str_glue (', after: "{cursor}"')
    }

    first <- github_issues_page_size ()

    stringr::str_glue (
        'query {{
            repository(owner: "{owner}", name: "{repo}") {{
                defaultBranchRef {{
                    target {{
                        ... on Commit {{
                            history(since: "{since}", until: "{until}", first: {first}{after}) {{
                                pageInfo {{ hasNextPage endCursor }}
                                nodes {{ committedDate }}
                            }}
                        }}
                    }}
                }}
            }}
        }}'
    )
}

#' Timestamps of every commit landed on a repo's default branch within
#' `[since, until)`. Returns `character(0)` for a repos with no default branch.
#' @noRd
github_repo_commit_dates <- function (owner, repo, since, until) {

    cursor <- NULL
    pages <- list ()
    single_page_only <- identical (Sys.getenv ("PEERREVIEW_TESTS"), "true")

    repeat {

        body <- gh::gh_gql (
            build_commit_history_query (owner, repo, since, until, cursor)
        )
        history <- purrr::pluck (
            body, "data", "repository", "defaultBranchRef", "target", "history"
        )
        if (is.null (history)) {
            return (character ())
        }

        nodes <- history$nodes
        if (length (nodes) > 0) {
            pages [[length (pages) + 1]] <- purrr::map_chr (
                nodes, "committedDate"
            )
        }

        if (single_page_only || !isTRUE (history$pageInfo$hasNextPage)) {
            break
        }
        cursor <- history$pageInfo$endCursor
    }

    unlist (pages, use.names = FALSE)
}

#' Monthly commit counts on a single GitHub repo's default branch.
#'
#' This is meant to run after issue-author data has already been fetched
#' (`fetch_issue_authors()`).
#'
#' @param repo_url A GitHub repo URL, e.g. `"https://github.com/owner/repo"`.
#' @param repo_created_at The repo's own creation timestamp (as recorded in
#' `issue-authors.csv`'s `repo_created_at` column), used to raise
#' `date_start` up to the month the repo actually came into existence.
#' `NULL` defaults to `date_start .
#' @param date_start,date_end Date bounds on the monthly sequence, raised to
#' the repo's creation month if `repo_created_at` is later; `date_end`
#' defaults to the start of the current month.
#'
#' @return A tibble with one row per month: `repo_url`, `month`, `n_commits`,
#' trimmed of any leading/trailing zero-commit months. Zero rows if the repo's
#' creation month is after `date_end`, or it has no commits at all in range.
#'
#' @examples
#' \dontrun{
#' commits <- github_commit_counts_by_month ("https://github.com/ropensci/targets")
#' }
#' @export
github_commit_counts_by_month <- function (repo_url = NULL,
                                           repo_created_at = NULL,
                                           date_start = as.Date ("2015-01-01"),
                                           date_end = NULL) {

    month <- n_commits <- NULL # rm no visible binding notes

    if (is.null (date_end)) date_end <- floor_month (Sys.Date ())

    start <- date_start
    if (!is.null (repo_created_at) && !is.na (repo_created_at)) {
        start <- max (date_start, floor_month (repo_created_at))
    }

    if (start > date_end) {
        return (tibble::tibble (
            repo_url = character (), month = as.Date (character ()),
            n_commits = integer ()
        ))
    }

    repo <- parse_github_repo_url (repo_url)

    to_git_timestamp <- \ (d) strftime (d, "%Y-%m-%dT00:00:00Z", tz = "UTC")
    until <- seq (date_end, by = "month", length.out = 2) [2]

    commit_dates <- github_repo_commit_dates (
        repo$owner, repo$repo,
        since = to_git_timestamp (start),
        until = to_git_timestamp (until)
    )

    months <- seq (start, date_end, by = "month")

    counts_tbl <- tibble::tibble (month = floor_month (commit_dates)) |>
        dplyr::count (month, name = "n_commits")

    out <- tibble::tibble (repo_url = repo_url, month = months) |>
        dplyr::left_join (counts_tbl, by = "month") |>
        dplyr::mutate (n_commits = as.integer (dplyr::coalesce (n_commits, 0)))

    nonzero <- which (out$n_commits > 0)
    if (length (nonzero) == 0) {
        return (out [0, ])
    }
    out [seq (min (nonzero), max (nonzero)), ]
}

COMMIT_COUNTS_COL_TYPES <- readr::cols (
    repo_url = readr::col_character (),
    month = readr::col_date (),
    n_commits = readr::col_integer ()
)

#' Fetch monthly commit counts (`github_commit_counts_by_month()`) for many
#' repos, batched like `fetch_issue_authors()`.
#'
#' Assumes `fetch_issue_authors()` has already been run, so each repo's
#' creation timestamp can be read straight out of its `issue-authors.csv`
#' rather than fetched again here. A repo missing from that file defaults
#' back to `date_start`.
#'
#' @inheritParams fetch_issue_authors
#' @param date_start,date_end Passed to `github_commit_counts_by_month()`.
#' @return A tibble with columns `repo_url`, `month`, `n_commits` - the full
#' accumulated result, including rows from any previous run(s).
#'
#' @examples
#' repo_urls <- c (
#'     "https://github.com/ropensci/targets",
#'     "https://github.com/ropensci/drake"
#' )
#' \dontrun{
#' commit_counts_tbl <- fetch_repo_commits (repo_urls, "path/to/repo-data-out")
#' }
#' @export
fetch_repo_commits <- function (repo_urls, out_dir, batch_size = 50L,
                                date_start = as.Date ("2015-01-01"),
                                date_end = NULL) {

    # rm no visible binding notes:
    repo_url <- repo_created_at <- NULL

    is_test_env <- identical (Sys.getenv ("PEERREVIEW_TESTS"), "true")

    if (!is_test_env) {
        requireNamespace ("progressify", quietly = TRUE)
        requireNamespace ("futurize", quietly = TRUE)
        progressr::handlers (global = TRUE)
    }

    repo_created_at_lookup <- read_issue_authors_data (out_dir) |>
        dplyr::distinct (repo_url, repo_created_at) |>
        tibble::deframe ()

    commit_counts_csv <- file.path (out_dir, "commit-counts.csv")
    commit_counts_done_rds <- file.path (out_dir, "commit-counts-done.rds")

    commit_counts_tbl <- if (file.exists (commit_counts_csv)) {

        readr::read_csv (commit_counts_csv, col_types = COMMIT_COUNTS_COL_TYPES)

    } else {

        tibble::tibble (
            repo_url = character (), month = as.Date (character ()),
            n_commits = integer ()
        )
    }

    repo_urls_done <- if (file.exists (commit_counts_done_rds)) {
        readRDS (commit_counts_done_rds)
    } else {
        character ()
    }

    repo_urls <- unique (repo_urls)
    repo_urls_todo <- setdiff (repo_urls, repo_urls_done)
    n_done <- length (repo_urls_done)
    n_total <- length (repo_urls)
    n_todo <- length (repo_urls_todo)
    msg <- stringr::str_glue (
        "Commit counts: {n_done} of {n_total} repos already done, ",
        "{n_todo} remaining..."
    )
    cli::cli_alert_info (msg)

    get_commit_counts_safe <- function (repo_url) {
        tryCatch (
            github_commit_counts_by_month (
                repo_url,
                repo_created_at = unname (repo_created_at_lookup [repo_url]),
                date_start = date_start,
                date_end = date_end
            ),
            error = function (e) {
                cli::cli_alert_warning (
                    "Commit counts: failed for {repo_url}: {conditionMessage (e)}"
                )
                tibble::tibble (repo_url = repo_url) [0, ]
            }
        )
    }

    n_batches <- ceiling (length (repo_urls_todo) / batch_size)
    batches <- interlace_for_even_coverage (repo_urls_todo, n_batches)

    for (b in seq_along (batches)) {

        batch <- batches [[b]]
        msg <- stringr::str_glue (
            "Commit counts: batch {b}/{length (batches)} ",
            "({length (batch)} repos)..."
        )
        cli::cli_alert_info (msg)

        if (is_test_env) {
            batch_tbl <- lapply (batch, get_commit_counts_safe)
        } else {
            batch_tbl <- lapply (batch, get_commit_counts_safe) |>
                progressify::progressify () |>
                futurize::futurize ()
        }

        commit_counts_tbl <- dplyr::bind_rows (commit_counts_tbl, batch_tbl)
        repo_urls_done <- c (repo_urls_done, batch)

        readr::write_csv (commit_counts_tbl, commit_counts_csv)
        saveRDS (repo_urls_done, commit_counts_done_rds)
    }

    msg <- stringr::str_glue (
        "Commit counts: wrote {nrow (commit_counts_tbl)} rows to ",
        "{commit_counts_csv}"
    )
    cli::cli_alert_success (msg)

    commit_counts_tbl
}
