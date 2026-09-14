# Functions backing the README.Rmd "Analyses" section: combining the
# per-source output tables into one, then fetching and attaching GitHub
# issue-author data for every repo in that combined table.

# ---- combined repo table ---------------------------------------------------

#' Combine the PyPI/npm/JOSS/rOpenSci output tables in `out_dir` into a
#' single table of names, GitHub URLs, and download/star metrics, tagged
#' with their `source`. Files are matched by exact basename, so anything
#' else written to `out_dir` (e.g. the issue-authors output below) is
#' ignored rather than breaking the read.
#'
#' @param out_dir Directory holding the source CSVs.
#' @return A tibble with columns `name`, `repo_url`, `downloads`, `stars`,
#' `source`.
#'
#' @examples
#' \dontrun{
#' repo_tbl <- build_repo_tbl ("path/to/repo-data-out")
#' }
#' @export
build_repo_tbl <- function (out_dir) {

    source_patterns <- c (
        ropensci = "ropensci.csv",
        joss = "joss.csv",
        cran = "cran.csv",
        pypi = "pypi.csv",
        npm = "npm.csv"
    )

    source_for_file <- function (path) {
        fname <- fs::path_file (path)
        hit <- fname == source_patterns
        names (source_patterns) [hit]
    }

    read_one <- function (path) {

        tbl <- readr::read_csv (path, show_col_types = FALSE, progress = FALSE)
        name_col <- intersect (c ("name", "package", "title"), names (tbl)) [1]

        name <- tbl [[name_col]]
        if (name_col == "title") {
            name <- sub ("^\\[REVIEW\\]:\\s*", "", name)
        }

        tibble::tibble (
            name = name,
            repo_url = tbl$repo_url,
            downloads = if ("downloads" %in% names (tbl)) {
                tbl$downloads
            } else {
                NA_integer_
            },
            stars = if ("stars" %in% names (tbl)) tbl$stars else NA_integer_,
            source = source_for_file (path)
        )
    }

    flist <- fs::dir_ls (out_dir, glob = "*.csv")
    flist <- flist [lengths (lapply (flist, source_for_file)) > 0]

    purrr::map_dfr (flist, read_one)
}

# ---- issue authors ----------------------------------------------------------

ISSUE_AUTHORS_COL_TYPES <- readr::cols (
    repo_url = readr::col_character (),
    issue_number = readr::col_integer (),
    author = readr::col_character (),
    created_at = readr::col_character (),
    n_comments = readr::col_integer (),
    contribution = readr::col_double (),
    repo_created_at = readr::col_character ()
)

#' Split a vector into `n_batches` groups so that fetching group 1, then
#' group 2, etc. gives even coverage of the whole vector at any stopping
#' point, rather than exhausting one end of it first. Assumes `x` arrives
#' already prioritised (e.g. `repo_tbl`'s PyPI/npm rows, built by
#' `build_working_sample()` as a deterministic head of the most-downloaded
#' packages followed by a random tail).
#'
#' @param x Vector already ordered by priority (highest first).
#' @param n_batches Number of interleaved groups to split `x` into - in
#' practice `fetch_issue_authors()`'s number of batches, so each
#' checkpointed batch is itself one such group.
#' @return A list of `n_batches` groups (as from `split()`), each an evenly
#' spread subsample of `x`, in group order.
#' @noRd
interlace_for_even_coverage <- function (x, n_batches) {

    if (n_batches <= 1 || length (x) == 0) {
        return (list (x))
    }

    split (x, rep (seq_len (n_batches), length.out = length (x)))
}

#' Fetch issue-author data (`github_issue_authors()`, for many repos,
#' batched and checkpointed to disk.
#'
#' Repos already recorded as done (in `<out_dir>/issue-authors-done.rds`,
#' tracked independently of row count so a repo with zero issues isn't retried
#' forever) are skipped, so re-running after an interruption - rate-limited or
#' otherwise - picks up where it left off rather than starting over. Each
#' batch's repos are fetched concurrently (via `progressify`/`futurize`);
#' GitHub's hourly rate limit is a cumulative budget rather than a burst limit,
#' so the risk is running through it too fast overall, not concurrency within
#' one batch - hence checkpointing after every batch rather than throttling
#' within one.
#'
#' Batches are drawn via `interlace_for_even_coverage()` rather than taken
#' sequentially off `repo_urls`, so that however far fetching gets before
#' stopping, the fetched subsample stays evenly spread across `repo_urls`'s own
#' order instead of silently favouring whatever came first in it.
#'
#' @param repo_urls Character vector of repo URLs to fetch issue authors for.
#' Assumed already ordered by priority if it matters which get fetched
#' first (see `interlace_for_even_coverage()`).
#' @param out_dir Directory to read/write the CSV + done-list checkpoint files.
#' @param batch_size Repos fetched (concurrently) per checkpoint.
#' @return A tibble with columns `repo_url`, `issue_number`, `author`,
#' `created_at`, `n_comments`, `contribution`, `repo_created_at` - the full
#' accumulated result, including rows from any previous run(s).
#'
#' @examples
#' repo_urls <- c (
#'     "https://github.com/ropensci/targets",
#'     "https://github.com/ropensci/drake"
#' )
#' \dontrun{
#' issue_authors_tbl <- fetch_issue_authors (repo_urls, "path/to/repo-data-out")
#' }
#' @export
fetch_issue_authors <- function (repo_urls, out_dir, batch_size = 50L) {

    is_test_env <- identical (Sys.getenv ("PEERREVIEW_TESTS"), "true")

    if (!is_test_env) {
        requireNamespace ("progressify", quietly = TRUE)
        requireNamespace ("futurize", quietly = TRUE)
        progressr::handlers (global = TRUE)
    }

    issue_authors_csv <- file.path (out_dir, "issue-authors.csv")
    issue_authors_done_rds <- file.path (out_dir, "issue-authors-done.rds")

    issue_authors_tbl <- if (file.exists (issue_authors_csv)) {

        readr::read_csv (issue_authors_csv, col_types = ISSUE_AUTHORS_COL_TYPES)

    } else {

        tibble::tibble (
            repo_url = character (), issue_number = integer (),
            author = character (), created_at = character (),
            n_comments = integer (),
            contribution = double (),
            repo_created_at = character ()
        )
    }

    repo_urls_done <- if (file.exists (issue_authors_done_rds)) {
        readRDS (issue_authors_done_rds)
    } else {
        character ()
    }

    repo_urls <- unique (repo_urls)
    repo_urls_todo <- setdiff (repo_urls, repo_urls_done)
    n_done <- length (repo_urls_done)
    n_total <- length (repo_urls)
    n_todo <- length (repo_urls_todo)
    msg <- stringr::str_glue (
        "Issue authors: {n_done} of {n_total} repos already done, ",
        "{n_todo} remaining..."
    )
    cli::cli_alert_info (msg)

    get_issue_authors_safe <- function (repo_url) {
        tryCatch (
            github_issue_authors (repo_url),
            error = function (e) {
                cli::cli_alert_warning (
                    "Issue authors: failed for {repo_url}: {conditionMessage (e)}"
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
            "Issue authors: batch {b}/{length (batches)} ",
            "({length (batch)} repos)..."
        )
        cli::cli_alert_info (msg)

        if (is_test_env) {
            batch_tbl <- lapply (batch, get_issue_authors_safe)
        } else {
            batch_tbl <- lapply (batch, get_issue_authors_safe) |>
                progressify::progressify () |>
                futurize::futurize ()
        }

        issue_authors_tbl <- dplyr::bind_rows (issue_authors_tbl, batch_tbl)
        repo_urls_done <- c (repo_urls_done, batch)

        readr::write_csv (issue_authors_tbl, issue_authors_csv)
        saveRDS (repo_urls_done, issue_authors_done_rds)
    }

    msg <- stringr::str_glue (
        "Issue authors: wrote {nrow(issue_authors_tbl)} rows to ",
        "{issue_authors_csv}"
    )
    cli::cli_alert_success (msg)

    issue_authors_tbl
}

#' Left-join `repo_tbl`'s per-repo metadata (name, downloads, stars, source)
#' onto an issue-authors table by `repo_url`, after deduplicating `repo_tbl`
#' on `repo_url` so a repo appearing under multiple sources doesn't fan out
#' the join.
#'
#' @inheritParams issue_rate_tbl
#' @return `issue_authors_tbl` with `repo_tbl`'s columns attached.
#'
#' @examples
#' issue_authors_tbl <- tibble::tibble (
#'     repo_url = c (
#'         "https://github.com/org/pkg1", "https://github.com/org/pkg2"
#'     ),
#'     issue_number = c (1L, 1L)
#' )
#' repo_tbl <- tibble::tibble (
#'     repo_url = c (
#'         "https://github.com/org/pkg1", "https://github.com/org/pkg2"
#'     ),
#'     name = c ("pkg1", "pkg2"),
#'     downloads = c (100, 200),
#'     stars = c (5, 10),
#'     source = c ("pypi", "npm")
#' )
#' join_repo_metadata (issue_authors_tbl, repo_tbl)
#' @export
join_repo_metadata <- function (issue_authors_tbl, repo_tbl) {

    # suppress no visible binding notes:
    repo_url <- NULL

    repo_tbl_unique <- dplyr::distinct (repo_tbl, repo_url, .keep_all = TRUE)
    dplyr::left_join (issue_authors_tbl, repo_tbl_unique, by = "repo_url")
}

# ---- commit counts ----------------------------------------------------------

# GitHub's GraphQL API has no endpoint that hands back a pre-binned
# month-by-month commit histogram in one call. What it does have is
# `history(since:, until:)` on a `Commit` object (reached via a branch ref's
# `target`), whose `totalCount` field counts commits reachable from that
# target within the window - without ever paging through or returning the
# commits themselves. That makes it cheap to call once per month of a
# repo's history, even though it can't be done in a single request.

#' GraphQL query for a repo's own creation timestamp only - used to bound
#' the monthly loop in `github_commit_counts_by_month()` below to months the
#' repo actually existed in, without wasting a query on months before it was
#' created.
#' @noRd
build_repo_created_at_query <- function (owner, repo) {
    stringr::str_glue (
        'query {{
            repository(owner: "{owner}", name: "{repo}") {{
                createdAt
            }}
        }}'
    )
}

#' GraphQL query for the number of commits landed on a repo's default
#' branch within `[since, until)`. `since`/`until` are GitTimestamp strings
#' (ISO-8601, e.g. `"2020-01-01T00:00:00Z"`).
#' @noRd
build_commit_count_query <- function (owner, repo, since, until) {
    stringr::str_glue (
        'query {{
            repository(owner: "{owner}", name: "{repo}") {{
                defaultBranchRef {{
                    target {{
                        ... on Commit {{
                            history(since: "{since}", until: "{until}") {{
                                totalCount
                            }}
                        }}
                    }}
                }}
            }}
        }}'
    )
}

#' Number of commits on a single repo's default branch within `[since,
#' until)`. Returns 0 for a repo with no default branch at all (an empty
#' repo), rather than erroring.
#' @noRd
github_repo_commit_count <- function (owner, repo, since, until) {

    body <- gh::gh_gql (build_commit_count_query (owner, repo, since, until))
    history <- purrr::pluck (
        body, "data", "repository", "defaultBranchRef", "target", "history"
    )
    if (is.null (history)) {
        return (0L)
    }
    as.integer (history$totalCount)
}

#' Monthly commit counts on a single GitHub repo's default branch, as a
#' direct measure of code-activity to sit alongside the issue-based measures
#' elsewhere in this package. One GraphQL query per calendar month in range
#' (bounded below by the repo's own creation date, capped above by
#' `date_end`) - there's no way to ask for all months in a single request,
#' but each query only ever asks for a `totalCount`, so the cost per query
#' stays low regardless of how many commits actually landed that month.
#'
#' @param repo_url A GitHub repo URL, e.g. `"https://github.com/owner/repo"`.
#' @param date_start,date_end Date bounds on the monthly sequence;
#' `date_end` defaults to the start of the current month. Months before the
#' repo's own creation date are skipped rather than queried and discarded.
#'
#' @return A tibble with one row per month: `repo_url`, `month`,
#' `n_commits`. Zero rows if the repo has no commits in range (including a
#' repo created after `date_end`).
#'
#' @examples
#' \dontrun{
#' commits <- github_commit_counts_by_month ("https://github.com/ropensci/targets")
#' }
#' @export
github_commit_counts_by_month <- function (repo_url = NULL,
                                           date_start = as.Date ("2015-01-01"),
                                           date_end = NULL) {

    if (is.null (date_end)) date_end <- floor_month (Sys.Date ())

    empty <- tibble::tibble (
        repo_url = character (), month = as.Date (character ()),
        n_commits = integer ()
    )

    repo <- parse_github_repo_url (repo_url)

    created_at_body <- gh::gh_gql (
        build_repo_created_at_query (repo$owner, repo$repo)
    )
    repo_created_at <- created_at_body$data$repository$createdAt
    if (is.null (repo_created_at)) {
        return (empty)
    }

    start <- max (date_start, floor_month (repo_created_at))
    if (start > date_end) {
        return (empty)
    }

    n_months <- length (seq (start, date_end, by = "month"))
    bounds <- seq (start, by = "month", length.out = n_months + 1)
    months <- utils::head (bounds, -1)
    month_ends <- utils::tail (bounds, -1)

    to_git_timestamp <- \ (d) strftime (d, "%Y-%m-%dT00:00:00Z", tz = "UTC")

    n_commits <- purrr::map2_int (months, month_ends, \ (since, until) {
        github_repo_commit_count (
            repo$owner, repo$repo,
            since = to_git_timestamp (since),
            until = to_git_timestamp (until)
        )
    })

    tibble::tibble (repo_url = repo_url, month = months, n_commits = n_commits)
}
