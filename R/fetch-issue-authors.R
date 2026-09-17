# Fetches issue-author data (github_issue_authors(), for many repos,
# batched and checkpointed to disk.

ISSUE_AUTHORS_COL_TYPES <- readr::cols (
    repo_url = readr::col_character (),
    issue_number = readr::col_integer (),
    author = readr::col_character (),
    created_at = readr::col_character (),
    n_comments = readr::col_integer (),
    contribution = readr::col_double (),
    repo_created_at = readr::col_character ()
)

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
