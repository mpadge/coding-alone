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
            downloads = if ("downloads" %in% names (tbl)) tbl$downloads else NA_integer_,
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

#' Fetch issue-author data (`github_issue_authors()`, which now also
#' returns each repo's own GitHub creation timestamp as `repo_created_at` -
#' used elsewhere as the start of a repo's exposure window) for many repos,
#' batched and checkpointed to disk. Repos already recorded as done (in
#' `<out_dir>/issue-authors-done.rds`, tracked independently of row count so
#' a repo with zero issues isn't retried forever) are skipped, so
#' re-running after an interruption - rate-limited or otherwise - picks up
#' where it left off rather than starting over. Each batch's repos are
#' fetched concurrently (via `progressify`/`futurize`); GitHub's hourly rate
#' limit is a cumulative budget rather than a burst limit, so the risk is
#' running through it too fast overall, not concurrency within one batch -
#' hence checkpointing after every batch rather than throttling within one.
#'
#' @param repo_urls Character vector of repo URLs to fetch issue authors for.
#' @param out_dir Directory to read/write the CSV + done-list checkpoint files.
#' @param batch_size Repos fetched (concurrently) per checkpoint.
#' @return A tibble with columns `repo_url`, `issue_number`, `author`,
#' `created_at`, `n_comments`, `contribution`, `repo_created_at` - the full
#' accumulated result, including rows from any previous run(s).
#' @export
fetch_issue_authors <- function (repo_urls, out_dir, batch_size = 50L) {

    is_test_env <- !identical (Sys.getenv ("LONGTAIL_TESTS", "true"), "false")

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
            author = character (), created_at = character (), n_comments = integer (),
            contribution = double (),
            repo_created_at = character ()
        )
    }
    repo_urls_done <- if (file.exists (issue_authors_done_rds)) readRDS (issue_authors_done_rds) else character ()

    repo_urls <- unique (repo_urls)
    repo_urls_todo <- setdiff (repo_urls, repo_urls_done)
    cli::cli_alert_info (
        "Issue authors: {length (repo_urls_done)} of {length (repo_urls)} repos already done, {length (repo_urls_todo)} remaining..."
    )

    get_issue_authors_safe <- function (repo_url) {
        tryCatch (
            github_issue_authors (repo_url),
            error = function (e) {
                cli::cli_alert_warning ("Issue authors: failed for {repo_url}: {conditionMessage (e)}")
                tibble::tibble (repo_url = repo_url) [0, ]
            }
        )
    }

    batches <- split (repo_urls_todo, ceiling (seq_along (repo_urls_todo) / batch_size))
    for (b in seq_along (batches)) {
        batch <- batches [[b]]
        cli::cli_alert_info ("Issue authors: batch {b}/{length (batches)} ({length (batch)} repos)...")

        if (is_test_env) {
            batch_tbl <- lapply (batch, get_issue_authors_safe)
        } else {
            batch_tbl <- batch_tbl |>
                progressify::progressify () |>
                futurize::futurize ()
        }
        issue_authors_tbl <- dplyr::bind_rows (issue_authors_tbl, batch_tbl)
        repo_urls_done <- c (repo_urls_done, batch)

        readr::write_csv (issue_authors_tbl, issue_authors_csv)
        saveRDS (repo_urls_done, issue_authors_done_rds)
    }
    cli::cli_alert_success ("Issue authors: wrote {nrow(issue_authors_tbl)} rows to {issue_authors_csv}")

    issue_authors_tbl
}

#' Left-join `repo_tbl`'s per-repo metadata (name, downloads, stars, source)
#' onto an issue-authors table by `repo_url`, after deduplicating `repo_tbl`
#' on `repo_url` so a repo appearing under multiple sources doesn't fan out
#' the join.
#'
#' @param issue_authors_tbl As returned by `fetch_issue_authors()`.
#' @param repo_tbl As returned by `build_repo_tbl()`.
#' @return `issue_authors_tbl` with `repo_tbl`'s columns attached.
#' @export
join_repo_metadata <- function (issue_authors_tbl, repo_tbl) {
    repo_tbl_unique <- dplyr::distinct (repo_tbl, repo_url, .keep_all = TRUE)
    dplyr::left_join (issue_authors_tbl, repo_tbl_unique, by = "repo_url")
}
