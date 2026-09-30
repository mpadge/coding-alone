# Fetches pull-request-author data (github_pr_authors(), for many repos,
# batched and checkpointed to disk. update_pr_authors() then refreshes
# that checkpoint, re-fetching only what's changed per repo since its
# `last_updated` value.
#
# Pull-request data is fetched via GraphQL, exactly as issue data is in
# github-issues.R (see the note there on why, and on the `contribution`
# score). One difference: the `pullRequests` connection has no
# `filterBy: {since: ...}` argument like `issues` does, so `since` is instead
# applied by ordering PRs by `UPDATED_AT` descending and paging only until
# reaching one last updated before `since`.

#' GraphQL query for one page of a repo's pull requests (creator login,
#' creation and last-update timestamps, and comment count), plus the repo's
#' own creation timestamp. Ordered by most recently updated first, so that
#' paging can stop early when refreshing (see `github_repo_prs_graphql()`).
#' @noRd
build_prs_query <- function (owner, repo, cursor = NULL) {

    after <- if (is.null (cursor)) {
        ""
    } else {
        stringr::str_glue (', after: "{cursor}"')
    }

    first <- github_issues_page_size ()

    stringr::str_glue (
        'query {{
            repository(owner: "{owner}", name: "{repo}") {{
                createdAt
                pullRequests(first: {first}{after}, orderBy: {{field: UPDATED_AT, direction: DESC}}) {{
                    pageInfo {{ hasNextPage endCursor }}
                    nodes {{ number createdAt updatedAt author {{ login }} comments {{ totalCount }} }}
                }}
            }}
        }}'
    )
}

#' Every pull request opened against a single GitHub repo, with the opener's
#' login, creation timestamp and comment count, plus the repo's own GitHub
#' creation timestamp. Pages via GraphQL cursors until `hasNextPage` is
#' `FALSE`, or, if `since` is given, until a PR last updated before `since`
#' is reached.
#' @param since If not `NULL`, an ISO-8601 timestamp string restricting
#' results to pull requests updated at or after that time.
#' @return A list with `repo_created_at` (an ISO-8601 timestamp string) and
#' `prs` (a tibble with `pr_number`, `author`, `created_at`, `n_comments`).
#' @noRd
github_repo_prs_graphql <- function (owner, repo, since = NULL) {

    cursor <- NULL
    repo_created_at <- NULL
    pages <- list ()
    single_page_only <- identical (Sys.getenv ("PEERREVIEW_TESTS"), "true")

    repeat {

        body <- gh::gh_gql (build_prs_query (owner, repo, cursor))
        node <- body$data$repository
        if (is.null (repo_created_at)) {
            repo_created_at <- node$createdAt
        }

        pr_nodes <- node$pullRequests$nodes
        reached_since <- FALSE
        if (!is.null (since) && length (pr_nodes) > 0) {
            # ISO-8601 UTC timestamps compare correctly as strings
            updated <- purrr::map_chr (pr_nodes, "updatedAt")
            reached_since <- any (updated < since)
            pr_nodes <- pr_nodes [updated >= since]
        }

        if (length (pr_nodes) > 0) {
            pages [[length (pages) + 1]] <- tibble::tibble (
                pr_number = purrr::map_int (pr_nodes, "number"),
                author = purrr::map_chr (
                    pr_nodes,
                    purrr::pluck, "author", "login",
                    .default = NA_character_
                ),
                created_at = purrr::map_chr (pr_nodes, "createdAt"),
                n_comments = purrr::map_int (
                    pr_nodes, \ (n) n$comments$totalCount
                )
            )
        }

        if (single_page_only || reached_since ||
            !isTRUE (node$pullRequests$pageInfo$hasNextPage)) {
            break
        }
        cursor <- node$pullRequests$pageInfo$endCursor
    }

    prs <- if (length (pages) == 0) {
        tibble::tibble (
            pr_number = integer (), author = character (),
            created_at = character (), n_comments = integer ()
        )
    } else {
        dplyr::bind_rows (pages)
    }

    list (repo_created_at = repo_created_at, prs = prs)
}

#' Extract every pull request opened against a single GitHub repo, with the
#' opener's handle and a `contribution` score: that handle's fractional share
#' (0-1) of all commits ever landed on the repo's default branch, or 0 if the
#' author isn't a contributor at all (see the note at the top of
#' github-issues.R for why this is a coarser but far cheaper substitute for
#' "was this author already a contributor at the time they opened the pull
#' request"). Also carries each pull request's comment count, and the repo's
#' own GitHub creation timestamp, used elsewhere as the start of a repo's
#' exposure window.
#'
#' @param repo_url A GitHub repo URL, e.g. `"https://github.com/owner/repo"`.
#' @param since If not `NULL`, an ISO-8601 timestamp string restricting the
#' GraphQL fetch to pull requests updated (including new comments, not just
#' newly opened) at or after that time - used to refresh previously-fetched
#' data rather than re-fetching every pull request. The `contributors` REST
#' call has no equivalent filter and is always fetched in full.
#'
#' @return A tibble with one row per pull request: `repo_url`, `pr_number`,
#' `author`, `created_at`, `n_comments`, `contribution`, `repo_created_at`
#' (the repo's own GitHub creation timestamp, repeated on every row), and
#' `last_updated` (this fetch's own timestamp, repeated on every row).
#'
#' @examples
#' \dontrun{
#' pr_authors <- github_pr_authors ("https://github.com/ropensci/targets")
#' }
#' @export
github_pr_authors <- function (repo_url = NULL, since = NULL) {

    pr_number <- contribution <- NULL # rm no visible binding notes

    repo <- parse_github_repo_url (repo_url)

    contributors <- github_repo_contributors (repo$owner, repo$repo)
    result <- github_repo_prs_graphql (repo$owner, repo$repo, since)

    last_updated <- strftime (Sys.time (), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

    if (nrow (result$prs) == 0) {
        return (tibble::tibble (
            repo_url = character (),
            pr_number = integer (),
            author = character (),
            created_at = character (),
            n_comments = integer (),
            contribution = double (),
            repo_created_at = character (),
            last_updated = character ()
        ))
    }

    result$prs |>
        dplyr::left_join (contributors, by = c (author = "login")) |>
        dplyr::mutate (contribution = dplyr::coalesce (contribution, 0)) |>
        dplyr::mutate (repo_url = repo_url, .before = pr_number) |>
        dplyr::mutate (
            repo_created_at = result$repo_created_at,
            last_updated = last_updated
        )
}

#' Read the `pr-authors.csv` checkpoint file written by
#' `fetch_pr_authors()`, falling back to an empty tibble with the same
#' schema if it hasn't been written yet.
#'
#' @param out_dir Directory holding `pr-authors.csv`.
#' @return A tibble with columns `repo_url`, `pr_number`, `author`,
#' `created_at`, `n_comments`, `contribution`, `repo_created_at`,
#' `last_updated`.
#' @noRd
read_pr_authors_data <- function (out_dir) {

    pr_authors_csv <- file.path (out_dir, "pr-authors.csv")

    col_types <- readr::cols (
        repo_url = readr::col_character (),
        pr_number = readr::col_integer (),
        author = readr::col_character (),
        created_at = readr::col_character (),
        n_comments = readr::col_integer (),
        contribution = readr::col_double (),
        repo_created_at = readr::col_character (),
        last_updated = readr::col_character ()
    )

    if (file.exists (pr_authors_csv)) {
        out <- readr::read_csv (pr_authors_csv, col_types = col_types)
    } else {
        header <- paste (names (col_types$cols), collapse = ",")
        out <- tibble::as_tibble (
            readr::read_csv (I (header), col_types = col_types)
        )
    }

    return (out)
}

#' Fetch pull-request-author data (`github_pr_authors()`, for many repos,
#' with intermediate batches dumped to disk.
#'
#' Completed repos are tracked in `<out_dir>/pr-authors-done.rds`, and that
#' file is read on re-start. Each batch is fetched concurrently via
#' `progressify`/`futurize`).
#'
#' Batches are drawn via `interlace_for_even_coverage()` rather than
#' sequentially, so that the fetched subsample stays evenly spread across
#' `repo_urls`'s intrinsic order.
#'
#' @param repo_urls Character vector of repo URLs to fetch pull-request authors
#' for. Assumed already ordered by priority if it matters which get fetched
#' first (see `interlace_for_even_coverage()`).
#' @param out_dir Directory to read/write the CSV + done-list checkpoint files.
#' @param batch_size Repos fetched (concurrently) per checkpoint.
#' @return A tibble with columns `repo_url`, `pr_number`, `author`,
#' `created_at`, `n_comments`, `contribution`, `repo_created_at`,
#' `last_updated` - the full accumulated result, including rows from any
#' previous run(s).
#'
#' @examples
#' repo_urls <- c (
#'     "https://github.com/ropensci/targets",
#'     "https://github.com/ropensci/drake"
#' )
#' \dontrun{
#' pr_authors_tbl <- fetch_pr_authors (repo_urls, "path/to/repo-data-out")
#' }
#' @export
fetch_pr_authors <- function (repo_urls, out_dir, batch_size = 50L) {

    is_test_env <- identical (Sys.getenv ("PEERREVIEW_TESTS"), "true")

    if (!is_test_env) {
        requireNamespace ("progressify", quietly = TRUE)
        requireNamespace ("futurize", quietly = TRUE, warn.conflicts = FALSE)
        progressr::handlers (global = TRUE)
    }

    pr_authors_csv <- file.path (out_dir, "pr-authors.csv")
    pr_authors_done_rds <- file.path (out_dir, "pr-authors-done.rds")

    pr_authors_tbl <- read_pr_authors_data (out_dir)

    repo_urls_done <- if (file.exists (pr_authors_done_rds)) {
        readRDS (pr_authors_done_rds)
    } else {
        character ()
    }

    repo_urls <- unique (repo_urls)
    repo_urls_todo <- setdiff (repo_urls, repo_urls_done)
    n_done <- length (repo_urls_done)
    n_total <- length (repo_urls)
    n_todo <- length (repo_urls_todo)
    msg <- stringr::str_glue (
        "PR authors: {n_done} of {n_total} repos already done, ",
        "{n_todo} remaining..."
    )
    cli::cli_alert_info (msg)

    get_pr_authors_safe <- function (repo_url) {
        tryCatch (
            github_pr_authors (repo_url),
            error = function (e) {
                cli::cli_alert_warning (
                    "PR authors: failed for {repo_url}: {conditionMessage (e)}"
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
            "PR authors: batch {b}/{length (batches)} ",
            "({length (batch)} repos)..."
        )
        cli::cli_alert_info (msg)

        if (is_test_env) {
            batch_tbl <- lapply (batch, get_pr_authors_safe)
        } else {
            batch_tbl <- lapply (batch, get_pr_authors_safe) |>
                progressify::progressify () |>
                futurize::futurize ()
        }

        pr_authors_tbl <- dplyr::bind_rows (pr_authors_tbl, batch_tbl)
        repo_urls_done <- c (repo_urls_done, batch)

        readr::write_csv (pr_authors_tbl, pr_authors_csv)
        saveRDS (repo_urls_done, pr_authors_done_rds)
    }

    msg <- stringr::str_glue (
        "PR authors: wrote {nrow(pr_authors_tbl)} rows to ",
        "{pr_authors_csv}"
    )
    cli::cli_alert_success (msg)

    pr_authors_tbl
}

#' Refresh previously-fetched pull-request-author data
#' (`fetch_pr_authors()`'s checkpoint), for repos that have already been
#' fetched at least once.
#'
#' Unlike `fetch_pr_authors()`, which skips any repo already marked done,
#' this re-fetches every repo in `repo_urls`, but each repo's own
#' `github_pr_authors()` call is scoped with `since` set to that repo's
#' most recent `last_updated` value already on disk - so GitHub only returns
#' pull requests that are new, or that changed (e.g. picked up new comments)
#' since that time. A repo not yet present in the checkpoint is fetched in
#' full, exactly as `fetch_pr_authors()` would. Rows returned for an
#' already-known pull request replace the stale row; all other existing rows
#' are left untouched.
#'
#' Because each repo's `since` cursor advances every time it's refreshed,
#' this is safe to re-run (e.g. from a scheduled job) without any separate
#' "done" checkpoint: a run interrupted partway simply leaves the
#' not-yet-reached repos with an older `last_updated`, picked up as normal
#' on the next call. Batches are drawn via `interlace_for_even_coverage()`
#' rather than sequentially, so an interrupted run leaves progress spread
#' across `repo_urls` rather than concentrated at the top.
#'
#' @param repo_urls Character vector of repo URLs to refresh. Repos not
#' already present in the `out_dir` checkpoint are fetched in full.
#' @param out_dir Directory holding the `pr-authors.csv` checkpoint
#' written/read by `fetch_pr_authors()`/`read_pr_authors_data()`.
#' @param batch_size Repos refreshed (concurrently) per checkpoint write.
#' @return A tibble with columns `repo_url`, `pr_number`, `author`,
#' `created_at`, `n_comments`, `contribution`, `repo_created_at`,
#' `last_updated` - the full accumulated result, with refreshed repos'
#' rows brought up to date.
#'
#' @examples
#' repo_urls <- c (
#'     "https://github.com/ropensci/targets",
#'     "https://github.com/ropensci/drake"
#' )
#' \dontrun{
#' pr_authors_tbl <- update_pr_authors (repo_urls, "path/to/repo-data-out")
#' }
#' @export
update_pr_authors <- function (repo_urls, out_dir, batch_size = 50L) {

    repo_url <- since <- last_updated <- NULL # rm no visible binding notes

    is_test_env <- identical (Sys.getenv ("PEERREVIEW_TESTS"), "true")

    if (!is_test_env) {
        requireNamespace ("progressify", quietly = TRUE)
        requireNamespace ("futurize", quietly = TRUE, warn.conflicts = FALSE)
        progressr::handlers (global = TRUE)
    }

    pr_authors_csv <- file.path (out_dir, "pr-authors.csv")

    pr_authors_tbl <- read_pr_authors_data (out_dir)

    repo_urls <- unique (repo_urls)
    n_total <- length (repo_urls)
    msg <- stringr::str_glue ("PR authors: updating {n_total} repos...")
    cli::cli_alert_info (msg)

    since_by_repo <- if (nrow (pr_authors_tbl) == 0) {
        tibble::tibble (repo_url = character (), since = character ())
    } else {
        pr_authors_tbl |>
            dplyr::filter (repo_url %in% repo_urls) |>
            dplyr::group_by (repo_url) |>
            dplyr::summarise (since = max (last_updated), .groups = "drop")
    }

    get_pr_authors_update_safe <- function (repo_url) {
        since <- since_by_repo$since [match (repo_url, since_by_repo$repo_url)]
        if (is.na (since)) since <- NULL
        tryCatch (
            github_pr_authors (repo_url, since = since),
            error = function (e) {
                cli::cli_alert_warning (
                    "PR authors: update failed for {repo_url}: {conditionMessage (e)}"
                )
                tibble::tibble (repo_url = repo_url) [0, ]
            }
        )
    }

    n_batches <- ceiling (length (repo_urls) / batch_size)
    batches <- interlace_for_even_coverage (repo_urls, n_batches)

    for (b in seq_along (batches)) {

        batch <- batches [[b]]
        msg <- stringr::str_glue (
            "PR authors: update batch {b}/{length (batches)} ",
            "({length (batch)} repos)..."
        )
        cli::cli_alert_info (msg)

        if (is_test_env) {
            batch_tbl <- lapply (batch, get_pr_authors_update_safe)
        } else {
            batch_tbl <- lapply (batch, get_pr_authors_update_safe) |>
                progressify::progressify () |>
                futurize::futurize ()
        }

        batch_tbl <- dplyr::bind_rows (batch_tbl)

        if (nrow (batch_tbl) > 0) {
            pr_authors_tbl <- pr_authors_tbl |>
                dplyr::anti_join (
                    batch_tbl,
                    by = c ("repo_url", "pr_number")
                ) |>
                dplyr::bind_rows (batch_tbl)
        }

        readr::write_csv (pr_authors_tbl, pr_authors_csv)
    }

    msg <- stringr::str_glue (
        "PR authors: update wrote {nrow(pr_authors_tbl)} rows to ",
        "{pr_authors_csv}"
    )
    cli::cli_alert_success (msg)

    pr_authors_tbl
}
