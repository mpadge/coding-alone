# Mocks github_issue_authors() itself (not any HTTP layer) - the underlying
# GraphQL/REST calls it makes are already covered directly in
# test-github-issues.R. What's under test here is fetch_issue_authors()'s
# own checkpointing/resume logic and per-repo error isolation.

fake_issue_authors_row <- function (repo_url, since = NULL) {
    tibble::tibble (
        repo_url = repo_url, issue_number = 1L, author = "someone",
        created_at = "2020-01-01", n_comments = 1L, contribution = 0,
        repo_created_at = "2020-01-01", last_updated = "2020-01-02"
    )
}

test_that ("fetch_issue_authors writes both checkpoint files on a fresh run", {
    out_dir <- withr::local_tempdir ()
    testthat::local_mocked_bindings (
        github_issue_authors = fake_issue_authors_row,
        .package = "codingAlone"
    )

    res <- fetch_issue_authors (
        c ("https://github.com/o/a", "https://github.com/o/b"), out_dir,
        batch_size = 50L
    )

    expect_identical (nrow (res), 2L)
    expect_true (file.exists (file.path (out_dir, "issue-authors.csv")))
    expect_true (file.exists (file.path (out_dir, "issue-authors-done.rds")))
})

test_that ("fetch_issue_authors resumes, skipping repos already marked done", {
    out_dir <- withr::local_tempdir ()
    calls <- character ()
    testthat::local_mocked_bindings (
        github_issue_authors = function (repo_url) {
            calls <<- c (calls, repo_url) # nolint: undesirable_operator_linter.
            fake_issue_authors_row (repo_url)
        },
        .package = "codingAlone"
    )

    fetch_issue_authors (
        c ("https://github.com/o/a", "https://github.com/o/b"),
        out_dir,
        batch_size = 50L
    )
    calls <- character () # reset before the resumed run
    res2 <- fetch_issue_authors (
        three_gh_urls,
        out_dir,
        batch_size = 50L
    )

    # only the new repo was fetched:
    expect_identical (calls, "https://github.com/o/c")
    # accumulated result includes the earlier run's rows:
    expect_identical (nrow (res2), 3L)
})

test_that ("fetch_issue_authors isolates error without abort", {

    out_dir <- withr::local_tempdir ()
    testthat::local_mocked_bindings (
        github_issue_authors = function (repo_url) {
            if (grepl ("bad", repo_url, fixed = TRUE)) stop ("boom")
            fake_issue_authors_row (repo_url)
        },
        .package = "codingAlone"
    )

    expect_message (
        res <- fetch_issue_authors (
            c ("https://github.com/o/a", "https://github.com/o/bad"), out_dir,
            batch_size = 50L
        ),
        "failed for https://github.com/o/bad"
    )

    expect_identical (nrow (res), 1L) # only the good repo contributed rows
    expect_identical (res$repo_url, "https://github.com/o/a")
})

test_that ("fetch_issue_authors samples evenly across repo_urls, not sequentially from the top", {
    out_dir <- withr::local_tempdir ()
    calls <- character ()
    testthat::local_mocked_bindings (
        github_issue_authors = function (repo_url) {
            calls <<- c (calls, repo_url) # nolint: undesirable_operator_linter.
            fake_issue_authors_row (repo_url)
        },
        .package = "codingAlone"
    )

    # ordered as if by descending popularity, most-popular first:
    repo_urls <- paste0 ("https://github.com/o/r", seq_len (100))

    fetch_issue_authors (repo_urls, out_dir, batch_size = 25L)

    # first batch (first 25 calls) should span the full range, not just the
    # most-popular prefix:
    first_batch_idx <- as.integer (sub (".*/r", "", calls [1:25]))
    expect_true (max (first_batch_idx) > 75)
    # and should be evenly spread (stride 4, since n_batches = 100/25 = 4):
    expect_identical (sort (first_batch_idx), seq (1L, 97L, by = 4L))
})

# ---- update_issue_authors ---------------------------------------------

test_that ("update_issue_authors fetches unknown repos in full, with since = NULL", {
    out_dir <- withr::local_tempdir ()
    sinces <- list ()
    testthat::local_mocked_bindings (
        github_issue_authors = function (repo_url, since = NULL) {
            sinces [[repo_url]] <<- since # nolint: undesirable_operator_linter.
            fake_issue_authors_row (repo_url)
        },
        .package = "codingAlone"
    )

    res <- update_issue_authors ("https://github.com/o/a", out_dir)

    expect_null (sinces [["https://github.com/o/a"]])
    expect_identical (nrow (res), 1L)
})

test_that ("update_issue_authors passes each repo's last_updated as since, and replaces stale rows", {
    out_dir <- withr::local_tempdir ()

    testthat::local_mocked_bindings (
        github_issue_authors = function (repo_url, since = NULL) {
            fake_issue_authors_row (repo_url)
        },
        .package = "codingAlone"
    )
    first <- fetch_issue_authors ("https://github.com/o/a", out_dir)
    expect_identical (first$n_comments, 1L)

    sinces <- list ()
    testthat::local_mocked_bindings (
        github_issue_authors = function (repo_url, since = NULL) {
            sinces [[repo_url]] <<- since # nolint: undesirable_operator_linter.
            tibble::tibble (
                repo_url = repo_url, issue_number = 1L, author = "someone",
                created_at = "2020-01-01", n_comments = 5L, contribution = 0,
                repo_created_at = "2020-01-01", last_updated = "2020-01-03"
            )
        },
        .package = "codingAlone"
    )
    res <- update_issue_authors ("https://github.com/o/a", out_dir)

    expect_identical (sinces [["https://github.com/o/a"]], "2020-01-02")
    # stale row (n_comments = 1) replaced, not duplicated:
    expect_identical (nrow (res), 1L)
    expect_identical (res$n_comments, 5L)
    expect_identical (res$last_updated, "2020-01-03")
})

test_that ("update_issue_authors isolates error without abort", {
    out_dir <- withr::local_tempdir ()
    testthat::local_mocked_bindings (
        github_issue_authors = function (repo_url, since = NULL) {
            if (grepl ("bad", repo_url, fixed = TRUE)) stop ("boom")
            fake_issue_authors_row (repo_url)
        },
        .package = "codingAlone"
    )

    expect_message (
        res <- update_issue_authors (
            c ("https://github.com/o/a", "https://github.com/o/bad"), out_dir
        ),
        "update failed for https://github.com/o/bad"
    )

    expect_identical (nrow (res), 1L)
    expect_identical (res$repo_url, "https://github.com/o/a")
})
