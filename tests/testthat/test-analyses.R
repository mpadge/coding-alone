test_that ("build_repo_tbl combines source CSVs, tagged with source", {

    Sys.setenv ("PEERREVIEW_TESTS" = "true")

    out_dir <- withr::local_tempdir ()
    write_test_analyses_data (out_dir)

    out <- build_repo_tbl (out_dir)

    expect_named (
        out,
        c ("name", "repo_url", "downloads", "stars", "source")
    )
    expect_setequal (out$source, c ("pypi", "npm", "joss"))
    expect_false ("https://github.com/o/ignored" %in% out$repo_url)

    joss_row <- dplyr::filter (out, source == "joss")
    expect_identical (joss_row$name, "Some Tool") # "[REVIEW]: " prefix stripped

    pypi_rows <- dplyr::filter (out, source == "pypi")
    expect_true (all (is.na (pypi_rows$stars))) # no stars column in pypi.csv
})

test_that ("build_repo_tbl returns a emtpy when out_dir has no matches", {
    out_dir <- withr::local_tempdir ()
    readr::write_csv (
        tibble::tibble (x = 1),
        file.path (out_dir, "unrelated.csv")
    )

    out <- build_repo_tbl (out_dir)
    expect_identical (nrow (out), 0L) # no source file matched, so no rows read
})

# ---- fetch_issue_authors ----------------------------------------------------
#
# Mocks github_issue_authors() itself (not any HTTP layer) - the underlying
# GraphQL/REST calls it makes are already covered directly in
# test-github-issues.R. What's under test here is fetch_issue_authors()'s
# own checkpointing/resume logic and per-repo error isolation.

fake_issue_authors_row <- function (repo_url) {
    tibble::tibble (
        repo_url = repo_url, issue_number = 1L, author = "someone",
        created_at = "2020-01-01", n_comments = 1L, contribution = 0,
        repo_created_at = "2020-01-01"
    )
}

test_that ("fetch_issue_authors writes both checkpoint files on a fresh run", {
    out_dir <- withr::local_tempdir ()
    testthat::local_mocked_bindings (
        github_issue_authors = fake_issue_authors_row,
        .package = "peerreview"
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
        .package = "peerreview"
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
        .package = "peerreview"
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
        .package = "peerreview"
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

test_that ("join_repo_metadata attaches repo_tbl columns by repo_url", {
    issue_authors_tbl <- tibble::tibble (
        repo_url = three_gh_urls,
        issue_number = c (1L, 2L, 1L)
    )
    # repo_tbl has "a" listed under two sources - should not fan out the join
    repo_tbl <- tibble::tibble (
        name = c ("a-pypi", "a-npm", "b"),
        repo_url = three_gh_urls,
        downloads = c (10, 20, 30),
        stars = c (NA, NA, 5),
        source = c ("pypi", "npm", "joss")
    )

    out <- join_repo_metadata (issue_authors_tbl, repo_tbl)

    # unchanged, no fan-out from the duplicate repo_url:
    expect_identical (nrow (out), 3L)
    expect_true (
        all (c ("name", "downloads", "stars", "source") %in% names (out))
    )
    expect_identical (
        unique (out$name [out$repo_url == "https://github.com/o/a"]),
        "a-pypi"
    )
})
