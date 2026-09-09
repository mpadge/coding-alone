test_that ("build_repo_tbl combines source CSVs, tagging each with its source", {

    out_dir <- withr::local_tempdir ()

    readr::write_csv (
        tibble::tibble (
            package = c ("p1", "p2"),
            repo_url = c ("https://github.com/o/p1", "https://github.com/o/p2"),
            downloads = c (100, 200)
        ),
        file.path (out_dir, "pypi.csv")
    )
    readr::write_csv (
        tibble::tibble (
            name = "n1",
            repo_url = "https://github.com/o/n1",
            downloads = 300
        ),
        file.path (out_dir, "npm.csv")
    )
    readr::write_csv (
        tibble::tibble (
            title = "[REVIEW]: Some Tool",
            repo_url = "https://github.com/o/joss1",
            stars = 5
        ),
        file.path (out_dir, "joss.csv")
    )
    # Not one of the recognized source filenames - must be ignored:
    readr::write_csv (
        tibble::tibble (repo_url = "https://github.com/o/ignored"),
        file.path (out_dir, "issue-authors.csv")
    )

    out <- longtail::build_repo_tbl (out_dir)

    expect_equal (names (out), c ("name", "repo_url", "downloads", "stars", "source"))
    expect_setequal (out$source, c ("pypi", "npm", "joss"))
    expect_false ("https://github.com/o/ignored" %in% out$repo_url)

    joss_row <- dplyr::filter (out, source == "joss")
    expect_equal (joss_row$name, "Some Tool") # "[REVIEW]: " prefix stripped

    pypi_rows <- dplyr::filter (out, source == "pypi")
    expect_true (all (is.na (pypi_rows$stars))) # no stars column in pypi.csv
})

test_that ("build_repo_tbl returns a zero-row tibble when out_dir has no matching CSVs", {
    out_dir <- withr::local_tempdir ()
    readr::write_csv (tibble::tibble (x = 1), file.path (out_dir, "unrelated.csv"))

    out <- longtail::build_repo_tbl (out_dir)
    expect_equal (nrow (out), 0L) # no source file matched, so no rows read
})

# ---- fetch_issue_authors ----------------------------------------------------
#
# Mocks github_issue_authors() itself (not any HTTP layer) - the underlying
# GraphQL/REST calls it makes are already covered directly in
# test-github-issues.R. What's under test here is fetch_issue_authors()'s
# own checkpointing/resume logic and per-repo error isolation.
#
# fetch_issue_authors() also unconditionally calls
# `progressr::handlers(global = TRUE)`, which registers a *global* calling
# handler (`base::globalCallingHandlers()`) - and that errors ("should not
# be called with handlers on the stack") if any calling handler is already
# active, which is true the instant testthat itself starts running (it
# wraps every test/setup file in `withCallingHandlers()`), with no
# handler-free point left to pre-register it from that works under both
# `devtools::test()` and `R CMD check`. `progressr::handlers()` is mocked to
# a no-op below for exactly that reason - not to skip progress reporting
# itself (irrelevant here, batches are 1-2 repos), but because the *real*
# function is fundamentally uncallable from inside a running test.

fake_issue_authors_row <- function (repo_url) {
    tibble::tibble (
        repo_url = repo_url, issue_number = 1L, author = "someone",
        created_at = "2020-01-01", n_comments = 1L, contribution = 0,
        repo_created_at = "2020-01-01"
    )
}

local_no_progressr <- function (env = parent.frame ()) {
    testthat::local_mocked_bindings (
        handlers = function (...) invisible (NULL),
        .package = "progressr",
        .env = env
    )
}

test_that ("fetch_issue_authors writes both checkpoint files on a fresh run", {
    out_dir <- withr::local_tempdir ()
    local_no_progressr ()
    testthat::local_mocked_bindings (
        github_issue_authors = fake_issue_authors_row,
        .package = "longtail"
    )

    res <- fetch_issue_authors (
        c ("https://github.com/o/a", "https://github.com/o/b"), out_dir,
        batch_size = 50L
    )

    expect_equal (nrow (res), 2L)
    expect_true (file.exists (file.path (out_dir, "issue-authors.csv")))
    expect_true (file.exists (file.path (out_dir, "issue-authors-done.rds")))
})

test_that ("fetch_issue_authors resumes, skipping repos already marked done", {
    out_dir <- withr::local_tempdir ()
    local_no_progressr ()
    calls <- character ()
    testthat::local_mocked_bindings (
        github_issue_authors = function (repo_url) {
            calls <<- c (calls, repo_url)
            fake_issue_authors_row (repo_url)
        },
        .package = "longtail"
    )

    fetch_issue_authors (c ("https://github.com/o/a", "https://github.com/o/b"), out_dir, batch_size = 50L)
    calls <- character () # reset before the resumed run
    res2 <- fetch_issue_authors (
        c ("https://github.com/o/a", "https://github.com/o/b", "https://github.com/o/c"),
        out_dir,
        batch_size = 50L
    )

    expect_equal (calls, "https://github.com/o/c") # only the new repo was fetched
    expect_equal (nrow (res2), 3L) # accumulated result includes the earlier run's rows
})

test_that ("fetch_issue_authors isolates a per-repo error without aborting the batch", {
    out_dir <- withr::local_tempdir ()
    local_no_progressr ()
    testthat::local_mocked_bindings (
        github_issue_authors = function (repo_url) {
            if (grepl ("bad", repo_url)) stop ("boom")
            fake_issue_authors_row (repo_url)
        },
        .package = "longtail"
    )

    expect_message (
        res <- fetch_issue_authors (
            c ("https://github.com/o/a", "https://github.com/o/bad"), out_dir,
            batch_size = 50L
        ),
        "failed for https://github.com/o/bad"
    )

    expect_equal (nrow (res), 1L) # only the good repo contributed rows
    expect_equal (res$repo_url, "https://github.com/o/a")
})

test_that ("join_repo_metadata attaches repo_tbl columns by repo_url, deduping repo_tbl first", {
    issue_authors_tbl <- tibble::tibble (
        repo_url = c ("https://github.com/o/a", "https://github.com/o/a", "https://github.com/o/b"),
        issue_number = c (1L, 2L, 1L)
    )
    # repo_tbl has "a" listed under two sources - should not fan out the join
    repo_tbl <- tibble::tibble (
        name = c ("a-pypi", "a-npm", "b"),
        repo_url = c ("https://github.com/o/a", "https://github.com/o/a", "https://github.com/o/b"),
        downloads = c (10, 20, 30),
        stars = c (NA, NA, 5),
        source = c ("pypi", "npm", "joss")
    )

    out <- longtail::join_repo_metadata (issue_authors_tbl, repo_tbl)

    expect_equal (nrow (out), 3L) # unchanged, no fan-out from the duplicate repo_url
    expect_true (all (c ("name", "downloads", "stars", "source") %in% names (out)))
    expect_equal (unique (out$name [out$repo_url == "https://github.com/o/a"]), "a-pypi")
})
