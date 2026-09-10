# Tests every function that touches GitHub's GraphQL API:
# - github_stars_many()
# - github_repo_issues_graphql()
# - github_issue_authors(),
# - build_joss_table(), which uses github_stars_many() internally
# These only works with real GITHUB_TOKEN/GITHUB_PAT.

test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") ||
    identical (Sys.getenv ("GITHUB_JOB"), "test-coverage"))

skip_if (!test_all)

# All fixtures here were recorded with LONGTAIL_TESTS = "true" (shrinks
# GraphQL page sizes - see github_issues_page_size() in R/github-issues.R
# and joss_issues_page_size() in R/extract-data-joss.R), so it must also be
# set for replay: the query text (and hence the mock file's hash) differs
# between the small test-mode page size and the real default.
Sys.setenv ("LONGTAIL_TESTS" = "true")

test_that ("github_stars_many works but NAs unresolvable repos", {

    out <- httptest2::with_mock_dir ("graphql_stars", {
        suppressMessages (github_stars_many (c (
            "https://github.com/hypertidy/ncmeta", # real repo
            "https://github.com/o/this-repo-does-not-exist-xyz-987654",
            "not-a-github-url" # fails parse_github_repo_url() before request
        )))
    })
    expect_equal (out, c (13L, NA_integer_, NA_integer_))
})

# github_issue_authors() (contributors REST + issues GraphQL) and
# github_repo_issues_graphql() (issues GraphQL only) share the
# "gh_issue_authors_ncmeta" mock dir. with_mock_dir() decides record-vs-
# replay purely by whether the directory exists yet - so whichever test
# runs first must be the one that exercises the *full* set of requests
# (github_issue_authors()'s).

test_that ("github_issue_authors composes real contributors + issues", {
    out <- suppressMessages (
        httptest2::with_mock_dir ("gh_issue_authors_ncmeta",
            {
                longtail::github_issue_authors (
                    "https://github.com/hypertidy/ncmeta"
                )
            },
            simplify = FALSE
        )
    )

    expect_true (nrow (out) > 0L)
    expect_equal (
        names (out),
        c (
            "repo_url", "issue_number", "author", "created_at",
            "n_comments", "contribution", "repo_created_at"
        )
    )
    expect_true (all (out$repo_url == "https://github.com/hypertidy/ncmeta"))
    expect_true (all (out$repo_created_at == "2017-06-10T03:38:22Z"))
    # mdsumner is ncmeta's dominant real contributor:
    expect_true (any (out$author == "mdsumner" & out$contribution > 0.5))
})

test_that ("github_repo_issues_graphql works", {

    result <- httptest2::with_mock_dir ("gh_issue_authors_ncmeta", {
        github_repo_issues_graphql ("hypertidy", "ncmeta")
    })

    expect_equal (result$repo_created_at, "2017-06-10T03:38:22Z")
    expect_true (nrow (result$issues) > 0L)
    expect_equal (
        names (result$issues),
        c ("issue_number", "author", "created_at", "n_comments")
    )
})

test_that ("build_joss_table extracts repo/language/stars from real issues", {
    out <- suppressMessages (httptest2::with_mock_dir ("joss_mock", {
        longtail::build_joss_table ()
    }))

    expect_true (nrow (out) > 0L)
    expect_true (nrow (out) <= 2L) # LONGTAIL_TESTS caps the query at 2 issues
    expect_equal (
        names (out),
        c (
            "issue_number", "title", "issue_url",
            "repo_url", "language", "stars", "downloads"
        )
    )
    expect_type (out$issue_number, "integer")
    expect_true (all (
        grepl (
            "^https://github\\.com/openjournals/joss-reviews/issues/",
            out$issue_url
        )
    ))

    # Every extracted repo_url should at least be a real, resolvable repo,
    # reflected in a non-NA stargazer count:
    resolved <- !is.na (out$repo_url)
    expect_true (all (!is.na (out$stars [resolved])))
    expect_true (all (is.na (out$downloads))) # no pypi_tbl/npm_tbl supplied
})
