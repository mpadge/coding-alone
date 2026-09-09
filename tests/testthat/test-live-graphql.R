# Tests every function that touches GitHub's GraphQL API:
# - github_stars_many()
# - github_repo_issues_graphql()
# - github_issue_authors(),
# - build_joss_table(), which uses github_stars_many() internally
# These only works with real GITHUB_TOKEN/GITHUB_PAT.

test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") ||
    identical (Sys.getenv ("GITHUB_JOB"), "test-coverage"))

skip_if (!test_all)

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

test_that ("github_issue_authors composes real contributors + issues", {
    withr::local_envvar (c (GITHUB_TOKEN = NA, GITHUB_PAT = NA))
    out <- suppressMessages (
        httptest2::with_mock_dir ("gh_issue_authors_ncmeta", {
            longtail::github_issue_authors (
                "https://github.com/hypertidy/ncmeta"
            )
        })
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

# ---- build_joss_table -------------------------------------------------------
#
# The REST side (accepted-issue listing from openjournals/joss-reviews)
# stays a hand-crafted, deliberately short fixture - the real list runs to
# thousands of issues, which would still be too large/slow to record even
# with a token and test_all gating. Its 4 synthetic issues (a
# comment-delimited, an anchor-tag, and a bare-URL Repository line, plus one
# with none at all) reference 3 *real* small repos so the follow-on
# stargazer-count GraphQL lookup below is a genuine recorded response, not
# hand-typed.

test_that ("build_joss_table extracts repo/language/stars, dropping PRs", {
    withr::local_envvar (c (GITHUB_TOKEN = NA, GITHUB_PAT = NA))
    out <- suppressMessages (httptest2::with_mock_dir ("joss_mock", {
        longtail::build_joss_table ()
    }))

    expect_equal (nrow (out), 4L) # the pull_request-tagged 5th entry is dropped
    expect_equal (out$issue_number, c (1000L, 1001L, 1002L, 1003L))
    expect_equal (
        out$repo_url,
        c (
            "https://github.com/hypertidy/ncmeta",
            "https://github.com/r-lib/rprojroot",
            "https://github.com/jeroen/curl",
            NA_character_
        )
    )
    expect_equal (out$language, c ("R", "Python", NA_character_, "C++"))
    expect_equal (out$stars, c (13L, 149L, 233L, NA_integer_))
    expect_true (all (is.na (out$downloads))) # no pypi_tbl/npm_tbl supplied
})
