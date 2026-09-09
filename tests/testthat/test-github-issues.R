# ---- github_repo_contributors (HTTP, mocked via httptest2) -----------------
#
# Fixture recorded live against hypertidy/ncmeta (7 contributors), with
# `simplify = FALSE` for the same reason as the github_api_get_all() fixtures
# in test-utils-github.R: this goes through github_api_get_all() internally,
# which needs real x-ratelimit-* headers on replay to avoid the missing-
# headers edge case in github_respect_rate_limit().

test_that ("github_repo_contributors returns login + fractional contribution share", {
    withr::local_envvar (c (GITHUB_TOKEN = NA, GITHUB_PAT = NA))
    ctbs <- httptest2::with_mock_dir ("ghrepos_contributors", {
        suppressMessages (github_repo_contributors ("hypertidy", "ncmeta"))
    })

    expect_s3_class (ctbs, "tbl_df")
    expect_equal (names (ctbs), c ("login", "contribution"))
    expect_equal (nrow (ctbs), 7L)
    expect_equal (sum (ctbs$contribution), 1, tolerance = 1e-6)
    expect_true (ctbs$contribution [ctbs$login == "mdsumner"] > 0.5) # dominant contributor
})

test_that ("github_repo_contributors returns a zero-row tibble for a repo with no contributors", {
    # Reuses the paginated-issues mock dir's structure but points at an
    # endpoint with no matching fixture file... instead, directly unit-test
    # the zero-row shape by stubbing github_api_get_all() - avoids needing a
    # dedicated "genuinely no contributors" live fixture.
    testthat::local_mocked_bindings (
        github_api_get_all = function (...) list (),
        .package = "longtail"
    )
    out <- github_repo_contributors ("o", "empty-repo")
    expect_equal (nrow (out), 0L)
    expect_equal (names (out), c ("login", "contribution"))
})

# ---- github_repo_issues_graphql / github_issue_authors ---------------------
#
# GraphQL fixtures are hand-crafted rather than recorded live: unauthenticated
# requests get a GraphQL rate limit of 0 (verified against the real API - see
# tests-plan.md), so there is no way to record a real GraphQL response
# without a GITHUB_TOKEN. The request hashes these fixture filenames depend
# on were instead computed by intercepting the request httr2 builds (via
# `options(httr2_mock = ...)`) without performing it, then calling
# `httptest2::build_mock_url()` on the (redacted) result - see the
# recording script noted in this file's git history / PR description.
#
# The fixture models a 2-page cursor-paginated issues connection for
# hypertidy/ncmeta: page 1 has 2 issues (one with a login, one to be joined
# against a non-contributor), page 2 (via `endCursor = "CURSOR1"`) has 1
# issue with `author: null` (a deleted/anonymized account), exercising the
# `purrr::pluck(..., .default = NA_character_)` fallback.

test_that ("github_repo_issues_graphql pages via cursor and returns repo_created_at", {
    result <- httptest2::with_mock_dir ("gh_issue_authors_ncmeta", {
        github_repo_issues_graphql ("hypertidy", "ncmeta")
    })

    expect_equal (result$repo_created_at, "2017-06-10T03:38:22Z")
    expect_equal (nrow (result$issues), 3L)
    expect_equal (result$issues$issue_number, c (1L, 2L, 3L))
    expect_true (is.na (result$issues$author [3])) # null author -> NA, not an error
    expect_equal (result$issues$n_comments, c (3L, 0L, 1L))
})

test_that ("github_issue_authors composes contributors + issues, coalescing contribution to 0", {
    withr::local_envvar (c (GITHUB_TOKEN = NA, GITHUB_PAT = NA))
    out <- suppressMessages (httptest2::with_mock_dir ("gh_issue_authors_ncmeta", {
        longtail::github_issue_authors ("https://github.com/hypertidy/ncmeta")
    }))

    expect_equal (nrow (out), 3L)
    expect_equal (
        names (out),
        c ("repo_url", "issue_number", "author", "created_at", "n_comments", "contribution", "repo_created_at")
    )
    expect_true (all (out$repo_url == "https://github.com/hypertidy/ncmeta"))
    expect_true (all (out$repo_created_at == "2017-06-10T03:38:22Z"))

    # issue 1's author (mdsumner) is the dominant real contributor; issue 3's
    # author is NA (not a contributor at all) and must coalesce to 0, not NA.
    expect_true (out$contribution [out$issue_number == 1] > 0.5)
    expect_equal (out$contribution [out$issue_number == 3], 0)
})

test_that ("github_issue_authors returns the empty-shape tibble for a repo with zero issues", {
    testthat::local_mocked_bindings (
        github_repo_contributors = function (...) tibble::tibble (login = character (), contribution = double ()),
        github_repo_issues_graphql = function (...) {
            list (
                repo_created_at = "2020-01-01T00:00:00Z",
                issues = tibble::tibble (
                    issue_number = integer (), author = character (),
                    created_at = character (), n_comments = integer ()
                )
            )
        },
        .package = "longtail"
    )
    out <- longtail::github_issue_authors ("https://github.com/o/empty-repo")
    expect_equal (nrow (out), 0L)
    expect_equal (
        names (out),
        c ("repo_url", "issue_number", "author", "created_at", "n_comments", "contribution", "repo_created_at")
    )
})
