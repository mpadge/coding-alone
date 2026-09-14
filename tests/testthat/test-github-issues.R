# ---- github_repo_contributors (HTTP, mocked via httptest2) -----------------


test_that ("github_repo_contribs returns login + ctb proportion", {

    withr::local_envvar (c (GITHUB_TOKEN = NA, GITHUB_PAT = NA))
    ctbs <- httptest2::with_mock_dir ("ghrepos_contributors",
        {
            suppressMessages (github_repo_contributors ("hypertidy", "ncmeta"))
        },
        simplify = FALSE
    )

    expect_s3_class (ctbs, "tbl_df")
    expect_named (ctbs, c ("login", "contribution"))
    expect_identical (nrow (ctbs), 7L)
    expect_equal (sum (ctbs$contribution), 1, tolerance = 1e-6)
    expect_gt (
        ctbs$contribution [ctbs$login == "mdsumner"], 0.5
    ) # dominant contributor
})

test_that ("github_repo_contribs returns empty when no ctbs", {

    # Reuses the paginated-issues mock dir's structure but points at an
    # endpoint with no matching fixture file... instead, directly unit-test
    # the zero-row shape by stubbing github_api_get_all() - avoids needing a
    # dedicated "genuinely no contributors" live fixture.
    testthat::local_mocked_bindings (
        github_api_get_all = function (...) list (),
        .package = "codingAlone"
    )
    out <- github_repo_contributors ("o", "empty-repo")
    expect_identical (nrow (out), 0L)
    expect_named (out, c ("login", "contribution"))
})

# github_repo_issues_graphql()'s and github_issue_authors()'s real-repo/
# GraphQL-fixture cases are in test-live-graphql.R, gated behind test_all -
# see that file's header comment.

test_that ("github_issue_authors returns empty-shape tibble for 0 issues", {
    testthat::local_mocked_bindings (
        github_repo_contributors = function (...) {
            tibble::tibble (login = character (), contribution = double ())
        },
        github_repo_issues_graphql = function (...) {
            list (
                repo_created_at = "2020-01-01T00:00:00Z",
                issues = tibble::tibble (
                    issue_number = integer (), author = character (),
                    created_at = character (), n_comments = integer ()
                )
            )
        },
        .package = "codingAlone"
    )
    out <- github_issue_authors ("https://github.com/o/empty-repo")
    expect_identical (nrow (out), 0L)
    expect_named (
        out,
        c (
            "repo_url", "issue_number", "author", "created_at",
            "n_comments", "contribution", "repo_created_at"
        )
    )
})
