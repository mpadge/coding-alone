test_that ("normalize_github_url handles NA/empty input", {
    expect_true (is.na (normalize_github_url (NA_character_)))
    expect_true (is.na (normalize_github_url (NULL)))
    expect_true (is.na (normalize_github_url ("")))
})

test_that ("normalize_github_url normalizes plain github.com URLs", {
    expect_equal (
        normalize_github_url ("https://github.com/owner/repo"),
        "https://github.com/owner/repo"
    )
    expect_equal (
        normalize_github_url ("http://github.com/owner/repo"),
        "https://github.com/owner/repo"
    )
    expect_equal (
        normalize_github_url ("https://github.com/owner/repo.git"),
        "https://github.com/owner/repo"
    )
    expect_equal (
        normalize_github_url ("https://github.com/owner/repo/issues"),
        "https://github.com/owner/repo"
    )
    expect_equal (
        normalize_github_url ("git+https://github.com/owner/repo.git"),
        "https://github.com/owner/repo"
    )
})

test_that ("normalize_github_url handles the git@github.com: SSH form", {
    expect_equal (
        normalize_github_url ("git@github.com:owner/repo.git"),
        "https://github.com/owner/repo"
    )
})

test_that ("normalize_github_url handles the github: shorthand", {
    expect_equal (
        normalize_github_url ("github:owner/repo"),
        "https://github.com/owner/repo"
    )
})

test_that ("normalize_github_url returns NA for non-github URLs", {
    expect_true (is.na (normalize_github_url ("https://example.com/owner/repo")))
    expect_true (is.na (normalize_github_url ("not a url at all")))
})

test_that ("find_github_url returns first resolvable candidate", {
    candidates <- c (NA_character_, "https://example.com/x", "https://github.com/owner/repo")
    expect_equal (find_github_url (candidates), "https://github.com/owner/repo")
})

test_that ("find_github_url handles NULL entries and empty/all-non-matching input", {
    candidates <- list (NULL, "https://example.com/x", NULL)
    expect_true (is.na (find_github_url (candidates)))
    expect_true (is.na (find_github_url (character ())))
})

test_that ("parse_github_repo_url splits owner/repo", {
    parsed <- parse_github_repo_url ("https://github.com/owner/repo")
    expect_equal (parsed, list (owner = "owner", repo = "repo"))

    parsed2 <- parse_github_repo_url ("https://github.com/owner/repo.git")
    expect_equal (parsed2$repo, "repo")
})

test_that ("parse_github_repo_url errors on non-github URLs", {
    expect_error (
        parse_github_repo_url ("https://example.com/owner/repo"),
        "Not a github.com repo URL"
    )
})

test_that ("github_token reads GITHUB_TOKEN, falls back to GITHUB_PAT, else NA", {
    withr::with_envvar (
        c (GITHUB_TOKEN = "", GITHUB_PAT = ""),
        expect_true (is.na (github_token ()))
    )
    withr::with_envvar (
        c (GITHUB_TOKEN = "tok-1", GITHUB_PAT = "tok-2"),
        expect_equal (github_token (), "tok-1")
    )
    withr::with_envvar (
        c (GITHUB_TOKEN = NA, GITHUB_PAT = "tok-2"),
        expect_equal (github_token (), "tok-2")
    )
})

test_that ("github_respect_rate_limit is silent when quota is not nearly exhausted", {
    resp_ok <- httr2::response (
        status_code = 200,
        headers = list (`x-ratelimit-remaining` = "42", `x-ratelimit-reset` = "0")
    )
    expect_silent (github_respect_rate_limit (resp_ok))
})

test_that ("github_respect_rate_limit messages and waits when quota nearly exhausted", {
    resp_low <- httr2::response (
        status_code = 200,
        headers = list (
            `x-ratelimit-remaining` = "1",
            `x-ratelimit-reset` = as.character (as.numeric (Sys.time ()))
        )
    )
    # `reset_at` is ~now, so the actual `Sys.sleep()` call is ~2s (the fixed
    # buffer added on top of the remaining reset time) - short enough to
    # exercise for real rather than mocking a base function.
    expect_message (
        github_respect_rate_limit (resp_low),
        "Rate limit nearly exhausted"
    )
})

test_that ("github_respect_rate_limit errors if rate-limit headers are absent entirely", {
    # Pre-existing edge case: `remaining` comes back as `numeric(0)` (not NA)
    # when the header is missing altogether (rather than present-but-blank),
    # which makes the `!is.na(remaining) && remaining <= 1` guard evaluate to
    # NA rather than FALSE, and `if (NA)` errors. Real GitHub API responses
    # always carry these headers, so this only matters for a response that
    # was never actually a rate-limited GitHub API response - documented
    # here rather than silently asserted away.
    resp_missing <- httr2::response (status_code = 200)
    expect_error (
        github_respect_rate_limit (resp_missing),
        "missing value where TRUE/FALSE needed"
    )
})

# ---- github_api_get_all (HTTP, mocked via httptest2) -----------------------
#
# github_api_get_all() is a generic REST-paginating helper (also used by
# github_repo_contributors(), unrelated to issues), so it isn't made
# LONGTAIL_TESTS-aware itself - instead this test's own query is trimmed:
# `since` filters hypertidy/ncmeta's real issues down to just the 5 most
# recently updated (all bulk-touched on the same day, confirmed live),
# and `per_page = 2` still forces real pagination across 3 requests despite
# that small total.
#
# These fixtures are recorded with `simplify = FALSE`, unlike the other
# httptest2 fixtures in this suite. `simplify = TRUE` (httptest2's default)
# only ever saves the raw JSON body, discarding all response headers -
# meaning every replayed response comes back with NO `x-ratelimit-*`
# headers, which triggers the exact "errors if rate-limit headers are absent
# entirely" edge case documented above on every single page, since
# `github_api_get_all()` calls `github_respect_rate_limit()` after every
# page. `simplify = FALSE` instead serializes the full response object
# (headers included) via `dput()`, so the real recorded `x-ratelimit-*`
# values (comfortably > 1) are replayed too, avoiding the crash without
# changing production code.

test_that ("github_api_get_all pages through a paginated REST endpoint", {
    call_it <- function () {
        httptest2::with_mock_dir ("ghrepos_issues_paginated",
            {
                github_api_get_all (
                    "/repos/hypertidy/ncmeta/issues",
                    query = list (state = "all", since = "2026-07-28T00:00:00Z"),
                    per_page = 2L
                )
            },
            simplify = FALSE
        )
    }
    issues <- call_it ()

    expect_length (issues, 5L)
    expect_true (all (vapply (issues, function (i) "number" %in% names (i), logical (1))))
})
