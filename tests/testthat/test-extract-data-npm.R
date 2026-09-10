# ---- npm_download_counts_meta -----------------------------------------------
#
# Only the small metadata call (version + tarball URL) is tested here. The
# tarball itself is a single ~28MB dump of all ~3.77M npm packages'
# download counts, with no server-side size-limiting parameter (unlike
# ClickHouse's LIMIT or the GitHub GraphQL queries' first) - there's no way
# to get a genuinely live, small sample of it, so npm_downloads_full()'s
# untar/parse logic isn't covered by a fixture here. This fixture is NOT
# hand-crafted: it holds a real, live-recorded metadata response. If it's
# ever regenerated, delete tests/testthat/npm_downloads_mock/ and re-run
# this test (no token needed) to re-record it - it must never be
# hand-typed back in.

test_that ("npm_download_counts_meta fetches version and tarball URL", {
    fetched <- httptest2::with_mock_dir ("npm_downloads_mock", {
        npm_download_counts_meta ()
    })
    expect_type (fetched$version, "character")
    expect_type (fetched$dist$tarball, "character")
    # inst/httptest2/redact.R rewrites "https://registry.npmjs.org/" -> "npm/"
    # in recorded fixture bodies, so the replayed URL is redacted, not real:
    expect_match (fetched$dist$tarball, "download-counts.*\\.tgz$")
})

# npm_repo_urls_many() uses req_perform_parallel() internally
# (registry_repo_urls_many() -> perform_json_parallel() in
# R/utils-httr2.R), which httptest2 can't trace/record - so
# LONGTAIL_TESTS = "true" switches perform_json_parallel() to sequential
# req_perform() calls instead (see that function's comment). This fixture
# is NOT hand-crafted: it holds real, live-recorded responses for 2 real
# packages (is-number, left-pad) plus one nonexistent package for the
# 404/NA case. If it's ever regenerated, delete tests/testthat/npm_mock/
# and re-run this test (no token needed, npm's registry is unauthenticated)
# to re-record it - it must never be hand-typed back in.

test_that ("npm_repo_urls_many resolves via repository/homepage, NA on 404", {
    Sys.setenv ("LONGTAIL_TESTS" = "true")
    out <- httptest2::with_mock_dir ("npm_mock", {
        npm_repo_urls_many (
            c ("is-number", "left-pad", "this-package-does-not-exist-xyz123")
        )
    })
    expect_identical (
        out,
        c (
            "https://github.com/jonschlinkert/is-number",
            "https://github.com/stevemao/left-pad",
            NA_character_
        )
    )
})
