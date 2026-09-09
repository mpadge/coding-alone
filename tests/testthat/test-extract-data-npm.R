# ---- npm_download_counts_fetch / npm_downloads_full ------------------------
#
# The fixture tarball (fixtures/counts-mini.tgz) is a hand-built 4-package
# `package/counts.json`.

test_that ("npm_download_counts_fetches version and raw tarball bytes", {

    fetched <- httptest2::with_mock_dir ("npm_downloads_mock", {
        npm_download_counts_fetch ()
    })
    expect_equal (fetched$version, "2026.09.08")
    expect_true (is.raw (fetched$tarball))
    expect_equal (
        fetched$tarball,
        readBin (test_path ("fixtures", "counts-mini.tgz"), "raw", n = 1000)
    )
})

test_that ("npm_downloads_full untars and reshapes the counts.json download", {
    out <- suppressMessages (httptest2::with_mock_dir ("npm_downloads_mock", {
        longtail::npm_downloads_full ()
    }))

    expect_equal (names (out), c ("name", "downloads"))
    expect_equal (nrow (out), 4L)
    expect_equal (out$downloads [out$name == "is-number"], 5000000)
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
    expect_equal (
        out,
        c (
            "https://github.com/jonschlinkert/is-number",
            "https://github.com/stevemao/left-pad",
            NA_character_
        )
    )
})
