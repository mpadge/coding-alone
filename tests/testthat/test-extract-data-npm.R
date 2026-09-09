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

test_that ("npm_repo_urls_many resolves via repository/homepage, NA on 404", {
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
