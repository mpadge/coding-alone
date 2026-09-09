# ---- npm_repo_urls_many (HTTP, req_perform_parallel()) ---------------------
#
# Uses httr2::req_perform_parallel() (via registry_repo_urls_many() in
# utils-httr2.R), which httptest2 cannot auto-record (see the note above
# cran_data_downloads() in test-extract-data-runiverse.R). This fixture was
# instead recorded by manually performing (via plain req_perform()) each of
# the 3 individual requests npm_repo_urls_many() would otherwise dispatch in
# parallel - 2 real, small, stable packages (is-number, left-pad; both have
# `repository` as a list with a `url` field - the plain-string `repository`
# form isn't separately covered here) plus one nonexistent package for the
# 404/NA path.

# ---- npm_download_counts_fetch / npm_downloads_full ------------------------
#
# npm_downloads_full() originally downloaded its tarball straight to disk via
# `req_perform(req, path = destfile)` - a form httptest2 cannot mock at all:
# httr2's mock short-circuit returns from `req_perform()` before the
# `path =` disk-write step ever runs (confirmed by tracing httr2:::req_perform),
# so no mock file, however recorded, can ever land bytes at `destfile`. Fixed
# by splitting the HTTP side into `npm_download_counts_fetch()` (an
# in-memory `req_perform()` for both the metadata call and the tarball
# itself, returning the raw bytes), which mocks the ordinary httptest2 way,
# and leaving only local disk I/O (writing those bytes to a temp file,
# untarring, reading counts.json) in `npm_downloads_full()`, tested here via
# the same fixture. The fixture tarball (fixtures/counts-mini.tgz) is a
# hand-built 4-package `package/counts.json`.

test_that ("npm_download_counts_fetch returns the version and raw tarball bytes", {
    fetched <- httptest2::with_mock_dir ("npm_downloads_mock", {
        longtail:::npm_download_counts_fetch ()
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
        longtail:::npm_repo_urls_many (c ("is-number", "left-pad", "this-package-does-not-exist-xyz123"))
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
