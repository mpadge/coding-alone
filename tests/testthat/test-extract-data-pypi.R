test_that ("clickhouse_query returns a JSONCompact result as a character matrix", {
    out <- httptest2::with_mock_dir ("clickhouse_mock", {
        longtail:::clickhouse_query ("SELECT 1 AS one, 2 AS two FORMAT JSONCompact")
    })
    expect_true (is.matrix (out))
    expect_equal (dim (out), c (1L, 2L))
})

# ---- pypi_downloads_full ----------------------------------------------------
#
# IMPORTANT discovered issue, not fixed here (out of scope for a test PR):
# recording a real fixture for this function against the live ClickHouse
# `demo` endpoint failed with a real HTTP 500 - "Code: 396. DB::Exception:
# Limit for result exceeded, max rows: 10.00 thousand, current rows: 20.00
# thousand (TOO_MANY_ROWS_OR_BYTES)". The public `demo` user's row cap has
# apparently been lowered (from this code's assumed 100,000, per
# `CLICKHOUSE_PAGE_SIZE` and its own comment) to 10,000 sometime after this
# code was written, meaning `pypi_downloads_full()` currently fails outright
# against the real service - every page requests `LIMIT 100000`. This
# fixture instead models what a *successful* response would look like (a
# single, already-short page, so pagination stops after one request),
# because that's the behaviour the code is written to expect - not what the
# live service currently returns.

test_that ("pypi_downloads_full pages until a short page, reshaping to name/downloads", {
    out <- httptest2::with_mock_dir ("pypi_downloads_mock", {
        longtail::pypi_downloads_full ()
    })
    expect_equal (names (out), c ("downloads", "name"))
    expect_equal (nrow (out), 5L)
    expect_equal (out$name [1], "numpy")
    expect_equal (out$downloads [1], 5000000)
})

# ---- pypi_repo_urls_many (HTTP, req_perform_parallel()) --------------------
#
# Same req_perform_parallel() situation as npm_repo_urls_many() in
# test-extract-data-npm.R - fixture recorded via manual sequential
# req_perform() calls. 2 real packages (requests, flask; both resolve via
# `info.project_urls`, trimmed down to just the `info` object actually read)
# plus one nonexistent package for the 404/NA path. Neither exercises the
# `info.home_page`-only fallback (no convenient live example found with
# non-github project_urls but a github home_page) - that fallback is
# exercised structurally by extract_repo_url()'s equivalent JOSS test and by
# find_github_url()'s own unit tests, just not through this specific
# extract_candidates() closure.

test_that ("pypi_repo_urls_many resolves via project_urls, NA on 404", {
    out <- httptest2::with_mock_dir ("pypi_mock", {
        longtail:::pypi_repo_urls_many (c ("requests", "flask", "this-package-does-not-exist-xyz123"))
    })
    expect_equal (
        out,
        c ("https://github.com/psf/requests", "https://github.com/pallets/flask", NA_character_)
    )
})
