test_that ("clickhouse_query returns JSONCompact as character matrix", {

    out <- httptest2::with_mock_dir ("clickhouse_mock", {
        clickhouse_query ("SELECT 1 AS one, 2 AS two FORMAT JSONCompact")
    })
    expect_true (is.matrix (out))
    expect_equal (dim (out), c (1L, 2L))
})


test_that ("pypi_downloads_full returns a short page of name/downloads", {

    out <- httptest2::with_mock_dir ("pypi_downloads_mock", {
        longtail::pypi_downloads_full ()
    })
    expect_equal (names (out), c ("downloads", "name"))
    expect_equal (nrow (out), 5L)
    expect_equal (out$name [1], "numpy")
    expect_equal (out$downloads [1], 5000000)
})

# pypi_repo_urls_many() uses req_perform_parallel() internally
# (registry_repo_urls_many() -> perform_json_parallel() in
# R/utils-httr2.R), which httptest2 can't trace/record - so
# LONGTAIL_TESTS = "true" switches perform_json_parallel() to sequential
# req_perform() calls instead (see that function's comment). This fixture
# is NOT hand-crafted: it holds real, live-recorded responses for 2 real
# packages (requests, flask) plus one nonexistent package for the 404/NA
# case. If it's ever regenerated, delete tests/testthat/pypi_mock/ and
# re-run this test (no token needed, PyPI is unauthenticated) to re-record
# it - it must never be hand-typed back in.

test_that ("pypi_repo_urls_many resolves via project_urls, NA on 404", {
    Sys.setenv ("LONGTAIL_TESTS" = "true")
    out <- httptest2::with_mock_dir ("pypi_mock", {
        pypi_repo_urls_many (
            c ("requests", "flask", "this-package-does-not-exist-xyz123")
        )
    })
    expect_equal (
        out,
        c (
            "https://github.com/psf/requests",
            "https://github.com/pallets/flask",
            NA_character_
        )
    )
})
