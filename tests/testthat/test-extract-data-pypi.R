test_that ("clickhouse_query returns JSONCompact as character matrix", {

    out <- httptest2::with_mock_dir ("clickhouse_mock", {
        clickhouse_query ("SELECT 1 AS one, 2 AS two FORMAT JSONCompact")
    })
    expect_true (is.matrix (out))
    expect_identical (dim (out), c (1L, 2L))
})


test_that ("pypi_downloads_full returns a short page of name/downloads", {

    Sys.setenv ("PEERREVIEW_TESTS" = "true")

    out <- httptest2::with_mock_dir ("pypi_downloads_mock", {
        pypi_downloads_full ()
    })

    expect_named (out, c ("downloads", "name"))
    expect_gt (nrow (out), 0L)
    expect_lte (nrow (out), 5L) # PEERREVIEW_TESTS caps the query at 5 rows
    expect_type (out$name, "character")
    expect_true (all (out$downloads > 0))
    expect_false (is.unsorted (rev (out$downloads))) # ORDER BY downloads DESC
})

# pypi_repo_urls_many() uses req_perform_parallel() internally
# (registry_repo_urls_many() -> perform_json_parallel() in
# R/utils-httr2.R), which httptest2 can't trace/record - so
# PEERREVIEW_TESTS = "true" switches perform_json_parallel() to sequential
# req_perform() calls instead (see that function's comment).

test_that ("pypi_repo_urls_many resolves via project_urls, NA on 404", {

    Sys.setenv ("PEERREVIEW_TESTS" = "true")

    out <- httptest2::with_mock_dir ("pypi_mock", {
        pypi_repo_urls_many (
            c ("requests", "flask", "this-package-does-not-exist-xyz123")
        )
    })

    expect_identical (
        out,
        c (
            "https://github.com/psf/requests",
            "https://github.com/pallets/flask",
            NA_character_
        )
    )
})
