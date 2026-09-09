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

test_that ("pypi_repo_urls_many resolves via project_urls, NA on 404", {

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
