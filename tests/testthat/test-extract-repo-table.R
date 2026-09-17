test_that ("build_repo_tbl combines source CSVs, tagged with source", {

    Sys.setenv ("PEERREVIEW_TESTS" = "true")

    out_dir <- withr::local_tempdir ()
    write_test_analyses_data (out_dir)

    out <- build_repo_tbl (out_dir)

    expect_named (
        out,
        c ("name", "repo_url", "downloads", "stars", "source")
    )
    expect_setequal (out$source, c ("pypi", "npm", "joss"))
    expect_false ("https://github.com/o/ignored" %in% out$repo_url)

    joss_row <- dplyr::filter (out, source == "joss")
    expect_identical (joss_row$name, "Some Tool") # "[REVIEW]: " prefix stripped

    pypi_rows <- dplyr::filter (out, source == "pypi")
    expect_true (all (is.na (pypi_rows$stars))) # no stars column in pypi.csv
})

test_that ("build_repo_tbl returns a emtpy when out_dir has no matches", {
    out_dir <- withr::local_tempdir ()
    readr::write_csv (
        tibble::tibble (x = 1),
        file.path (out_dir, "unrelated.csv")
    )

    out <- build_repo_tbl (out_dir)
    expect_identical (nrow (out), 0L) # no source file matched, so no rows read
})

test_that ("join_repo_metadata attaches repo_tbl columns by repo_url", {
    issue_authors_tbl <- tibble::tibble (
        repo_url = three_gh_urls,
        issue_number = c (1L, 2L, 1L)
    )
    # repo_tbl has "a" listed under two sources - should not fan out the join
    repo_tbl <- tibble::tibble (
        name = c ("a-pypi", "a-npm", "b"),
        repo_url = three_gh_urls,
        downloads = c (10, 20, 30),
        stars = c (NA, NA, 5),
        source = c ("pypi", "npm", "joss")
    )

    out <- join_repo_metadata (issue_authors_tbl, repo_tbl)

    # unchanged, no fan-out from the duplicate repo_url:
    expect_identical (nrow (out), 3L)
    expect_true (
        all (c ("name", "downloads", "stars", "source") %in% names (out))
    )
    expect_identical (
        unique (out$name [out$repo_url == "https://github.com/o/a"]),
        "a-pypi"
    )
})
