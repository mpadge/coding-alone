test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") ||
    identical (Sys.getenv ("GITHUB_JOB"), "test-coverage"))

test_that ("build_runiv_table validates its universe argument", {
    expect_error (build_runiv_table ("not-a-universe"))
})

test_that ("build_runiv_table extracts URLs and review metadata", {
    Sys.setenv ("PEERREVIEW_TESTS" = "true")
    out <- suppressMessages (httptest2::with_mock_dir ("runiv_mock", {
        build_runiv_table ("ropensci")
    }))

    expect_gt (nrow (out), 0L)
    expect_lte (nrow (out), 5L) # PEERREVIEW_TESTS caps the query at 5 packages
    expect_named (
        out,
        c ("package", "repo_url", "downloads", "stars", "reviewed", "review_id")
    )
    expect_type (out$package, "character")
    expect_type (out$reviewed, "logical")
    # Every reviewed package should carry a review_id, and vice versa:
    expect_identical (out$reviewed, !is.na (out$review_id))
})

skip_if_not (test_all)

test_that ("build table from db", {

    # This does an actual live extraction - no mocking

    univ <- "urbananalyst"
    x <- build_table_from_db (univ)
    expect_s3_class (x, "tbl")
    expect_identical (ncol (x), 4L)
    expect_named (x, c ("package", "version", "repo_url", "downloads"))
})
