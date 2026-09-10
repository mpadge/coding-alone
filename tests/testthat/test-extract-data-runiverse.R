test_that ("build_runiv_table validates its universe argument", {
    expect_error (longtail::build_runiv_table ("not-a-universe"))
})

# ---- build_runiv_table (HTTP, dynamically-recorded httptest2 fixture) ------

test_that ("build_runiv_table extracts URLs and review metadata from real packages", {
    Sys.setenv ("LONGTAIL_TESTS" = "true")
    out <- suppressMessages (httptest2::with_mock_dir ("runiv_mock", {
        longtail::build_runiv_table ("ropensci")
    }))

    expect_true (nrow (out) > 0L)
    expect_true (nrow (out) <= 5L) # LONGTAIL_TESTS caps the query at 5 packages
    expect_equal (
        names (out),
        c ("package", "repo_url", "downloads", "stars", "reviewed", "review_id")
    )
    expect_type (out$package, "character")
    expect_type (out$reviewed, "logical")
    # Every reviewed package should carry a review_id, and vice versa:
    expect_equal (out$reviewed, !is.na (out$review_id))
})
