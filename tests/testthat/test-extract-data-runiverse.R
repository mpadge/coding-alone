test_that ("build_runiv_table aborts for universe = 'cran'", {
    expect_error (
        longtail::build_runiv_table ("cran"),
        "does not currently work"
    )
})

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

# ---- cran_data_pkgstats -----------------------------------------------------

local_pkgstats_fixture <- function (env = parent.frame ()) {
    f <- fs::path (fs::path_temp (), "pkgstats-CRAN-current.Rds")
    dat <- readr::read_csv (test_path ("fixtures", "pkgstats-mini.csv"), show_col_types = FALSE)
    saveRDS (dat, f)
    withr::defer (fs::file_delete (f), envir = env)
    invisible (f)
}

test_that ("cran_data_pkgstats filters with resolvable gh URL", {

    local_pkgstats_fixture ()

    out <- cran_data_pkgstats ()

    expect_equal (names (out), c ("package", "version", "repo_url"))
    expect_equal (out$package, c ("toolA", "toolD"))
    expect_equal (out$version, c ("1.0.0", "3.0.0"))
    expect_equal (
        out$repo_url,
        c (
            "https://github.com/testauthor/toolA",
            "https://github.com/testauthor/toolD"
        )
    )
})

# ---- cran_data_downloads (HTTP, req_perform_parallel()) ---------------------
#
# cran_data_downloads() uses req_perform_parallel() directly, which
# httptest2 can't trace/record - so LONGTAIL_TESTS = "true" switches it to
# sequential req_perform() calls instead (see perform_json_parallel() in
# R/utils-httr2.R for the same pattern/rationale). This fixture is NOT
# hand-crafted: it holds a real, live-recorded download count for a real
# CRAN package (fs) plus a nonexistent one (always 0 downloads). The real
# count drifts over time, so this only checks it's a plausible live value,
# not an exact frozen number. If it's ever regenerated, delete
# tests/testthat/cranlogs_mock/ and re-run this test (no token needed) to
# re-record it - it must never be hand-typed back in.

test_that ("cran_data_downloads left-joins counts onto input", {
    Sys.setenv ("LONGTAIL_TESTS" = "true")
    dat <- tibble::tibble (
        package = c ("fs", "doesnotexist12345"),
        version = c ("1.0.0", "1.0.0"),
        repo_url = NA_character_
    )
    out <- httptest2::with_mock_dir ("cranlogs_mock", {
        cran_data_downloads (dat)
    })

    expect_equal (
        names (out),
        c ("package", "version", "repo_url", "downloads")
    )
    expect_true (out$downloads [out$package == "fs"] > 0L) # real download count, drifts over time
    expect_equal (out$downloads [out$package == "doesnotexist12345"], 0L)
})

test_that ("build_cran_table composes pkgstats + downloads", {

    local_pkgstats_fixture ()
    testthat::local_mocked_bindings (
        cran_data_downloads = function (dat) {
            dplyr::mutate (dat, downloads = c (100L, 200L))
        },
        .package = "longtail"
    )

    out <- longtail::build_cran_table ()
    expect_equal (out$package, c ("toolA", "toolD"))
    expect_equal (out$downloads, c (100L, 200L))
})
