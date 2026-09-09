test_that ("build_runiv_table aborts for universe = 'cran'", {
    expect_error (
        longtail::build_runiv_table ("cran"),
        "does not currently work"
    )
})

test_that ("build_runiv_table validates its universe argument", {
    expect_error (longtail::build_runiv_table ("not-a-universe"))
})

# ---- build_runiv_table (HTTP, hand-crafted httptest2 fixture) --------------
#
# Hand-crafted to build 4 packages covering resolution via `URL`, via
# `BugReports` (when `URL` isn't a github.com link), via `RemoteUrl` (when
# neither `URL` nor `BugReports` resolve), no resolvable URL at all, a reviewed
# submission (with `review_id`), a non-reviewed one, and one with no
# `_metadata$review` at all.

test_that ("build_runiv_table extracts URLs and review metadata", {

    out <- suppressMessages (httptest2::with_mock_dir ("runiv_mock", {
        longtail::build_runiv_table ("ropensci")
    }))

    expect_equal (nrow (out), 4L)
    expect_equal (
        names (out),
        c ("package", "repo_url", "downloads", "stars", "reviewed", "review_id")
    )
    expect_equal (
        out$repo_url,
        c (
            "https://github.com/testauthor/toolA",
            "https://github.com/testauthor/toolB",
            "https://github.com/testauthor/toolC",
            NA_character_
        )
    )
    expect_equal (out$reviewed, c (TRUE, FALSE, FALSE, FALSE))
    expect_equal (out$review_id, c (123L, NA, NA, NA))
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


test_that ("cran_data_downloads left-joins counts onto input", {

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
    expect_equal (out$downloads, c (1759231L, 0L))
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
