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
# The real ropensci.r-universe.dev dump is ~360 packages / ~8MB - too big to
# check in as a fixture, and not paginated so there's no small natural
# subset to record. Hand-crafted instead: 4 packages covering resolution via
# `URL`, via `BugReports` (when `URL` isn't a github.com link), via
# `RemoteUrl` (when neither `URL` nor `BugReports` resolve), no resolvable
# URL at all, a reviewed submission (with `review_id`), a non-reviewed one,
# and one with no `_metadata$review` at all.

test_that ("build_runiv_table extracts repo URLs and rOpenSci review metadata", {
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
            "https://github.com/testauthor/toolA", "https://github.com/testauthor/toolB",
            "https://github.com/testauthor/toolC", NA_character_
        )
    )
    expect_equal (out$reviewed, c (TRUE, FALSE, FALSE, FALSE))
    expect_equal (out$review_id, c (123L, NA, NA, NA))
})

# ---- cran_data_pkgstats -----------------------------------------------------
#
# Downloads its .Rds via a bare `download.file()` call, not httr2 - entirely
# outside httptest2's reach. Mocked instead via
# `local_mocked_bindings(download.file = ..., .package = "utils")`, per
# testthat's own docs on mocking namespaced calls to another package (not
# generally recommended, since it affects every `download.file()` call for
# the duration of the test, but there is no source-level binding for
# `download.file` inside `longtail` itself for `local_mocked_bindings()` to
# target with the usual `.package = "longtail"` form). The fixture
# (fixtures/pkgstats-mini.Rds) is a hand-built 5-row stand-in for the real
# `pkgstats-CRAN-current.Rds`, covering: two versions of the same package
# (only the latest should survive `slice_max(date)`), a resolvable
# comma-separated `urls` github.com entry, a github.com URL with a non-repo
# path (`/issues` - 5 path segments, not the resolvable 4), a `urls` value
# with no github.com entry at all, and a comma+newline-separated `urls`
# value (exercising the alternate separator in the `strsplit()` regex).

test_that ("cran_data_pkgstats filters to the latest version with a resolvable github.com URL", {
    fixture <- test_path ("fixtures", "pkgstats-mini.Rds")
    testthat::local_mocked_bindings (
        download.file = function (url, destfile, ...) file.copy (fixture, destfile, overwrite = TRUE),
        .package = "utils"
    )

    out <- cran_data_pkgstats ()

    expect_equal (names (out), c ("package", "version", "repo_url"))
    expect_equal (out$package, c ("toolA", "toolD"))
    expect_equal (out$version, c ("1.0.0", "3.0.0")) # latest of toolA's two versions
    expect_equal (
        out$repo_url,
        c ("https://github.com/testauthor/toolA", "https://github.com/testauthor/toolD")
    )
})

# ---- cran_data_downloads (HTTP, req_perform_parallel()) ---------------------
#
# cran_data_downloads() uses httr2::req_perform_parallel(), which - unlike
# req_perform() - httptest2 cannot auto-record (capture_requests() only
# traces req_perform(); confirmed empirically, see tests-plan.md), even
# though replay works fine (httr2_mock is honoured by both). This fixture
# was instead recorded by manually performing (via plain req_perform()) the
# exact single combined-package-list request cran_data_downloads() would
# otherwise dispatch through the parallel queue - real data for a real (`fs`)
# and a nonexistent (`doesnotexist12345`) CRAN package, frozen at record time.

test_that ("cran_data_downloads left-joins download counts onto the input data", {
    dat <- tibble::tibble (
        package = c ("fs", "doesnotexist12345"),
        version = c ("1.0.0", "1.0.0"),
        repo_url = NA_character_
    )
    out <- httptest2::with_mock_dir ("cranlogs_mock", {
        cran_data_downloads (dat)
    })

    expect_equal (names (out), c ("package", "version", "repo_url", "downloads"))
    expect_equal (out$downloads, c (1759231L, 0L))
})

test_that ("build_cran_table composes cran_data_pkgstats + cran_data_downloads", {
    fixture <- test_path ("fixtures", "pkgstats-mini.Rds")
    testthat::local_mocked_bindings (
        download.file = function (url, destfile, ...) file.copy (fixture, destfile, overwrite = TRUE),
        .package = "utils"
    )
    testthat::local_mocked_bindings (
        cran_data_downloads = function (dat) dplyr::mutate (dat, downloads = c (100L, 200L)),
        .package = "longtail"
    )

    out <- longtail::build_cran_table ()
    expect_equal (out$package, c ("toolA", "toolD"))
    expect_equal (out$downloads, c (100L, 200L))
})
