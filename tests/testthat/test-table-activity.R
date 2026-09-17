# ---- issue_rate_tbl --------------------------------------------------------

make_issue_authors_tbl <- function () {
    tibble::tibble (
        repo_url = c (
            "https://github.com/o/popular",
            "https://github.com/o/niche"
        ),
        repo_created_at = c ("2020-01-01", "2020-01-01")
    ) |>
        dplyr::slice (rep (1:2, each = 1)) |>
        dplyr::distinct ()
}

test_that ("issue_rate_tbl returns empty tibble", {

    issue_authors_tbl <- tibble::tibble (
        repo_url = character (),
        created_at = character (),
        n_comments = integer (),
        contribution = double (),
        repo_created_at = character ()
    )
    repo_tbl <- tibble::tibble (
        repo_url = character (),
        source = character (),
        downloads = double (),
        stars = double ()
    )

    out <- issue_rate_tbl (issue_authors_tbl, repo_tbl, "pypi")
    expect_identical (nrow (out), 0L)
    expect_named (
        out,
        c ("popularity_stratum", "month", "n_metric", "n_repo_months", "rate")
    )
    expect_identical (attr (out, "source_name"), "pypi")
})

test_that ("issue_rate_tbl errors on an unknown source", {

    issue_authors_tbl <- tibble::tibble (
        repo_url = character (),
        created_at = character (),
        n_comments = integer (),
        contribution = double (),
        repo_created_at = character ()
    )
    repo_tbl <- tibble::tibble (
        repo_url = character (),
        source = character (),
        downloads = double (),
        stars = double ()
    )
    expect_error (
        issue_rate_tbl (issue_authors_tbl, repo_tbl, "not-a-source"),
        "Unknown source"
    )
})

test_that ("issue_rate_tbl computes exposure and rate", {

    u <- three_gh_urls
    u [2] <- u [1]

    repo_tbl <- tibble::tibble (
        repo_url = unique (u),
        source = "pypi",
        downloads = c (1e6, 10),
        stars = NA_real_
    )
    issue_authors_tbl <- tibble::tibble (
        repo_url = u,
        created_at = c ("2020-02-15", "2020-03-01", "2020-02-01"),
        n_comments = c (2L, 3L, 1L),
        contribution = c (0, 0, 0),
        repo_created_at = c ("2020-01-01", "2020-01-01", "2020-01-01")
    )

    out <- issue_rate_tbl (
        issue_authors_tbl,
        repo_tbl,
        "pypi",
        n_strata = 2L,
        window = 12L,
        date_start = as.Date ("2020-01-01"),
        date_end = as.Date ("2020-03-01")
    )

    expect_identical (nrow (out), 2L * 3L) # 2 strata x 3 months
    expect_true (all (c (
        "popularity_stratum", "month", "n_metric", "n_repo_months", "rate"
    ) %in% names (out)))

    # By March, the popular repo has had 1 issue in Feb + 1 in Mar = 2 total,
    # accumulated (trailing window covers the whole span here).
    popular_march <- dplyr::filter (
        out, month == as.Date ("2020-03-01"), popularity_stratum == "Q2"
    )
    expect_identical (popular_march$n_metric, 2)
    expect_identical (popular_march$n_repo_months, 3L) # Jan, Feb, Mar exposure

    expect_identical (attr (out, "metric"), "issues")
    expect_identical (attr (out, "window"), 12L)
    expect_equal (attr (out, "contrib_threshold"), 0.01)
    expect_identical (attr (out, "source_name"), "pypi")
})

test_that ("issue_rate_tbl excludes contributor issues above threshold", {

    # `popularity_strata()` called with n_strata = 1 sees more than one
    # distinct value - a single repeated value collapses its quantile breaks to
    # one point and errors.

    u <- three_gh_urls
    u [2] <- u [1]
    repo_tbl <- tibble::tibble (
        repo_url = unique (u),
        source = "pypi", downloads = c (100, 5), stars = NA_real_
    )
    # 3rd 'created_at' is outside the analysis window
    issue_authors_tbl <- tibble::tibble (
        repo_url = u,
        created_at = c ("2020-01-15", "2020-01-16", "2010-01-01"),
        n_comments = c (1L, 1L, 0L),
        contribution = c (0, 0.5, 0), # 2nd issue is by a core contributor
        repo_created_at = c ("2020-01-01", "2020-01-01", "2020-01-01")
    )

    out <- issue_rate_tbl (
        issue_authors_tbl, repo_tbl, "pypi",
        n_strata = 1L, contrib_threshold = 0.01,
        date_start = as.Date ("2020-01-01"), date_end = as.Date ("2020-01-01")
    )
    expect_identical (sum (out$n_metric), 1) # only non-ctb issue counted
})

test_that ("issue_rate_tbl supports metric = 'comments'", {
    repo_tbl <- tibble::tibble (
        repo_url = three_gh_urls,
        source = "pypi",
        downloads = c (500, 100, 5),
        stars = NA_real_
    )
    # 3rd 'created_at' is outside the analysis window:
    issue_authors_tbl <- tibble::tibble (
        repo_url = three_gh_urls,
        created_at = c ("2020-01-15", "2020-01-16", "2010-01-01"),
        n_comments = c (2L, 5L, 0L),
        contribution = c (0, 0, 0),
        repo_created_at = c ("2020-01-01", "2020-01-01", "2020-01-01")
    )

    out <- issue_rate_tbl (
        issue_authors_tbl, repo_tbl, "pypi",
        n_strata = 1L, metric = "comments",
        date_start = as.Date ("2020-01-01"), date_end = as.Date ("2020-01-01")
    )
    expect_identical (sum (out$n_metric), 7)
    expect_identical (attr (out, "metric"), "comments")
})
