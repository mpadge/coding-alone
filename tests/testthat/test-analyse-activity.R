test_that ("floor_month rounds down to first-of-month", {
    expect_identical (floor_month ("2022-03-17"), as.Date ("2022-03-01"))
    expect_identical (
        floor_month (c ("2022-03-17", "2022-01-01")),
        as.Date (c ("2022-03-01", "2022-01-01"))
    )
})

test_that ("popularity_strata splits into n_strata ordered quantile bins", {
    x <- c (1, 10, 100, 1000, 10000, 100000, 1000000, 10000000)
    out <- popularity_strata (x, n_strata = 4L)

    expect_s3_class (out, "factor")
    expect_s3_class (out, "ordered")
    expect_identical (nlevels (out), 4L)
    expect_identical (levels (out), paste0 ("Q", 1:4))
    expect_lte (out [1], out [length (out)]) # roughly monotonic ordering
})

test_that ("popularity_strata falls back when ties collapse", {

    x <- c (rep (1, 10), 1000)
    out <- popularity_strata (x, n_strata = 4L)
    expect_identical (nlevels (out), 1L)
    expect_identical (levels (out), "Q1")
})

test_that ("popularity_strata errors when ALL values are identical", {

    x <- rep (0, 20)
    expect_error (
        suppressWarnings (popularity_strata (x, n_strata = 4L)),
        "length.out"
    )
})

test_that ("label_stratum_extremes labels only the first/last levels", {

    x <- factor (
        c ("Q1", "Q2", "Q3"),
        levels = c ("Q1", "Q2", "Q3"),
        ordered = TRUE
    )
    out <- label_stratum_extremes (x)
    expect_identical (levels (out), c ("Q1 (low)", "Q2", "Q3 (high)"))
})

test_that ("label_stratum_extremes leaves a single-level factor untouched", {
    x <- factor ("Q1", levels = "Q1", ordered = TRUE)
    out <- label_stratum_extremes (x)
    expect_identical (levels (out), "Q1")
})

test_that ("trailing_roll_sum sums a trailing window, partial at the start", {
    x <- c (1, 1, 1, 1, 1)
    out <- trailing_roll_sum (x, window = 3L)
    expect_identical (out, c (1, 2, 3, 3, 3))
})

test_that ("activity_metric_label reports metric verb, window, and threshold", {

    lab <- activity_metric_label (
        "issues",
        window = 6L,
        contrib_threshold = 0.05
    )
    expect_match (lab, "Issues opened")
    expect_match (lab, "6-month trailing avg")
    expect_match (lab, "contrib-threshold=0.05")

    lab2 <- activity_metric_label ("comments")
    expect_match (lab2, "Comments received")
})

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

test_that ("fit_activity_model fits GLM with interaction term", {

    rate_tbl <- tibble::tibble (
        popularity_stratum = factor (
            rep (c ("Q1", "Q2"), each = 6),
            levels = c ("Q1", "Q2"), ordered = TRUE
        ),
        month = rep (seq (
            as.Date ("2020-01-01"),
            by = "month", length.out = 6
        ), 2),
        n_metric = c (1:6, 6:1),
        n_repo_months = rep (10L, 12)
    )

    fit <- fit_activity_model (rate_tbl)
    expect_s3_class (fit, "glm")
    expect_true (any (
        grepl (
            "month_num:popularity_stratum",
            names (stats::coef (fit)),
            fixed = TRUE
        )
    ))
})

test_that ("fit_activity_model drops rows with zero repo-months exposure", {

    rate_tbl <- tibble::tibble (
        popularity_stratum = factor (
            rep (c ("Q1", "Q2"), each = 4),
            levels = c ("Q1", "Q2"), ordered = TRUE
        ),
        month = rep (seq (
            as.Date ("2020-01-01"),
            by = "month", length.out = 4
        ), 2),
        n_metric = c (0, 1, 2, 3, 1, 2, 3, 4),
        n_repo_months = c (0L, 10L, 10L, 10L, 10L, 10L, 10L, 10L)
    )
    fit <- fit_activity_model (rate_tbl)
    # the one zero-exposure row is dropped
    expect_identical (nrow (fit$model), 7L)
})

# ---- plotting ---------------------------------------------------------------

make_rate_tbl <- function (n_strata = 2L, n_months = 12L) {

    strata <- paste0 ("Q", seq_len (n_strata))
    months <- seq (as.Date ("2020-01-01"), by = "month", length.out = n_months)
    tbl <- tidyr_expand_grid_stub (strata, months)
    set.seed (42)
    tbl$rate <- stats::runif (nrow (tbl), 0, 5)
    tbl$popularity_stratum <-
        factor (tbl$strata, levels = strata, ordered = TRUE)
    tbl$month <- tbl$months
    attr (tbl, "metric") <- "issues"
    attr (tbl, "window") <- 12L
    attr (tbl, "contrib_threshold") <- 0.01
    tbl
}

# Small stand-in for tidyr::expand_grid, to avoid adding a new dependency
# just for test fixtures.
tidyr_expand_grid_stub <- function (strata, months) {
    do.call (rbind, lapply (strata, function (s) {
        data.frame (strata = s, months = months)
    }))
}

test_that ("plot_activity returns plot with a colour-mapped stratum lines", {
    rate_tbl <- make_rate_tbl ()
    attr (rate_tbl, "source_name") <- "pypi"

    p <- plot_activity (rate_tbl)
    expect_s3_class (p, "ggplot")
    expect_true (
        "colour" %in% names (p$mapping) ||
            "colour" %in% names (p$layers [[1]]$mapping)
    )
})

test_that ("plot_activity honours start_year by cropping the plotted data", {
    rate_tbl <- make_rate_tbl (n_months = 24L)
    p_full <- plot_activity (rate_tbl)
    p_cropped <- plot_activity (rate_tbl, start_year = 2021)

    expect_gte (min (p_cropped$data$month), as.Date ("2021-01-01"))
    expect_lt (min (p_full$data$month), as.Date ("2021-01-01"))
})

test_that ("plot_activity_by_source builds one line for one stratum", {

    # 2 repos per source (with different `downloads`/`stars`) so
    # popularity_strata() doesn't collapse to a single tied value within any
    # source, and every one of the 5 known sources has at least one
    # repo.
    sources <- c ("pypi", "npm", "joss", "ropensci", "cran")
    repo_tbl <- purrr::map_dfr (sources, \ (s) {
        tibble::tibble (
            repo_url = paste0 ("https://github.com/o/", s, c ("-1", "-2")),
            source = s,
            downloads = if (s %in% c ("pypi", "npm", "cran")) {
                c (1000, 50)
            } else {
                NA_real_
            },
            stars = if (s %in% c ("joss", "ropensci")) {
                c (1000, 50)
            } else {
                NA_real_
            }
        )
    })
    issue_authors_tbl <- tibble::tibble (
        repo_url = repo_tbl$repo_url,
        created_at = "2020-01-15",
        n_comments = 1L,
        contribution = 0,
        repo_created_at = "2020-01-01"
    )

    p <- plot_activity_by_source (
        issue_authors_tbl, repo_tbl,
        stratum = 1,
        n_strata = 1L, start_year = 2020
    )
    expect_s3_class (p, "ggplot")
    expect_setequal (as.character (unique (p$data$source_name)), sources)
})

test_that ("plot_activity_by_source errors when no repos match", {

    repo_tbl <- tibble::tibble (
        repo_url = three_gh_urls,
        source = "pypi",
        downloads = c (1000, 200, 50),
        stars = NA_real_
    )
    issue_authors_tbl <- tibble::tibble (
        repo_url = repo_tbl$repo_url,
        created_at = "2020-01-15",
        n_comments = 1L,
        contribution = 0,
        repo_created_at = "2020-01-01"
    )

    expect_error (
        plot_activity_by_source (
            issue_authors_tbl, repo_tbl,
            stratum = 1,
            n_strata = 1L, start_year = 2020
        ),
        "Can't combine"
    )
})
