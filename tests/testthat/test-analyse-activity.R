test_that ("floor_month rounds down to first-of-month", {
    expect_equal (longtail:::floor_month ("2022-03-17"), as.Date ("2022-03-01"))
    expect_equal (
        longtail:::floor_month (c ("2022-03-17", "2022-01-01")),
        as.Date (c ("2022-03-01", "2022-01-01"))
    )
})

test_that ("popularity_strata splits into n_strata ordered quantile bins", {
    x <- c (1, 10, 100, 1000, 10000, 100000, 1000000, 10000000)
    out <- longtail:::popularity_strata (x, n_strata = 4L)

    expect_s3_class (out, "factor")
    expect_true (is.ordered (out))
    expect_equal (nlevels (out), 4L)
    expect_equal (levels (out), paste0 ("Q", 1:4))
    expect_true (out [1] <= out [length (out)]) # roughly monotonic ordering
})

test_that ("popularity_strata falls back to fewer strata when ties partially collapse breaks", {
    # 10 tied low values + 1 high value: only the top quantile break differs,
    # so 4 requested strata collapse to the 1 the data can actually support.
    x <- c (rep (1, 10), 1000)
    out <- longtail:::popularity_strata (x, n_strata = 4L)
    expect_equal (nlevels (out), 1L)
    expect_equal (levels (out), "Q1")
})

test_that ("popularity_strata errors when ALL values are identical", {
    # Pre-existing edge case: when every quantile break collapses to a
    # single value, `unique(breaks)` has length 1, and the subsequent
    # `breaks[1] <- -Inf; breaks[length(breaks)] <- Inf` both target that
    # same single element (ending on `Inf`), leaving `cut()` a length-1
    # `breaks` argument - which `cut()` then treats as "number of
    # intervals" rather than boundaries, and errors on `Inf` intervals.
    x <- rep (0, 20)
    expect_error (
        suppressWarnings (longtail:::popularity_strata (x, n_strata = 4L)),
        "length.out"
    )
})

test_that ("label_stratum_extremes labels only the first/last levels", {
    x <- factor (c ("Q1", "Q2", "Q3"), levels = c ("Q1", "Q2", "Q3"), ordered = TRUE)
    out <- longtail:::label_stratum_extremes (x)
    expect_equal (levels (out), c ("Q1 (low)", "Q2", "Q3 (high)"))
})

test_that ("label_stratum_extremes leaves a single-level factor untouched", {
    x <- factor ("Q1", levels = "Q1", ordered = TRUE)
    out <- longtail:::label_stratum_extremes (x)
    expect_equal (levels (out), "Q1")
})

test_that ("trailing_roll_sum sums a trailing window, partial at the start", {
    x <- c (1, 1, 1, 1, 1)
    out <- longtail:::trailing_roll_sum (x, window = 3L)
    expect_equal (out, c (1, 2, 3, 3, 3))
})

test_that ("activity_metric_label reports metric verb, window, and threshold", {
    lab <- longtail:::activity_metric_label ("issues", window = 6L, contrib_threshold = 0.05)
    expect_match (lab, "Issues opened")
    expect_match (lab, "6-month trailing avg")
    expect_match (lab, "contrib-threshold=0.05")

    lab2 <- longtail:::activity_metric_label ("comments")
    expect_match (lab2, "Comments received")
})

# ---- issue_rate_tbl --------------------------------------------------------

make_issue_authors_tbl <- function () {
    tibble::tibble (
        repo_url = c ("https://github.com/o/popular", "https://github.com/o/niche"),
        repo_created_at = c ("2020-01-01", "2020-01-01")
    ) |>
        dplyr::slice (rep (1:2, each = 1)) |>
        dplyr::distinct ()
}

test_that ("issue_rate_tbl returns the empty-shape tibble when no repos qualify", {
    issue_authors_tbl <- tibble::tibble (
        repo_url = character (), created_at = character (), n_comments = integer (),
        contribution = double (), repo_created_at = character ()
    )
    repo_tbl <- tibble::tibble (
        repo_url = character (), source = character (), downloads = double (), stars = double ()
    )

    out <- longtail::issue_rate_tbl (issue_authors_tbl, repo_tbl, "pypi")
    expect_equal (nrow (out), 0L)
    expect_equal (
        names (out),
        c ("popularity_stratum", "month", "n_metric", "n_repo_months", "rate")
    )
    expect_equal (attr (out, "source_name"), "pypi")
})

test_that ("issue_rate_tbl errors on an unknown source", {
    issue_authors_tbl <- tibble::tibble (
        repo_url = character (), created_at = character (), n_comments = integer (),
        contribution = double (), repo_created_at = character ()
    )
    repo_tbl <- tibble::tibble (
        repo_url = character (), source = character (), downloads = double (), stars = double ()
    )
    expect_error (
        longtail::issue_rate_tbl (issue_authors_tbl, repo_tbl, "not-a-source"),
        "Unknown source"
    )
})

test_that ("issue_rate_tbl computes exposure and rate for a small synthetic case", {
    repo_tbl <- tibble::tibble (
        repo_url = c ("https://github.com/o/popular", "https://github.com/o/niche"),
        source = "pypi",
        downloads = c (1e6, 10),
        stars = NA_real_
    )
    issue_authors_tbl <- tibble::tibble (
        repo_url = c (
            "https://github.com/o/popular", "https://github.com/o/popular",
            "https://github.com/o/niche"
        ),
        created_at = c ("2020-02-15", "2020-03-01", "2020-02-01"),
        n_comments = c (2L, 3L, 1L),
        contribution = c (0, 0, 0),
        repo_created_at = c ("2020-01-01", "2020-01-01", "2020-01-01")
    )

    out <- longtail::issue_rate_tbl (
        issue_authors_tbl, repo_tbl, "pypi",
        n_strata = 2L, window = 12L,
        date_start = as.Date ("2020-01-01"), date_end = as.Date ("2020-03-01")
    )

    expect_equal (nrow (out), 2L * 3L) # 2 strata x 3 months
    expect_true (all (c ("popularity_stratum", "month", "n_metric", "n_repo_months", "rate") %in% names (out)))

    # By March, the popular repo has had 1 issue in Feb + 1 in Mar = 2 total,
    # accumulated (trailing window covers the whole span here).
    popular_march <- dplyr::filter (
        out, month == as.Date ("2020-03-01"), popularity_stratum == "Q2"
    )
    expect_equal (popular_march$n_metric, 2)
    expect_equal (popular_march$n_repo_months, 3L) # Jan, Feb, Mar exposure

    expect_equal (attr (out, "metric"), "issues")
    expect_equal (attr (out, "window"), 12L)
    expect_equal (attr (out, "contrib_threshold"), 0.01)
    expect_equal (attr (out, "source_name"), "pypi")
})

test_that ("issue_rate_tbl excludes contributor issues above threshold", {
    # A second, issue-less repo with a different `downloads` value is
    # included purely so `popularity_strata()` (called with n_strata = 1)
    # sees more than one distinct value - a single repeated value collapses
    # its quantile breaks to one point and errors (see the
    # popularity_strata "errors when ALL values are identical" test above).
    repo_tbl <- tibble::tibble (
        repo_url = c ("https://github.com/o/repo", "https://github.com/o/other"),
        source = "pypi", downloads = c (100, 5), stars = NA_real_
    )
    issue_authors_tbl <- tibble::tibble (
        repo_url = c ("https://github.com/o/repo", "https://github.com/o/repo", "https://github.com/o/other"),
        created_at = c ("2020-01-15", "2020-01-16", "2010-01-01"), # 3rd is outside the analysis window
        n_comments = c (1L, 1L, 0L),
        contribution = c (0, 0.5, 0), # 2nd issue is by a core contributor
        repo_created_at = c ("2020-01-01", "2020-01-01", "2020-01-01")
    )

    out <- longtail::issue_rate_tbl (
        issue_authors_tbl, repo_tbl, "pypi",
        n_strata = 1L, contrib_threshold = 0.01,
        date_start = as.Date ("2020-01-01"), date_end = as.Date ("2020-01-01")
    )
    expect_equal (sum (out$n_metric), 1) # only the non-contributor issue counted
})

test_that ("issue_rate_tbl supports metric = 'comments'", {
    repo_tbl <- tibble::tibble (
        repo_url = c ("https://github.com/o/repo", "https://github.com/o/other"),
        source = "pypi", downloads = c (100, 5), stars = NA_real_
    )
    issue_authors_tbl <- tibble::tibble (
        repo_url = c ("https://github.com/o/repo", "https://github.com/o/repo", "https://github.com/o/other"),
        created_at = c ("2020-01-15", "2020-01-16", "2010-01-01"), # 3rd is outside the analysis window
        n_comments = c (2L, 5L, 0L),
        contribution = c (0, 0, 0),
        repo_created_at = c ("2020-01-01", "2020-01-01", "2020-01-01")
    )

    out <- longtail::issue_rate_tbl (
        issue_authors_tbl, repo_tbl, "pypi",
        n_strata = 1L, metric = "comments",
        date_start = as.Date ("2020-01-01"), date_end = as.Date ("2020-01-01")
    )
    expect_equal (sum (out$n_metric), 7)
    expect_equal (attr (out, "metric"), "comments")
})

test_that ("fit_activity_model fits a quasipoisson GLM with the interaction term", {
    rate_tbl <- tibble::tibble (
        popularity_stratum = factor (
            rep (c ("Q1", "Q2"), each = 6), levels = c ("Q1", "Q2"), ordered = TRUE
        ),
        month = rep (seq (as.Date ("2020-01-01"), by = "month", length.out = 6), 2),
        n_metric = c (1:6, 6:1),
        n_repo_months = rep (10L, 12)
    )

    fit <- longtail::fit_activity_model (rate_tbl)
    expect_s3_class (fit, "glm")
    expect_true (any (grepl ("month_num:popularity_stratum", names (stats::coef (fit)))))
})

test_that ("fit_activity_model drops rows with zero repo-months exposure", {
    rate_tbl <- tibble::tibble (
        popularity_stratum = factor (
            rep (c ("Q1", "Q2"), each = 4), levels = c ("Q1", "Q2"), ordered = TRUE
        ),
        month = rep (seq (as.Date ("2020-01-01"), by = "month", length.out = 4), 2),
        n_metric = c (0, 1, 2, 3, 1, 2, 3, 4),
        n_repo_months = c (0L, 10L, 10L, 10L, 10L, 10L, 10L, 10L)
    )
    fit <- longtail::fit_activity_model (rate_tbl)
    expect_equal (nrow (fit$model), 7L) # the one zero-exposure row is dropped
})

# ---- plotting ---------------------------------------------------------------

make_rate_tbl <- function (n_strata = 2L, n_months = 12L) {
    strata <- paste0 ("Q", seq_len (n_strata))
    months <- seq (as.Date ("2020-01-01"), by = "month", length.out = n_months)
    tbl <- tidyr_expand_grid_stub (strata, months)
    set.seed (42)
    tbl$rate <- stats::runif (nrow (tbl), 0, 5)
    tbl$popularity_stratum <- factor (tbl$strata, levels = strata, ordered = TRUE)
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

test_that ("plot_activity returns a ggplot with a colour-mapped line per stratum", {
    rate_tbl <- make_rate_tbl ()
    attr (rate_tbl, "source_name") <- "pypi"

    p <- longtail::plot_activity (rate_tbl)
    expect_s3_class (p, "ggplot")
    expect_true ("colour" %in% names (p$mapping) || "colour" %in% names (p$layers [[1]]$mapping))
})

test_that ("plot_activity honours start_year by cropping the plotted data", {
    rate_tbl <- make_rate_tbl (n_months = 24L)
    p_full <- longtail::plot_activity (rate_tbl)
    p_cropped <- longtail::plot_activity (rate_tbl, start_year = 2021)

    expect_true (min (p_cropped$data$month) >= as.Date ("2021-01-01"))
    expect_true (min (p_full$data$month) < as.Date ("2021-01-01"))
})

test_that ("plot_activity_by_source builds one line per source for one stratum", {
    # 2 repos per source (with different `downloads`/`stars`) so
    # popularity_strata() doesn't collapse to a single tied value within any
    # source (see the note above `issue_rate_tbl excludes contributor
    # issues...`), and every one of the 5 known sources has at least one
    # repo, to avoid the bind_rows/factor-level mismatch documented in
    # "errors when a source has no matching repos at all" below.
    sources <- c ("pypi", "npm", "joss", "ropensci", "cran")
    repo_tbl <- purrr::map_dfr (sources, \ (s) {
        tibble::tibble (
            repo_url = paste0 ("https://github.com/o/", s, c ("-1", "-2")),
            source = s,
            downloads = if (s %in% c ("pypi", "npm", "cran")) c (1000, 50) else NA_real_,
            stars = if (s %in% c ("joss", "ropensci")) c (1000, 50) else NA_real_
        )
    })
    issue_authors_tbl <- tibble::tibble (
        repo_url = repo_tbl$repo_url,
        created_at = "2020-01-15",
        n_comments = 1L,
        contribution = 0,
        repo_created_at = "2020-01-01"
    )

    p <- longtail::plot_activity_by_source (
        issue_authors_tbl, repo_tbl, stratum = 1,
        n_strata = 1L, start_year = 2020
    )
    expect_s3_class (p, "ggplot")
    expect_setequal (as.character (unique (p$data$source_name)), sources)
})

test_that ("plot_activity_by_source errors when a source has no matching repos at all", {
    # Discovered edge case: the roxygen docs for plot_activity_by_source()
    # claim "a source with no data yet for the requested window ... just
    # contributes no line, rather than erroring" - but issue_rate_tbl()'s
    # zero-row early return produces a `popularity_stratum` factor with NO
    # levels, which is incompatible (for dplyr::bind_rows()) with the
    # populated ordered factors from sources that DO have data. In practice
    # this only bites when a source is entirely absent from repo_tbl, which
    # doesn't happen once every source has been populated at all.
    repo_tbl <- tibble::tibble (
        repo_url = c ("https://github.com/o/a", "https://github.com/o/a2"),
        source = "pypi", downloads = c (1000, 50), stars = NA_real_
    )
    issue_authors_tbl <- tibble::tibble (
        repo_url = repo_tbl$repo_url,
        created_at = "2020-01-15", n_comments = 1L, contribution = 0,
        repo_created_at = "2020-01-01"
    )

    expect_error (
        longtail::plot_activity_by_source (
            issue_authors_tbl, repo_tbl, stratum = 1,
            n_strata = 1L, start_year = 2020
        ),
        "Can't combine"
    )
})
