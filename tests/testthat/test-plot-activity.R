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
