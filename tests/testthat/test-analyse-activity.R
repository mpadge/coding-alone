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
