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
