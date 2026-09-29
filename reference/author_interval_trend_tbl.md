# Rolling geometric-mean wait time between consecutive first-time authors, by source and popularity stratum

Aggregates [`author_interval_tbl()`](author_interval_tbl.md)'s
event-level intervals into a (popularity stratum x month) grid: each
interval is binned by the calendar month of its `event_time`, and
reported as a `window`-month trailing geometric mean of `interval_days`.
A geometric (not arithmetic) mean is used because wait times between
authors are heavily right-skewed - in a sparse stratum-month cell, a
single repo that went quiet for years would otherwise dominate an
arithmetic mean of just a handful of intervals. A handful of
near-simultaneous arrivals (`interval_days` at or near 0) are floored at
one minute before logging, since `log(0) = -Inf` would otherwise wreck
that whole cell's geometric mean rather than just pulling it down.

## Usage

``` r
author_interval_trend_tbl(
  issue_authors_tbl,
  repo_tbl,
  source_name,
  n_strata = 4L,
  window = 12L,
  date_start = as.Date("2015-01-01"),
  date_end = NULL
)
```

## Arguments

- issue_authors_tbl:

  As returned by [`fetch_issue_authors()`](fetch_issue_authors.md).

- repo_tbl:

  As returned by [`build_repo_tbl()`](build_repo_tbl.md).

- source_name:

  One of `repo_tbl$source` (`"pypi"`, `"npm"`, `"joss"`, `"ropensci"`).

- n_strata:

  Number of popularity strata.

- window:

  Trailing aggregation window, in months. Default 12: each reported
  month's `n_metric`/`n_repo_months` sum that month and the preceding
  11.

- date_start, date_end:

  Date bounds on the analysis window; `date_end` defaults to the start
  of the current month.

## Value

A tibble with one row per (popularity stratum, month):
`popularity_stratum`, `month`, `n_events` (trailing sum of author
arrivals contributing an interval that month), `geo_mean_days` (the
`window`-month trailing geometric mean of `interval_days`, `NA` where
`n_events` is 0). Carries `window` and `source_name` as attributes.

## Examples

``` r
if (FALSE) { # \dontrun{
it <- author_interval_trend_tbl (issue_authors_tbl, repo_tbl, "pypi")
} # }
```
