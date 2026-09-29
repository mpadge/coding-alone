# Non-core rate by creation-year cohort and fixed repo age

For each source, bin non-core issue activity by (creation-year cohort,
age-in-years-since-creation) instead of by (popularity stratum, calendar
month). This is the age/period/cohort disentangling trick: fixing "age"
(e.g. a repo's first or second year of life) while letting "cohort" (the
calendar year that age happened to fall in) vary isolates a shared
calendar-time effect from a repository's own maturation curve. Pure
maturation predicts roughly constant rates across cohorts at a fixed
age; a shared calendar-time break instead predicts a monotonic decline
in that fixed-age rate as cohort year increases.

## Usage

``` r
cohort_age_rate_tbl(
  issue_authors_tbl,
  repo_tbl,
  sources,
  contrib_threshold = 0.01,
  metric = c("issues", "comments"),
  age_years = 0:1,
  date_start = as.Date("2010-01-01")
)
```

## Arguments

- issue_authors_tbl:

  As returned by [`fetch_issue_authors()`](fetch_issue_authors.md).

- repo_tbl:

  As returned by [`build_repo_tbl()`](build_repo_tbl.md).

- sources:

  Character vector of `source_name` values to include.

- contrib_threshold, metric:

  As in [`issue_rate_tbl()`](issue_rate_tbl.md).

- age_years:

  Integer vector of ages (in whole years since repo creation) to compute
  rates for. Default `0:1` (a repo's first and second year of life).

- date_start:

  Earliest calendar month considered when accumulating repo-month
  exposure (not an age cutoff).

## Value

A tibble: `source`, `cohort_year`, `age_year`, `n_metric`,
`n_repo_months`, `rate`.

## Examples

``` r
if (FALSE) { # \dontrun{
cohort_tbl <- cohort_age_rate_tbl (issue_authors_tbl, repo_tbl, "cran")
plot_cohort_age (cohort_tbl)
} # }
```
