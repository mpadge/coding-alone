# Fold-change in distinct non-core author density, from a reference month to now

The [`author_density_tbl()`](author_density_tbl.md) analogue of
[`step_change_tbl()`](step_change_tbl.md): for each of several sources,
compare each popularity stratum's author-density value at a fixed
reference month against its most recent value. Both endpoints are
estimated from a linear regression fitted to the trailing rate from
`ref_date` onwards.

## Usage

``` r
author_density_step_change_tbl(
  issue_authors_tbl,
  repo_tbl,
  sources,
  n_strata = 4L,
  contrib_threshold = 0.01,
  contrib_min = -Inf,
  window = 12L,
  ref_date = as.Date("2021-01-01")
)
```

## Arguments

- issue_authors_tbl:

  As returned by [`fetch_issue_authors()`](fetch_issue_authors.md).

- repo_tbl:

  As returned by [`build_repo_tbl()`](build_repo_tbl.md).

- sources:

  Character vector of `source_name` values to include (see
  [`issue_rate_tbl()`](issue_rate_tbl.md)).

- n_strata:

  Number of popularity strata, passed to
  [`issue_rate_tbl()`](issue_rate_tbl.md). `n_strata = 1` pools every
  repo into a single, unstratified rate and drops `popularity_stratum`
  from the returned tibble.

- contrib_threshold, window:

  Passed to [`author_density_tbl()`](author_density_tbl.md).

- contrib_min:

  As in [`author_density_tbl()`](author_density_tbl.md).

- ref_date:

  Reference `Date` (first-of-month) to compare against the latest
  available month.

## Value

A tibble: `source`, `popularity_stratum`, `rate_ref`, `rate_latest`,
`step_change`, `latest_month`.

## Examples

``` r
if (FALSE) { # \dontrun{
fc <- author_density_step_change_tbl (issue_authors_tbl, repo_tbl, c ("cran", "npm"))
plot_step_change (fc, SOURCE_DISPLAY_NAME, metric = "issues") +
    ggplot2::labs (title = "Distinct authors")
} # }
```
