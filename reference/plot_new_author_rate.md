# Plot new (non-founding) author arrival rate, across sources and strata

Calls [`new_author_rate_tbl()`](new_author_rate_tbl.md) for every source
in `POPULARITY_METRIC` and row-binds the results, then plots each
source's trailing-window rate over calendar time, one line per
popularity stratum, faceted by source with a free y-scale per facet
(sources sit on very different absolute rates, as in
[`plot_cohort_age()`](plot_cohort_age.md)). Unlike
[`plot_step_change()`](plot_step_change.md)/
[`plot_activity_by_source()`](plot_activity_by_source.md), this builds
its own multi-source table internally rather than taking one as `tbl` -
[`new_author_rate_tbl()`](new_author_rate_tbl.md) itself is
single-source, matching [`issue_rate_tbl()`](issue_rate_tbl.md)/
[`author_density_tbl()`](author_density_tbl.md).

## Usage

``` r
plot_new_author_rate(
  issue_authors_tbl,
  repo_tbl,
  source_display = NULL,
  n_strata = 4L,
  window = 12L,
  date_start = as.Date("2015-01-01"),
  date_end = NULL,
  start_year = NULL
)
```

## Arguments

- issue_authors_tbl:

  As returned by [`fetch_issue_authors()`](fetch_issue_authors.md).

- repo_tbl:

  As returned by [`build_repo_tbl()`](build_repo_tbl.md).

- source_display:

  Named character vector as in
  [`plot_step_change()`](plot_step_change.md).

- n_strata, window, date_start, date_end:

  Passed to each source's
  [`new_author_rate_tbl()`](new_author_rate_tbl.md) call.

- start_year:

  Optional year to crop the plotted window to, as in
  [`plot_activity()`](plot_activity.md) - display-only, doesn't affect
  the underlying repo-months/rate calculations.

## Value

A ggplot object.

## Examples

``` r
if (FALSE) { # \dontrun{
plot_new_author_rate (issue_authors_tbl, repo_tbl, SOURCE_DISPLAY_NAME)
} # }
```
