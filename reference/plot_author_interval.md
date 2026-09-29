# Plot rolling geometric-mean wait time between consecutive first-time authors, across sources and strata

Calls [`author_interval_trend_tbl()`](author_interval_trend_tbl.md) for
every source in `POPULARITY_METRIC` and row-binds the results, then
plots each source's `window`-month trailing geometric mean of
`interval_days` over time, one line per popularity stratum, faceted by
source with a free y-scale per facet (sources sit on very different
absolute wait times, as in [`plot_cohort_age()`](plot_cohort_age.md)).

## Usage

``` r
plot_author_interval(
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
  [`author_interval_trend_tbl()`](author_interval_trend_tbl.md) call.

- start_year:

  Optional year to crop the plotted window to, as in
  [`plot_activity()`](plot_activity.md) - display-only, doesn't affect
  the underlying interval calculations.

## Value

A ggplot object.

## Examples

``` r
if (FALSE) { # \dontrun{
plot_author_interval (issue_authors_tbl, repo_tbl, SOURCE_DISPLAY_NAME)
} # }
```
