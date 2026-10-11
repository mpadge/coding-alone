# Plot non-core rate by creation-year cohort and fixed repo age

Plot [`cohort_age_rate_tbl()`](cohort_age_rate_tbl.md) output: rate
(log10 y-axis) against creation-year cohort, one line per fixed age,
faceted by source with a free y-scale per facet (sources sit on very
different absolute rate scales, and unlike the main cross-source plots
this one isn't trying to compare sources against each other - only
cohorts within a source).

## Usage

``` r
plot_cohort_age(
  tbl,
  source_display = NULL,
  min_repo_months = 30,
  narrow = FALSE
)
```

## Arguments

- tbl:

  As returned by [`cohort_age_rate_tbl()`](cohort_age_rate_tbl.md).

- source_display:

  Named character vector as in
  [`plot_step_change()`](plot_step_change.md).

- min_repo_months:

  Cells with less exposure than this are dropped before plotting, since
  a handful of repo-months makes for an unstable rate estimate.

- narrow:

  If `TRUE`, stack all facets in a single column for narrow screens.

## Value

A ggplot object.

## Examples

``` r
if (FALSE) { # \dontrun{
cohort_tbl <- cohort_age_rate_tbl (issue_authors_tbl, repo_tbl, "cran")
plot_cohort_age (cohort_tbl, SOURCE_DISPLAY_NAME)
} # }
```
