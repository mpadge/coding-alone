# Plot rOpenSci non-core rate over time, by review status

Time-trend line plot of
[`ropensci_reviewed_activity_tbl()`](ropensci_reviewed_activity_tbl.md)
output: rate over calendar time, one line for reviewed and one for
non-reviewed repos, faceted by popularity stratum (columns) and, if
`tbl` carries a `metric` column (issues vs. comments, row-bound from two
calls), by metric (rows) as well.

## Usage

``` r
plot_ropensci_reviewed_trend(tbl, start_year = NULL)
```

## Arguments

- tbl:

  As returned by
  [`ropensci_reviewed_activity_tbl()`](ropensci_reviewed_activity_tbl.md),
  optionally row-bound across metrics with an added `metric` column.

- start_year:

  Optional year to crop the plotted window to.

## Value

A ggplot object.

## Examples

``` r
if (FALSE) { # \dontrun{
ros <- ropensci_reviewed_activity_tbl (issue_authors_tbl, repo_tbl, ropensci_raw)
plot_ropensci_reviewed_trend (ros, start_year = 2016)
} # }
```
