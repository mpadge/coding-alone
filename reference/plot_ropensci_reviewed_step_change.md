# Plot step-change in rOpenSci non-core rate, by review status

Bar chart of
[`ropensci_reviewed_step_change_tbl()`](ropensci_reviewed_step_change_tbl.md)
output: one bar for reviewed, one for non-reviewed, faceted by
popularity stratum. If `tbl` carries a `metric` column (e.g. issues vs.
comments, row-bound from two
[`ropensci_reviewed_step_change_tbl()`](ropensci_reviewed_step_change_tbl.md)
calls), it is filtered down to the single `metric` requested before
plotting - one call produces one single-metric plot, so issues and
comments are two separate figures rather than two facet rows of the same
one.

## Usage

``` r
plot_ropensci_reviewed_step_change(
  tbl,
  metric = c("issues", "comments"),
  ref_date = as.Date("2021-01-01")
)
```

## Arguments

- tbl:

  As returned by
  [`ropensci_reviewed_step_change_tbl()`](ropensci_reviewed_step_change_tbl.md),
  optionally with an added `metric` column.

- metric:

  Which metric to plot: `"issues"` (default) or `"comments"`. Only used
  to filter `tbl` down to one metric when it carries a `metric` column
  (matched case-insensitively against that column's values, e.g.
  `"Issues"`/`"Comments"`); otherwise `tbl` is assumed to already be
  single-metric, and this only sets the plot title.

- ref_date:

  As in [`plot_step_change()`](plot_step_change.md), used only to label
  the plot.

## Value

A ggplot object.

## Examples

``` r
if (FALSE) { # \dontrun{
ros <- ropensci_reviewed_activity_tbl (issue_authors_tbl, repo_tbl, ropensci_raw)
ros_fc <- ropensci_reviewed_step_change_tbl (ros)
plot_ropensci_reviewed_step_change (ros_fc, metric = "issues")
} # }
```
