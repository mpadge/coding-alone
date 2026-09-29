# Plot step-change in non-core rate across sources and strata

Bar chart of [`step_change_tbl()`](step_change_tbl.md) output: one
column per source, one bar per popularity stratum, `step_change` on a
log y-axis (so a halving and a doubling are visually symmetric) with a
reference line at 1 (no change). If `tbl` carries a `metric` column
(e.g. issues vs. comments, row-bound from two
[`step_change_tbl()`](step_change_tbl.md) calls), it is filtered down to
the single `metric` requested before plotting - one call produces one
single-metric plot, so issues and comments are two separate figures
rather than two facet rows of the same one.

## Usage

``` r
plot_step_change(
  tbl,
  source_display = NULL,
  metric = c("issues", "comments"),
  ref_date = as.Date("2021-01-01")
)
```

## Arguments

- tbl:

  As returned by [`step_change_tbl()`](step_change_tbl.md), optionally
  with an added `metric` column.

- source_display:

  Named character vector mapping internal source names to display
  labels, e.g. `SOURCE_DISPLAY_NAME`.

- metric:

  Which metric to plot: `"issues"` (default) or `"comments"`. Only used
  to filter `tbl` down to one metric when it carries a `metric` column
  (matched case-insensitively against that column's values, e.g.
  `"Issues"`/`"Comments"`); otherwise `tbl` is assumed to already be
  single-metric, and this only sets the plot title.

- ref_date:

  The same `ref_date` passed to
  [`step_change_tbl()`](step_change_tbl.md), used only to label the
  plot.

## Value

A ggplot object.

## Examples

``` r
if (FALSE) { # \dontrun{
fc <- step_change_tbl (issue_authors_tbl, repo_tbl, c ("cran", "npm"))
plot_step_change (fc, SOURCE_DISPLAY_NAME, metric = "issues")
} # }
```
