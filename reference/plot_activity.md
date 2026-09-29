# Plot the trailing-window rate (`issue_rate_tbl()`'s `rate` column - see its `metric` param for whether that's issues or comments per repo-month) over time, one line per popularity stratum.

Plot the trailing-window rate ([`issue_rate_tbl()`](issue_rate_tbl.md)'s
`rate` column - see its `metric` param for whether that's issues or
comments per repo-month) over time, one line per popularity stratum.

## Usage

``` r
plot_activity(rate_tbl, src_name = NULL, start_year = NULL)
```

## Arguments

- rate_tbl:

  As returned by [`issue_rate_tbl()`](issue_rate_tbl.md) - its `metric`,
  `window`, `contrib_threshold`, and `source_name` attributes are read
  straight off it, the first three to label the y-axis and `source_name`
  to annotate the plot panel directly (top-right corner), rather than
  needing to be passed in again.

- src_name:

  Name of source of `rate_tbl` to be added as plot annotation if
  specified.

- start_year:

  Optional year (e.g. `2018`) to start the plotted window from; `NULL`
  (default) plots `rate_tbl`'s full window. Only crops the display -
  `rate_tbl` isn't refetched, so this can't extend the window beyond
  what [`issue_rate_tbl()`](issue_rate_tbl.md) was already called with.

## Value

A ggplot object.

## Examples

``` r
if (FALSE) { # \dontrun{
rate_tbl <- issue_rate_tbl (issue_authors_tbl, repo_tbl, "pypi")
plot_activity (rate_tbl)
} # }
```
