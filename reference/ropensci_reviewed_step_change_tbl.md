# Fold-change in rOpenSci non-core rate, by review status

Fold-change (latest 12-month rate vs. a fixed reference month) computed
from
[`ropensci_reviewed_activity_tbl()`](ropensci_reviewed_activity_tbl.md)
output, by (popularity stratum x reviewed status) rather than by (source
x popularity stratum) as in [`step_change_tbl()`](step_change_tbl.md).

## Usage

``` r
ropensci_reviewed_step_change_tbl(tbl, ref_date = as.Date("2021-01-01"))
```

## Arguments

- tbl:

  As returned by
  [`ropensci_reviewed_activity_tbl()`](ropensci_reviewed_activity_tbl.md).
  If it was built with `n_strata = 1`, `popularity_stratum` is dropped
  from the returned tibble too, matching
  [`step_change_tbl()`](step_change_tbl.md).

- ref_date:

  As in [`step_change_tbl()`](step_change_tbl.md).

## Value

A tibble: `popularity_stratum`, `reviewed`, `rate_ref`, `rate_latest`,
`step_change`, `latest_month`.

## Examples

``` r
if (FALSE) { # \dontrun{
ros <- ropensci_reviewed_activity_tbl (issue_authors_tbl, repo_tbl, ropensci_raw)
ros_fc <- ropensci_reviewed_step_change_tbl (ros)
plot_ropensci_reviewed_step_change (ros_fc)
} # }
```
