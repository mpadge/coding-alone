# Non-core rate for rOpenSci packages, by review status and popularity

Per-(popularity stratum x reviewed-status x month) non-core rate for
rOpenSci repositories only - the same trailing-window,
exposure-normalised construction as
[`issue_rate_tbl()`](issue_rate_tbl.md), but with formal-review status
(from rOpenSci's own package metadata) crossed with popularity stratum
instead of stratifying by popularity alone. Repos with no recorded
`reviewed` status are dropped rather than silently pooled into either
group.

## Usage

``` r
ropensci_reviewed_activity_tbl(
  issue_authors_tbl,
  repo_tbl,
  ropensci_raw,
  n_strata = 4L,
  contrib_threshold = 0.01,
  metric = c("issues", "comments"),
  window = 12L,
  date_start = as.Date("2015-01-01"),
  date_end = NULL
)
```

## Arguments

- issue_authors_tbl:

  As returned by [`fetch_issue_authors()`](fetch_issue_authors.md).

- repo_tbl:

  As returned by [`build_repo_tbl()`](build_repo_tbl.md).

- ropensci_raw:

  A tibble of rOpenSci package metadata with at least `repo_url` and
  `reviewed` (logical) columns, e.g. rOpenSci's own
  `packages.csv`/`ropensci.csv` listing.

- n_strata, contrib_threshold, metric, window, date_start, date_end:

  As in [`issue_rate_tbl()`](issue_rate_tbl.md).

## Value

A tibble: `popularity_stratum`, `reviewed`, `month`, `n_metric`,
`n_repo_months`, `rate`. Carries `metric`/`window`/`contrib_threshold`
as attributes, as [`issue_rate_tbl()`](issue_rate_tbl.md) does.

## Examples

``` r
if (FALSE) { # \dontrun{
ros <- ropensci_reviewed_activity_tbl (issue_authors_tbl, repo_tbl, ropensci_raw)
plot_ropensci_reviewed_trend (ros)
} # }
```
