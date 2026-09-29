# Plot repo-creation rate and commit rate, across sources

Two-panel figure: each source's percentage share of repos created (top,
from [`repo_creation_tbl()`](repo_creation_tbl.md)) and commits per
repo-month (bottom, from [`commit_rate_tbl()`](commit_rate_tbl.md)),
both `window`-month trailing sums/averages, one line per source. Unlike
[`plot_new_author_rate()`](plot_new_author_rate.md)/[`plot_author_interval()`](plot_author_interval.md),
sources aren't faceted apart and lines aren't split by popularity
stratum - all sources are overlaid on the same axes in each panel.

## Usage

``` r
plot_commit_rate(
  commit_counts_tbl,
  issue_authors_tbl,
  repo_tbl,
  source_display = NULL,
  window = 12L,
  date_start = as.Date("2015-01-01"),
  date_end = NULL,
  start_year = NULL
)
```

## Arguments

- commit_counts_tbl:

  As returned by [`fetch_repo_commits()`](fetch_repo_commits.md).

- issue_authors_tbl:

  As returned by [`fetch_issue_authors()`](fetch_issue_authors.md).

- repo_tbl:

  As returned by [`build_repo_tbl()`](build_repo_tbl.md).

- source_display:

  Named character vector as in
  [`plot_step_change()`](plot_step_change.md).

- window, date_start, date_end:

  Passed to each source's
  [`commit_rate_tbl()`](commit_rate_tbl.md)/[`repo_creation_tbl()`](repo_creation_tbl.md)
  call.

- start_year:

  Optional year to crop the plotted window to, as in
  [`plot_activity()`](plot_activity.md) - display-only, doesn't affect
  the underlying repo-months/rate calculations.

## Value

A `patchwork` object (two stacked ggplot panels).

## Details

The repo-creation panel plots each source's percentage share of that
month's total repos for each source .

## Examples

``` r
if (FALSE) { # \dontrun{
plot_commit_rate (commit_counts_tbl, issue_authors_tbl, repo_tbl, SOURCE_DISPLAY_NAME)
} # }
```
