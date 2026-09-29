# Compare monthly issue rate across all four sources (`pypi`, `npm`, `joss`, `ropensci`), for one popularity stratum. Note that "stratum" is relative to each source's own distribution (see `issue_rate_tbl()`/ `popularity_strata()`) - e.g. pypi's Q4 download count and joss's Q4 star count aren't the same absolute popularity, just each source's own top quarter. A source with no data yet for the requested window (e.g. not fully fetched - see `analysis-plan.md`) just contributes no line, rather than erroring.

Compare monthly issue rate across all four sources (`pypi`, `npm`,
`joss`, `ropensci`), for one popularity stratum. Note that "stratum" is
relative to each source's own distribution (see
[`issue_rate_tbl()`](issue_rate_tbl.md)/ `popularity_strata()`) - e.g.
pypi's Q4 download count and joss's Q4 star count aren't the same
absolute popularity, just each source's own top quarter. A source with
no data yet for the requested window (e.g. not fully fetched - see
`analysis-plan.md`) just contributes no line, rather than erroring.

## Usage

``` r
plot_activity_by_source(
  issue_authors_tbl,
  repo_tbl,
  stratum,
  n_strata = 4L,
  contrib_threshold = 0.01,
  metric = c("issues", "comments"),
  window = 12L,
  relative = TRUE,
  start_year = NULL,
  ros_joss_mult = 20
)
```

## Arguments

- issue_authors_tbl:

  As returned by [`fetch_issue_authors()`](fetch_issue_authors.md).

- repo_tbl:

  As returned by [`build_repo_tbl()`](build_repo_tbl.md).

- stratum:

  Integer popularity stratum to compare (`1` = lowest popularity,
  `n_strata` = highest), matching one of
  [`issue_rate_tbl()`](issue_rate_tbl.md)'s `popularity_stratum` levels
  (`"Q<stratum>"`).

- n_strata, contrib_threshold, metric, window:

  Passed to each source's [`issue_rate_tbl()`](issue_rate_tbl.md) call;
  `n_strata` must be the same one `stratum` is a level of.

- relative:

  If `TRUE` (default), rescale each source by its own mean before
  plotting - sources sit on very different absolute rate scales (e.g.
  pypi's raw issue traffic dwarfs ropensci's), which would otherwise
  squash the smaller sources' trends to flat lines near zero. Puts every
  line at a comparable "around 1 = that source's own average" scale, so
  trends are comparable even though absolute rates aren't. Set `FALSE`
  to plot absolute rates instead.

- start_year:

  Optional year (e.g. `2018`) to start the analysis from - passed
  straight through as each [`issue_rate_tbl()`](issue_rate_tbl.md)
  call's `date_start`, so it also governs the repo-months/rate
  calculations themselves, not just the plotted range. `NULL` (default)
  starts from 2015-01-01. There is no equivalent end-date control -
  analyses always run up to the current month.

- ros_joss_mult:

  Multiplier applied to the final (post-`relative`) `rate` values for
  the `"ropensci"` and `"joss"` sources only, after every other
  calculation. Default 10. This is a display-only scaling of those two
  sources relative to `"pypi"`/`"npm"` - the plot's y-axis label is
  annotated whenever it's not 1, so the scaling isn't silently hidden
  from anyone reading the plot.

## Value

A ggplot object.

## Examples

``` r
if (FALSE) { # \dontrun{
plot_activity_by_source (issue_authors_tbl, repo_tbl, stratum = 4L)
} # }
```
