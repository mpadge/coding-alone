# New (non-founding) authors first appearing per repo-month, by source and popularity stratum

Community-expansion analogue of
[`author_density_tbl()`](author_density_tbl.md): rather than "how many
distinct authors are active this month", counts how many people are
showing up in a repository's issue tracker *for the first time ever* - a
direct measure of whether a repo's community of interlocutors is still
growing, or has stalled to the same recurring faces. Each repo's very
first-ever issue author (typically the maintainer opening the repo's own
first issue) is excluded as a "founding" event rather than a new
arrival, since it isn't itself community growth. As with
[`issue_rate_tbl()`](issue_rate_tbl.md)/[`author_density_tbl()`](author_density_tbl.md),
the result is normalised by repo-months of exposure and reported as a
`window`-month trailing sum, so a single burst of new signups doesn't
read as a permanent step change.

## Usage

``` r
new_author_rate_tbl(
  issue_authors_tbl,
  repo_tbl,
  source_name,
  n_strata = 4L,
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

- source_name:

  One of `repo_tbl$source` (`"pypi"`, `"npm"`, `"joss"`, `"ropensci"`).

- n_strata:

  Number of popularity strata.

- window:

  Trailing aggregation window, in months. Default 12: each reported
  month's `n_metric`/`n_repo_months` sum that month and the preceding
  11.

- date_start, date_end:

  Date bounds on the analysis window; `date_end` defaults to the start
  of the current month.

## Value

A tibble with one row per (popularity stratum, month):
`popularity_stratum`, `month`, `n_metric` (trailing sum of new,
non-founding first-time authors), `n_repo_months`, `rate`. Carries
`metric = "issues"`, `window`, and `source_name` as attributes (no
`contrib_threshold`, since none applies here) so
[`plot_activity()`](plot_activity.md)/
[`plot_new_author_rate()`](plot_new_author_rate.md) don't need to be
told them again.

## Details

Author identity here isn't split by `contribution`/`contrib_threshold`
as most of this package's other rate tables are - someone who goes on to
become a heavy contributor is still a new arrival the month they first
show up, so every first-time author counts, core and non-core alike,
matching [`solo_repo_share_tbl()`](solo_repo_share_tbl.md)'s treatment
of contribution rather than [`issue_rate_tbl()`](issue_rate_tbl.md)'s.

## Examples

``` r
if (FALSE) { # \dontrun{
na <- new_author_rate_tbl (issue_authors_tbl, repo_tbl, "pypi")
plot_activity (na) + ggplot2::labs (y = "New (non-founding) authors")
} # }
```
