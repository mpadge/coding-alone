# Distinct authors per repo-month, by source and popularity stratum

For one source, count *distinct* issue authors active per calendar
month.

## Usage

``` r
author_density_tbl(
  issue_authors_tbl,
  repo_tbl,
  source_name,
  n_strata = 4L,
  contrib_threshold = 0.01,
  contrib_min = -Inf,
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

- contrib_threshold:

  Issues whose author's `contribution` is at or below this value count
  as "non-contributor" issues. Default 0.01 (allow a small nonzero
  commit share and still call it "non-contributor").

- contrib_min:

  Lower bound (exclusive) on `contribution`; authors are counted when
  `contrib_min < contribution <= contrib_threshold`. Default `-Inf` (no
  lower bound, i.e. the non-core-vs-core split is governed by
  `contrib_threshold` alone, as in the original non-core headcount).

- window:

  Trailing aggregation window, in months. Default 12: each reported
  month's `n_metric`/`n_repo_months` sum that month and the preceding
  11.

- date_start, date_end:

  Date bounds on the analysis window; `date_end` defaults to the start
  of the current month.

## Value

A tibble with one row per (popularity stratum, month):
`popularity_stratum`, `month`, `n_metric` (trailing sum of distinct
authors active that month whose `contribution` fell in
`(contrib_min, contrib_threshold]`), `n_repo_months`, `rate`. Carries
`metric = "issues"`, `window`, `contrib_threshold`, and `source_name` as
attributes - `metric` is deliberately set to `"issues"` rather than
something like `"authors"` so the result can be passed straight into
[`plot_activity()`](plot_activity.md), whose y-axis label should then be
overridden (e.g. via `+ ggplot2::labs(y = ...)`) since the value isn't
actually an issue rate.

## Details

`contrib_min`/`contrib_threshold` together select which authors count,
via `contrib_min < contribution <= contrib_threshold`: the defaults
(`-Inf`, `0.01`) counts only non-core contributors. In contrast,
`contrib_min = 0.01, contrib_threshold = Inf` counts *core* authors
only.

## Examples

``` r
if (FALSE) { # \dontrun{
ad <- author_density_tbl (issue_authors_tbl, repo_tbl, "pypi")
plot_activity (ad) + ggplot2::labs (y = "Distinct non-core authors")

# All contributors, core and non-core alike:
ad_all <- author_density_tbl (
    issue_authors_tbl, repo_tbl, "pypi",
    contrib_min = -Inf, contrib_threshold = Inf
)
} # }
```
