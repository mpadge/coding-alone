# Build the (popularity stratum-x-month) issue-rate table for one source: a chosen `metric` from non-contributor issues, aggregated over a trailing rolling `window` of months and normalized by repo-months of exposure, where a repo's exposure begins at its GitHub creation date. "Non-contributor" here means `contribution <= contrib_threshold` (see `github_issue_authors()` for how `contribution` - each author's fractional share of all commits ever landed on the repo - is computed). `repo_created_at` lives on `issue_authors_tbl` (fetched alongside each repo's issues by `github_issue_authors()`/`fetch_issue_authors()`), not `repo_tbl`, so a repo only contributes exposure once it's been fetched at least once - repos with zero issues fetched (either not yet fetched at all, or fetched and genuinely having none) don't have a `repo_created_at` on file and are excluded here rather than analysed.

Each reported month is a trailing aggregate over `window` months (that
month and the `window - 1` preceding it), not a single month's own
count - smoothing month-to-month noise at the cost of some lag, and of
treating the `window - 1` months at the very start of the series as a
shorter, partial window rather than dropping them.

## Usage

``` r
issue_rate_tbl(
  issue_authors_tbl,
  repo_tbl,
  source_name,
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

- source_name:

  One of `repo_tbl$source` (`"pypi"`, `"npm"`, `"joss"`, `"ropensci"`).

- n_strata:

  Number of popularity strata.

- contrib_threshold:

  Issues whose author's `contribution` is at or below this value count
  as "non-contributor" issues. Default 0.01 (allow a small nonzero
  commit share and still call it "non-contributor").

- metric:

  Which per-issue quantity to aggregate: `"issues"` (default) counts
  qualifying issues; `"comments"` sums qualifying issues' `n_comments`
  instead.

- window:

  Trailing aggregation window, in months. Default 12: each reported
  month's `n_metric`/`n_repo_months` sum that month and the preceding
  11.

- date_start, date_end:

  Date bounds on the analysis window; `date_end` defaults to the start
  of the current month.

## Value

A tibble with one row per (popularity stratum, month):
`popularity_stratum`, `month` (Date, first-of-month), `n_metric`,
`n_repo_months`, `rate` - the latter two already `window`-month trailing
sums, not single-month counts. Also carries `metric`, `window`,
`contrib_threshold`, and `source_name` as attributes, so
[`plot_activity()`](plot_activity.md) can label its y-axis correctly
without being told them again.

## Examples

``` r
if (FALSE) { # \dontrun{
rate_tbl <- issue_rate_tbl (issue_authors_tbl, repo_tbl, "pypi")
} # }
```
