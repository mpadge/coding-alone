# Step-change in comment volume, from a reference month to now

The `issue_rate_tbl(metric = "comments")` analogue of
[`author_density_step_change_tbl()`](author_density_step_change_tbl.md):
for each of several sources, compare each popularity stratum's comment
rate (`n_comments`, normalized by repo-months of exposure) at a fixed
reference month against its most recent value, estimated from linear
regression rates.

## Usage

``` r
num_comments_step_change_tbl(
  issue_authors_tbl,
  repo_tbl,
  sources,
  n_strata = 4L,
  window = 12L,
  ref_date = as.Date("2021-01-01")
)
```

## Arguments

- issue_authors_tbl:

  As returned by [`fetch_issue_authors()`](fetch_issue_authors.md).

- repo_tbl:

  As returned by [`build_repo_tbl()`](build_repo_tbl.md).

- sources:

  Character vector of `source_name` values to include (see
  [`issue_rate_tbl()`](issue_rate_tbl.md)).

- n_strata:

  Number of popularity strata, passed to
  [`issue_rate_tbl()`](issue_rate_tbl.md). `n_strata = 1` pools every
  repo into a single, unstratified rate and drops `popularity_stratum`
  from the returned tibble.

- window:

  Passed to [`issue_rate_tbl()`](issue_rate_tbl.md).

- ref_date:

  Reference `Date` (first-of-month) to compare against the latest
  available month.

## Value

A tibble: `source`, `popularity_stratum`, `rate_ref`, `rate_latest`,
`step_change`, `latest_month`.
