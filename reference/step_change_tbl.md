# Fold-change in non-core rate, from a reference month to now

For each of several sources, compare each popularity stratum's trailing
12-month rate at a fixed reference month against its most recent value.
A fixed reference month (rather than each stratum's own peak) is used
deliberately: individual per-stratum peaks are noisy, especially for
sparser low-popularity strata where a single active month can dominate a
small repo-month denominator, so anchoring on one shared calendar month
both avoids cherry-picking and keeps strata/sources comparable.

## Usage

``` r
step_change_tbl(
  issue_authors_tbl,
  repo_tbl,
  sources,
  n_strata = 4L,
  metric = "issues",
  contrib_threshold = 0.01,
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

- metric, contrib_threshold, window:

  Passed to [`issue_rate_tbl()`](issue_rate_tbl.md).

- ref_date:

  Reference `Date` (first-of-month) to compare against the latest
  available month.

## Value

A tibble: `source`, `popularity_stratum`, `rate_ref`, `rate_latest`,
`step_change` (`rate_latest / rate_ref`), `latest_month`.

## Examples

``` r
if (FALSE) { # \dontrun{
fc <- step_change_tbl (issue_authors_tbl, repo_tbl, c ("cran", "npm"))
plot_step_change (fc)
} # }
```
