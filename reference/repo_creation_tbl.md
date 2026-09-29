# Monthly repo-creation rate, by source

Counts how many repositories were created (per GitHub's own
`repo_created_at` timestamp, as recorded on `issue_authors_tbl`) each
calendar month, for one source, reported as a `window`-month trailing
sum in the same way every other rate in this package is - the
ecosystem's own raw growth in repo count over time, meant to be read
alongside [`commit_rate_tbl()`](commit_rate_tbl.md)'s per-repo-month
commit rate (see [`plot_commit_rate()`](plot_commit_rate.md)) so a
reader can judge how much of any shift in commit rate reflects more
repos existing now rather than a change in per-repo behaviour.
Restricted to the same repo population as
[`commit_rate_tbl()`](commit_rate_tbl.md) (repos with a non-`NA`
popularity metric and a known creation date), so the two panels describe
the same set of repositories.

## Usage

``` r
repo_creation_tbl(
  issue_authors_tbl,
  repo_tbl,
  source_name,
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

- window:

  Trailing aggregation window, in months. Default 12: each reported
  month's `n_metric`/`n_repo_months` sum that month and the preceding
  11.

- date_start, date_end:

  Date bounds on the analysis window; `date_end` defaults to the start
  of the current month.

## Value

A tibble with one row per month: `month`, `n_created` (`window`-month
trailing sum of repos created that month). Carries `window` and
`source_name` as attributes.

## Examples

``` r
if (FALSE) { # \dontrun{
rc <- repo_creation_tbl (issue_authors_tbl, repo_tbl, "pypi")
} # }
```
