# Monthly commit rate per repo-month, by source

Direct, commit-history-based analogue of
[`issue_rate_tbl()`](issue_rate_tbl.md)/
[`author_density_tbl()`](author_density_tbl.md): rather than counting
issue-tracker events or distinct issue authors, counts actual commits
landed on each repo's default branch (as fetched by
[`fetch_repo_commits()`](fetch_repo_commits.md)), normalised by
repo-months of exposure and reported as a `window`-month trailing sum
over the same trailing repo-months denominator. A direct measure of
code-level activity to compare against the issue-tracker-based measures
elsewhere in this package - it can see contributors who only ever commit
and never file an issue, which
[`author_density_tbl()`](author_density_tbl.md)'s "all contributors"
reading can't (see that function's doc).

## Usage

``` r
commit_rate_tbl(
  commit_counts_tbl,
  issue_authors_tbl,
  repo_tbl,
  source_name,
  window = 12L,
  date_start = as.Date("2015-01-01"),
  date_end = NULL
)
```

## Arguments

- commit_counts_tbl:

  As returned by [`fetch_repo_commits()`](fetch_repo_commits.md) (or
  read straight from `commit-counts.csv`): one row per (repo, month)
  with `n_commits`.

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

A tibble with one row per month: `month`, `n_metric` (trailing sum of
commits), `n_repo_months`, `rate`. Carries `window` and `source_name` as
attributes.

## Details

Unlike every other rate table in this package, this one isn't split by
popularity stratum: commit rate shows no material difference between
popularity strata, so pooling all of a source's repos into one line
loses nothing a stratified version would show and is simpler to read.

As in
[`author_density_tbl()`](author_density_tbl.md)/[`new_author_rate_tbl()`](new_author_rate_tbl.md),
`repo_created_at` is read off `issue_authors_tbl` (not
`commit_counts_tbl`, which has no such column) to compute repo-months
exposure, so a repo only contributes exposure once it's been fetched at
least once by [`fetch_issue_authors()`](fetch_issue_authors.md).

## Examples

``` r
if (FALSE) { # \dontrun{
commit_counts_tbl <- readr::read_csv ("repo-data-out/commit-counts.csv")
cr <- commit_rate_tbl (commit_counts_tbl, issue_authors_tbl, repo_tbl, "pypi")
} # }
```
