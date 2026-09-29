# Monthly commit counts on a single GitHub repo's default branch.

This is meant to run after issue-author data has already been fetched
([`fetch_issue_authors()`](fetch_issue_authors.md)).

## Usage

``` r
github_commit_counts_by_month(
  repo_url = NULL,
  repo_created_at = NULL,
  date_start = as.Date("2015-01-01"),
  date_end = NULL
)
```

## Arguments

- repo_url:

  A GitHub repo URL, e.g. `"https://github.com/owner/repo"`.

- repo_created_at:

  The repo's own creation timestamp (as recorded in
  `issue-authors.csv`'s `repo_created_at` column), used to raise
  `date_start` up to the month the repo actually came into existence.
  `NULL` defaults to \`date_start .

- date_start, date_end:

  Date bounds on the monthly sequence, raised to the repo's creation
  month if `repo_created_at` is later; `date_end` defaults to the start
  of the current month.

## Value

A tibble with one row per month: `repo_url`, `month`, `n_commits`,
trimmed of any leading/trailing zero-commit months. Zero rows if the
repo's creation month is after `date_end`, or it has no commits at all
in range.

## Examples

``` r
if (FALSE) { # \dontrun{
commits <- github_commit_counts_by_month ("https://github.com/ropensci/targets")
} # }
```
