# Fetch monthly commit counts (`github_commit_counts_by_month()`) for many repos, batched like `fetch_issue_authors()`.

Assumes [`fetch_issue_authors()`](fetch_issue_authors.md) has already
been run, so each repo's creation timestamp can be read straight out of
its `issue-authors.csv` rather than fetched again here. A repo missing
from that file defaults back to `date_start`.

## Usage

``` r
fetch_repo_commits(
  repo_urls,
  out_dir,
  batch_size = 50L,
  date_start = as.Date("2015-01-01"),
  date_end = NULL
)
```

## Arguments

- repo_urls:

  Character vector of repo URLs to fetch issue authors for. Assumed
  already ordered by priority if it matters which get fetched first (see
  `interlace_for_even_coverage()`).

- out_dir:

  Directory to read/write the CSV + done-list checkpoint files.

- batch_size:

  Repos fetched (concurrently) per checkpoint.

- date_start, date_end:

  Passed to
  [`github_commit_counts_by_month()`](github_commit_counts_by_month.md).

## Value

A tibble with columns `repo_url`, `month`, `n_commits` - the full
accumulated result, including rows from any previous run(s).

## Examples

``` r
repo_urls <- c (
    "https://github.com/ropensci/targets",
    "https://github.com/ropensci/drake"
)
if (FALSE) { # \dontrun{
commit_counts_tbl <- fetch_repo_commits (repo_urls, "path/to/repo-data-out")
} # }
```
