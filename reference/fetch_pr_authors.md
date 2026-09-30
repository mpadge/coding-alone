# Fetch pull-request-author data (`github_pr_authors()`, for many repos, with intermediate batches dumped to disk.

Completed repos are tracked in `<out_dir>/pr-authors-done.rds`, and that
file is read on re-start. Each batch is fetched concurrently via
`progressify`/`futurize`).

## Usage

``` r
fetch_pr_authors(repo_urls, out_dir, batch_size = 50L)
```

## Arguments

- repo_urls:

  Character vector of repo URLs to fetch pull-request authors for.
  Assumed already ordered by priority if it matters which get fetched
  first (see `interlace_for_even_coverage()`).

- out_dir:

  Directory to read/write the CSV + done-list checkpoint files.

- batch_size:

  Repos fetched (concurrently) per checkpoint.

## Value

A tibble with columns `repo_url`, `pr_number`, `author`, `created_at`,
`n_comments`, `contribution`, `repo_created_at`, `last_updated` - the
full accumulated result, including rows from any previous run(s).

## Details

Batches are drawn via `interlace_for_even_coverage()` rather than
sequentially, so that the fetched subsample stays evenly spread across
`repo_urls`'s intrinsic order.

## Examples

``` r
repo_urls <- c (
    "https://github.com/ropensci/targets",
    "https://github.com/ropensci/drake"
)
if (FALSE) { # \dontrun{
pr_authors_tbl <- fetch_pr_authors (repo_urls, "path/to/repo-data-out")
} # }
```
