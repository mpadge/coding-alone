# Refresh previously-fetched pull-request-author data (`fetch_pr_authors()`'s checkpoint), for repos that have already been fetched at least once.

Unlike [`fetch_pr_authors()`](fetch_pr_authors.md), which skips any repo
already marked done, this re-fetches every repo in `repo_urls`, but each
repo's own `github_pr_authors()` call is scoped with `since` set to that
repo's most recent `last_updated` value already on disk - so GitHub only
returns pull requests that are new, or that changed (e.g. picked up new
comments) since that time. A repo not yet present in the checkpoint is
fetched in full, exactly as [`fetch_pr_authors()`](fetch_pr_authors.md)
would. Rows returned for an already-known pull request replace the stale
row; all other existing rows are left untouched.

## Usage

``` r
update_pr_authors(repo_urls, out_dir, batch_size = 50L)
```

## Arguments

- repo_urls:

  Character vector of repo URLs to refresh. Repos not already present in
  the `out_dir` checkpoint are fetched in full.

- out_dir:

  Directory holding the `pr-authors.csv` checkpoint written/read by
  [`fetch_pr_authors()`](fetch_pr_authors.md)/`read_pr_authors_data()`.

- batch_size:

  Repos refreshed (concurrently) per checkpoint write.

## Value

A tibble with columns `repo_url`, `pr_number`, `author`, `created_at`,
`n_comments`, `contribution`, `repo_created_at`, `last_updated` - the
full accumulated result, with refreshed repos' rows brought up to date.

## Details

Because each repo's `since` cursor advances every time it's refreshed,
this is safe to re-run (e.g. from a scheduled job) without any separate
"done" checkpoint: a run interrupted partway simply leaves the
not-yet-reached repos with an older `last_updated`, picked up as normal
on the next call. Batches are drawn via `interlace_for_even_coverage()`
rather than sequentially, so an interrupted run leaves progress spread
across `repo_urls` rather than concentrated at the top.

## Examples

``` r
repo_urls <- c (
    "https://github.com/ropensci/targets",
    "https://github.com/ropensci/drake"
)
if (FALSE) { # \dontrun{
pr_authors_tbl <- update_pr_authors (repo_urls, "path/to/repo-data-out")
} # }
```
