# Extract every issue (pull requests excluded) opened against a single GitHub repo, with the opener's handle and a `contribution` score: that handle's fractional share (0-1) of all commits ever landed on the repo's default branch, or 0 if the author isn't a contributor at all (see the note at the top of this file for why this is a coarser but far cheaper substitute for "was this author already a contributor at the time they opened the issue"). Also carries each issue's comment count, and the repo's own GitHub creation timestamp, used elsewhere as the start of a repo's exposure window.

Extract every issue (pull requests excluded) opened against a single
GitHub repo, with the opener's handle and a `contribution` score: that
handle's fractional share (0-1) of all commits ever landed on the repo's
default branch, or 0 if the author isn't a contributor at all (see the
note at the top of this file for why this is a coarser but far cheaper
substitute for "was this author already a contributor at the time they
opened the issue"). Also carries each issue's comment count, and the
repo's own GitHub creation timestamp, used elsewhere as the start of a
repo's exposure window.

## Usage

``` r
github_issue_authors(repo_url = NULL, since = NULL)
```

## Arguments

- repo_url:

  A GitHub repo URL, e.g. `"https://github.com/owner/repo"`.

- since:

  If not `NULL`, an ISO-8601 timestamp string restricting the GraphQL
  issues fetch to issues updated (including new comments, not just newly
  opened) at or after that time - used to refresh previously-fetched
  data rather than re-fetching every issue. The `contributors` REST call
  has no equivalent filter and is always fetched in full.

## Value

A tibble with one row per issue: `repo_url`, `issue_number`, `author`,
`created_at`, `n_comments`, `contribution`, `repo_created_at` (the
repo's own GitHub creation timestamp, repeated on every row), and
`last_updated` (this fetch's own timestamp, repeated on every row).

## Examples

``` r
if (FALSE) { # \dontrun{
issue_authors <- github_issue_authors ("https://github.com/ropensci/targets")
} # }
```
