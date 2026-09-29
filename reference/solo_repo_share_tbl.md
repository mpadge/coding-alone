# Share of "active" repositories interacting with exactly one distinct person, per trailing window

The most direct available operationalisation of "coding alone": among
repositories with *any* issue-based interaction in a trailing window (an
"active" repo), what fraction have that activity concentrated in a
single distinct author - core or non-core, no `contrib_threshold`
applied - rather than spread across two or more people. Unlike
[`author_density_tbl()`](author_density_tbl.md)'s per-repo-month rate,
this is a repo-level, not-normalised-by-exposure share, deliberately:
the question here isn't "how much less does each repo hear from
outsiders" but "how many repositories, out of those that hear from
anyone at all, hear from only one person" - a repo-count analogue of a
shrinking bowling league's membership rolls, not of its per-lane
activity rate.

## Usage

``` r
solo_repo_share_tbl(
  issue_authors_tbl,
  repo_tbl,
  sources,
  window = 12L,
  months = NULL,
  date_start = as.Date("2015-01-01"),
  date_end = NULL
)
```

## Arguments

- issue_authors_tbl:

  As returned by [`fetch_issue_authors()`](fetch_issue_authors.md).

- repo_tbl:

  As returned by [`build_repo_tbl()`](build_repo_tbl.md).

- sources:

  Character vector of `source_name` values to pool together (see
  [`issue_rate_tbl()`](issue_rate_tbl.md)). Repositories from every
  requested source are pooled into one combined share per month, not
  reported separately per source.

- window:

  Trailing window, in months, over which a repository's distinct authors
  are counted before it's classed as solo/non-solo. Default 12, matching
  every other trailing-window metric in this package.

- months:

  Optional `Date` vector (first-of-month) of specific months to compute
  the share for, instead of every month in `date_start:date_end` -
  useful for a cheap two-point (reference vs. latest) comparison without
  recomputing the full monthly series.

- date_start, date_end:

  Date bounds for the default monthly sequence; ignored if `months` is
  supplied. `date_end` defaults to the start of the current month.

## Value

A tibble: `month`, `n_active_repos` (repos with at least one distinct
author in the trailing window), `n_solo` (of those, repos with exactly
one), `solo_share` (`n_solo / n_active_repos`).

## Examples

``` r
if (FALSE) { # \dontrun{
solo_tbl <- solo_repo_share_tbl (issue_authors_tbl, repo_tbl, c ("cran", "npm"))
} # }
```
