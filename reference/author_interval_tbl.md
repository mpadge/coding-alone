# Time elapsed between consecutive first-time authors, by source and popularity stratum

Event-level alternative to
[`new_author_rate_tbl()`](new_author_rate_tbl.md)'s repo-month rate:
instead of asking "how many new arrivals landed this month, normalised
by repo-months of exposure", this asks "how long does a repository wait
between one first-time author and the next". For one source, orders each
repository's distinct issue authors by their own first-ever appearance
in that repo's issue tracker (its founding author first, as identified
in [`new_author_rate_tbl()`](new_author_rate_tbl.md) - but *kept* here
rather than excluded, since it anchors the very first interval) and
computes, for every author after the founder, the elapsed time since the
previous first-time author's own first appearance. Each interval is
time-stamped at the *arriving* author's own issue, not at the interval's
start or midpoint, so a repository that goes quiet for two years and
then gains a new contributor logs one long interval dated to the day
that contributor actually showed up, not smeared backward across the
quiet period. Needs no repo-months exposure denominator at all - a
repository with only one author so far simply contributes no interval,
rather than a zero.

## Usage

``` r
author_interval_tbl(
  issue_authors_tbl,
  repo_tbl,
  source_name,
  n_strata = 4L,
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

- n_strata:

  Number of popularity strata.

- date_start, date_end:

  Date bounds on which authors' first appearances are considered; unlike
  the repo-month rate tables, these bound the raw event timestamps
  directly rather than a floored calendar month, and `date_end` defaults
  to [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html) (today), not
  the start of the current month, since there is no partial-month
  repo-months exposure to worry about truncating here.

## Value

A tibble with one row per (repository, author arrival after the
founder): `repo_url`, `popularity_stratum`, `arrival_index` (2 = the
first author after the founder, 3 = the second, and so on), `event_time`
(the arriving author's own first-issue timestamp - when this interval is
"logged"), `interval_days` (elapsed time, in days, since the previous
first-time author's own first appearance). Carries `source_name` as an
attribute.

## Examples

``` r
if (FALSE) { # \dontrun{
iv <- author_interval_tbl (issue_authors_tbl, repo_tbl, "pypi")
plot_author_interval (issue_authors_tbl, repo_tbl)
} # }
```
