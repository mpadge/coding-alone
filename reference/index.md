# Package index

## All functions

- [`SOURCE_DISPLAY_NAME`](SOURCE_DISPLAY_NAME.md) :

  Proper-cased display forms of each source's internal (lowercase)
  `POPULARITY_METRIC`/`repo_tbl$source` key, for anywhere a source name
  is shown to a reader rather than matched against data (e.g. plot
  annotations). "npm" is genuinely lowercase as a name, not an
  abbreviation, so it's left as-is.

- [`author_density_step_change_tbl()`](author_density_step_change_tbl.md)
  : Fold-change in distinct non-core author density, from a reference
  month to now

- [`author_density_tbl()`](author_density_tbl.md) : Distinct authors per
  repo-month, by source and popularity stratum

- [`author_interval_tbl()`](author_interval_tbl.md) : Time elapsed
  between consecutive first-time authors, by source and popularity
  stratum

- [`author_interval_trend_tbl()`](author_interval_trend_tbl.md) :
  Rolling geometric-mean wait time between consecutive first-time
  authors, by source and popularity stratum

- [`build_joss_table()`](build_joss_table.md) : Build a (issue_number,
  title, issue_url, repo_url, language, stars, downloads) table for
  every JOSS submission whose review issue carries the "accepted" label.

- [`build_pyos_table()`](build_pyos_table.md) : Extract repo data for
  pyOpenSci

- [`build_repo_tbl()`](build_repo_tbl.md) : Combine output tables into a
  single table of repository-specific data.

- [`build_runiv_table()`](build_runiv_table.md) : Extract repo data for
  an r-universe

- [`build_working_sample()`](build_working_sample.md) : Generate a
  working sample from a full population.

- [`cohort_age_rate_tbl()`](cohort_age_rate_tbl.md) : Non-core rate by
  creation-year cohort and fixed repo age

- [`commit_rate_tbl()`](commit_rate_tbl.md) : Monthly commit rate per
  repo-month, by source

- [`download_repo_data()`](download_repo_data.md) : Download
  pre-generated datasets from this package's GitHub release

- [`fetch_issue_authors()`](fetch_issue_authors.md) :

  Fetch issue-author data
  ([`github_issue_authors()`](../reference/github_issue_authors.md), for
  many repos, with intermediate batches dumped to disk.

- [`fetch_repo_commits()`](fetch_repo_commits.md) :

  Fetch monthly commit counts
  ([`github_commit_counts_by_month()`](../reference/github_commit_counts_by_month.md))
  for many repos, batched like
  [`fetch_issue_authors()`](../reference/fetch_issue_authors.md).

- [`fit_activity_model()`](fit_activity_model.md) :

  Fit a quasi-Poisson GLM to analyse differences in monthly issue rates
  across popularity strata. The `month_num:popularity_stratum`
  interaction tests whether the long tail trends differently from the
  popular head, rather than just reporting one global trend line.

- [`github_commit_counts_by_month()`](github_commit_counts_by_month.md)
  : Monthly commit counts on a single GitHub repo's default branch.

- [`github_issue_authors()`](github_issue_authors.md) :

  Extract every issue (pull requests excluded) opened against a single
  GitHub repo, with the opener's handle and a `contribution` score: that
  handle's fractional share (0-1) of all commits ever landed on the
  repo's default branch, or 0 if the author isn't a contributor at all
  (see the note at the top of this file for why this is a coarser but
  far cheaper substitute for "was this author already a contributor at
  the time they opened the issue"). Also carries each issue's comment
  count, and the repo's own GitHub creation timestamp, used elsewhere as
  the start of a repo's exposure window.

- [`issue_rate_tbl()`](issue_rate_tbl.md) :

  Build the (popularity stratum-x-month) issue-rate table for one
  source: a chosen `metric` from non-contributor issues, aggregated over
  a trailing rolling `window` of months and normalized by repo-months of
  exposure, where a repo's exposure begins at its GitHub creation date.
  "Non-contributor" here means `contribution <= contrib_threshold` (see
  [`github_issue_authors()`](../reference/github_issue_authors.md) for
  how `contribution` - each author's fractional share of all commits
  ever landed on the repo - is computed). `repo_created_at` lives on
  `issue_authors_tbl` (fetched alongside each repo's issues by
  [`github_issue_authors()`](../reference/github_issue_authors.md)/[`fetch_issue_authors()`](../reference/fetch_issue_authors.md)),
  not `repo_tbl`, so a repo only contributes exposure once it's been
  fetched at least once - repos with zero issues fetched (either not yet
  fetched at all, or fetched and genuinely having none) don't have a
  `repo_created_at` on file and are excluded here rather than analysed.

- [`join_repo_metadata()`](join_repo_metadata.md) :

  Left-join `repo_tbl`'s per-repo metadata (name, downloads, stars,
  source) onto an issue-authors table by `repo_url`.

- [`new_author_rate_tbl()`](new_author_rate_tbl.md) : New (non-founding)
  authors first appearing per repo-month, by source and popularity
  stratum

- [`npm_downloads_full()`](npm_downloads_full.md) :

  Full npm monthly download-count population (~3.77M packages), via the
  `download-counts` npm package:
  https://www.npmjs.com/package/download-counts — a single static JSON
  object, republished monthly, mapping package name to last-month
  download count. This is npm's practical equivalent of PyPI's
  ClickHouse/BigQuery dataset: there is no direct npm counterpart of
  BigQuery's public PyPI download-log dataset.

- [`num_comments_step_change_tbl()`](num_comments_step_change_tbl.md) :
  Step-change in comment volume, from a reference month to now

- [`plot_activity()`](plot_activity.md) :

  Plot the trailing-window rate
  ([`issue_rate_tbl()`](../reference/issue_rate_tbl.md)'s `rate`
  column - see its `metric` param for whether that's issues or comments
  per repo-month) over time, one line per popularity stratum.

- [`plot_activity_by_source()`](plot_activity_by_source.md) :

  Compare monthly issue rate across all four sources (`pypi`, `npm`,
  `joss`, `ropensci`), for one popularity stratum. Note that "stratum"
  is relative to each source's own distribution (see
  [`issue_rate_tbl()`](../reference/issue_rate_tbl.md)/
  `popularity_strata()`) - e.g. pypi's Q4 download count and joss's Q4
  star count aren't the same absolute popularity, just each source's own
  top quarter. A source with no data yet for the requested window (e.g.
  not fully fetched - see `analysis-plan.md`) just contributes no line,
  rather than erroring.

- [`plot_author_interval()`](plot_author_interval.md) : Plot rolling
  geometric-mean wait time between consecutive first-time authors,
  across sources and strata

- [`plot_cohort_age()`](plot_cohort_age.md) : Plot non-core rate by
  creation-year cohort and fixed repo age

- [`plot_commit_rate()`](plot_commit_rate.md) : Plot repo-creation rate
  and commit rate, across sources

- [`plot_new_author_rate()`](plot_new_author_rate.md) : Plot new
  (non-founding) author arrival rate, across sources and strata

- [`plot_ropensci_reviewed_step_change()`](plot_ropensci_reviewed_step_change.md)
  : Plot step-change in rOpenSci non-core rate, by review status

- [`plot_ropensci_reviewed_trend()`](plot_ropensci_reviewed_trend.md) :
  Plot rOpenSci non-core rate over time, by review status

- [`plot_step_change()`](plot_step_change.md) : Plot step-change in
  non-core rate across sources and strata

- [`pre_process_coding_alone()`](pre_process_coding_alone.md) : Function
  to pre-process data for vignettes

- [`pypi_downloads_full()`](pypi_downloads_full.md) : Full PyPI
  download-count population (~870k packages, last complete calendar
  month), paginated in chunks of CLICKHOUSE_PAGE_SIZE. Typically ~9
  requests, well under a minute, no rate limiting encountered.

- [`repo_creation_tbl()`](repo_creation_tbl.md) : Monthly repo-creation
  rate, by source

- [`resolve_repo_urls()`](resolve_repo_urls.md) : Resolve GitHub repo
  URLs for a working sample, and filter down to the packages for which
  one was found.

- [`ropensci_reviewed_activity_tbl()`](ropensci_reviewed_activity_tbl.md)
  : Non-core rate for rOpenSci packages, by review status and popularity

- [`ropensci_reviewed_step_change_tbl()`](ropensci_reviewed_step_change_tbl.md)
  : Fold-change in rOpenSci non-core rate, by review status

- [`solo_repo_share_tbl()`](solo_repo_share_tbl.md) : Share of "active"
  repositories interacting with exactly one distinct person, per
  trailing window

- [`step_change_tbl()`](step_change_tbl.md) : Fold-change in non-core
  rate, from a reference month to now

- [`update_issue_authors()`](update_issue_authors.md) :

  Refresh previously-fetched issue-author data
  ([`fetch_issue_authors()`](../reference/fetch_issue_authors.md)'s
  checkpoint), for repos that have already been fetched at least once.
