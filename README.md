
# longtail

Code to analyse GitHub repository activity in relation to “popularity”
metrics.

``` r
devtools::load_all ()
#> ℹ Loading longtail
```

## Input data

The chunks below build tables of sample repositories from PyPI and npm,
and full repo details from JOSS and rOpenSci. All data are dumped to the
`OUT_DIR` specified below. These contain repository URLs and popularity
metrics. The section after this then analyses each of the URLs to
extract data on GitHub issue activity.

For PyPI and npm, samples are stratified by download popularity (a
deterministic head of the most-downloaded packages, plus a random draw
from the long tail), filtered down to packages with resolvable GitHub
repo URLs.

## Config

``` r
WORKING_SAMPLE_TAIL_SIZE <- 40000L # random draw size, outside the known head
TOP_N_HEAD <- 15000L # deterministic head inclusion
OUT_DIR <- "repo-data-out"
dir.create (OUT_DIR, showWarnings = FALSE)
```

## PyPI

``` r
cli::cli_alert_info ("PyPI: fetching full download-count population via ClickHouse (fast)...")
downloads_tbl <- pypi_downloads_full ()

working_sample <- build_working_sample (downloads_tbl, TOP_N_HEAD, WORKING_SAMPLE_TAIL_SIZE, label = "PyPI")

cli::cli_alert_info ("PyPI: resolving GitHub repo URLs for {nrow(working_sample)} packages...")
pypi_tbl <- resolve_repo_urls (working_sample, pypi_repo_urls_many)
readr::write_csv (pypi_tbl, file.path (OUT_DIR, "pypi.csv"))
cli::cli_alert_success ("PyPI: wrote {nrow(pypi_tbl)} rows to {file.path(OUT_DIR, 'pypi.csv')}")
```

## npm

``` r
cli::cli_alert_info ("npm: fetching full download-count population via download-counts package (fast)...")
downloads_tbl <- npm_downloads_full ()

working_sample <- build_working_sample (downloads_tbl, TOP_N_HEAD, WORKING_SAMPLE_TAIL_SIZE, label = "npm")

cli::cli_alert_info ("npm: resolving GitHub repo URLs for {nrow(working_sample)} packages...")
npm_tbl <- resolve_repo_urls (working_sample, npm_repo_urls_many)
readr::write_csv (npm_tbl, file.path (OUT_DIR, "npm.csv"))
cli::cli_alert_success ("npm: wrote {nrow(npm_tbl)} rows to {file.path(OUT_DIR, 'npm.csv')}")
```

## JOSS

Accepted JOSS submissions and their repo URLs, GitHub stars (fetched via
the GraphQL API), and language label. Also matches repo URLs against the
PyPI and npm tables above (if already written to `OUT_DIR`) to fill in
`downloads` where a JOSS submission’s repo happens to also appear in one
of those.

``` r
pypi_csv <- file.path (OUT_DIR, "pypi.csv")
npm_csv <- file.path (OUT_DIR, "npm.csv")
pypi_tbl <- if (file.exists (pypi_csv)) readr::read_csv (pypi_csv, show_col_types = FALSE) else NULL
npm_tbl <- if (file.exists (npm_csv)) readr::read_csv (npm_csv, show_col_types = FALSE) else NULL

joss_tbl <- build_joss_table (pypi_tbl = pypi_tbl, npm_tbl = npm_tbl)
readr::write_csv (joss_tbl, file.path (OUT_DIR, "joss.csv"))
cli::cli_alert_success ("JOSS: wrote {nrow(joss_tbl)} rows to {file.path(OUT_DIR, 'joss.csv')}")
```

## rOpenSci

Packages in the rOpenSci r-universe, their repo URLs, and rOpenSci
software review status.

``` r
ropensci_tbl <- build_ropensci_table ()
readr::write_csv (ropensci_tbl, file.path (OUT_DIR, "ropensci.csv"))
cli::cli_alert_success ("rOpenSci: wrote {nrow(ropensci_tbl)} rows to {file.path(OUT_DIR, 'ropensci.csv')}")
```

------------------------------------------------------------------------

## Analyses

Combine the four input tables into a single table of names, GitHub URLs,
and download/star metrics, tagged with their `source`.

``` r
repo_tbl <- build_repo_tbl (OUT_DIR)
repo_tbl
#> # A tibble: 42,754 × 5
#>    name                                          repo_url downloads stars source
#>    <chr>                                         <chr>        <dbl> <dbl> <chr> 
#>  1 TanML: Automated Model Validation Toolkit fo… https:/…        NA     9 joss  
#>  2 nsEVDx: A Python library for modeling non-st… https:/…        NA    14 joss  
#>  3 priorsense: Efficient prior and likelihood s… https:/…        NA    82 joss  
#>  4 Haarpy: a Python library for Weingarten calc… https:/…        NA     8 joss  
#>  5 Bristlecone: an F# library for the long-term… https:/…        NA    12 joss  
#>  6 AlgebraOfGraphics.jl: A Makie-powererd algeb… https:/…        NA   518 joss  
#>  7 TEGAT: A Lightweight and Reusable Gateway fo… https:/…        NA     1 joss  
#>  8 ConVer-G: A Suite for Versioning, Querying a… https:/…        NA     5 joss  
#>  9 PyMC-Marketing: Bayesian Marketing Mix Model… https:/…    174377  1255 joss  
#> 10 Links and Nodes: Middleware for distributed … https:/…        NA    NA joss  
#> # ℹ 42,744 more rows
```

## GitHub issue authors

Fetch issue-author data for every repo in `repo_tbl` via
`github_issue_authors()`, which returns `repo_url` as one of its columns
directly, along with each repo’s own GitHub creation date
(`repo_created_at`, one extra cheap API call per repo) - used by the
issue-rate analysis below as the start of a repo’s exposure window.
`repo_url` is the natural unique identifier here (rather than `name`,
which is only unique within a single `source`), so `repo_tbl` is
deduplicated on it before joining, so a repo appearing under multiple
sources doesn’t fan out the join.

Calls are made sequentially in small batches. Progress is checkpointed
to disk after every batch, and repos already done are skipped on
re-running the chunk, so an interrupted run - rate-limited or
otherwise - just picks back up rather than starting over.

``` r
issue_authors_tbl <- fetch_issue_authors (repo_tbl$repo_url, OUT_DIR)
issue_authors_tbl <- join_repo_metadata (issue_authors_tbl, repo_tbl)
issue_authors_tbl
```

## Issue-rate analysis

For each fully-fetched source, compute the monthly issue-opening rate
(non-contributor issues per repo-month) stratified by popularity
(`downloads` for `npm`, `stars` for `joss`), fit the interaction model
testing whether that rate trends differently across popularity strata,
and plot it.

``` r
rate_tbl_npm <- issue_rate_tbl (issue_authors_tbl, repo_tbl, "npm")
summary (fit_activity_model (rate_tbl_npm))
plot_activity (rate_tbl_npm)
```

``` r
rate_tbl_joss <- issue_rate_tbl (issue_authors_tbl, repo_tbl, "joss")
summary (fit_activity_model (rate_tbl_joss))
plot_activity (rate_tbl_joss)
```
