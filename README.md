
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
source_patterns <- c (pypi = "pypi", npm = "npm", joss = "joss", ropensci = "ropensci")

source_for_file <- function (path) {
    fname <- fs::path_file (path)
    hit <- vapply (source_patterns, grepl, x = fname, FUN.VALUE = logical (1))
    names (source_patterns) [hit]
}

flist <- fs::dir_ls (OUT_DIR, glob = "*.csv")
flist <- flist [lengths (lapply (flist, source_for_file)) > 0]

read_one <- function (path) {
    tbl <- readr::read_csv (path, show_col_types = FALSE)
    name_col <- intersect (c ("name", "package", "title"), names (tbl)) [1]

    name <- tbl [[name_col]]
    if (name_col == "title") {
        name <- sub ("^\\[REVIEW\\]:\\s*", "", name)
    }

    tibble::tibble (
        name = name,
        repo_url = tbl$repo_url,
        downloads = if ("downloads" %in% names (tbl)) tbl$downloads else NA_integer_,
        stars = if ("stars" %in% names (tbl)) tbl$stars else NA_integer_,
        source = source_for_file (path)
    )
}

repo_tbl <- purrr::map_dfr (flist, read_one)
repo_tbl
#> # A tibble: 46,460 × 5
#>    name                                          repo_url downloads stars source
#>    <chr>                                         <chr>        <dbl> <dbl> <chr> 
#>  1 TanML: Automated Model Validation Toolkit fo… https:/…        NA    NA joss  
#>  2 nsEVDx: A Python library for modeling non-st… https:/…        NA    NA joss  
#>  3 priorsense: Efficient prior and likelihood s… https:/…        NA    NA joss  
#>  4 Haarpy: a Python library for Weingarten calc… https:/…        NA    NA joss  
#>  5 Bristlecone: an F# library for the long-term… https:/…        NA    NA joss  
#>  6 AlgebraOfGraphics.jl: A Makie-powererd algeb… https:/…        NA    NA joss  
#>  7 TEGAT: A Lightweight and Reusable Gateway fo… https:/…        NA    NA joss  
#>  8 ConVer-G: A Suite for Versioning, Querying a… https:/…        NA    NA joss  
#>  9 PyMC-Marketing: Bayesian Marketing Mix Model… https:/…        NA    NA joss  
#> 10 Links and Nodes: Middleware for distributed … https:/…        NA    NA joss  
#> # ℹ 46,450 more rows
```

## GitHub issue authors

Fetch issue-author data for every repo in `repo_tbl` via
`github_issue_authors()` (which now returns `repo_url` as one of its
columns directly). `repo_url` is the natural unique identifier here
(rather than `name`, which is only unique within a single `source`), so
`repo_tbl` is deduplicated on it before joining, so a repo appearing
under multiple sources doesn’t fan out the join.

Calls are made sequentially in small batches. Progress is checkpointed
to disk after every batch, and repos already done are skipped on
re-running the chunk, so an interrupted run - rate-limited or
otherwise - just picks back up rather than starting over.

``` r
library (progressify)
handlers (global = TRUE)

BATCH_SIZE <- 50L
issue_authors_csv <- file.path (OUT_DIR, "issue-authors.csv")
issue_authors_done_rds <- file.path (OUT_DIR, "issue-authors-done.rds")

issue_authors_col_types <- readr::cols (
    repo_url = readr::col_character (),
    issue_number = readr::col_integer (),
    author = readr::col_character (),
    created_at = readr::col_character (),
    is_contributor = readr::col_logical ()
)
issue_authors_tbl <- if (file.exists (issue_authors_csv)) {
    readr::read_csv (issue_authors_csv, col_types = issue_authors_col_types)
} else {
    tibble::tibble (
        repo_url = character (), issue_number = integer (),
        author = character (), created_at = character (), is_contributor = logical ()
    )
}
repo_urls_done <- if (file.exists (issue_authors_done_rds)) readRDS (issue_authors_done_rds) else character ()

repo_urls <- unique (repo_tbl$repo_url)
repo_urls_todo <- setdiff (repo_urls, repo_urls_done)
cli::cli_alert_info (
    "Issue authors: {length (repo_urls_done)} of {length (repo_urls)} repos already done, {length (repo_urls_todo)} remaining..."
)

get_issue_authors_safe <- function (repo_url) {
    tryCatch (
        github_issue_authors (repo_url),
        error = function (e) {
            cli::cli_alert_warning ("Issue authors: failed for {repo_url}: {conditionMessage (e)}")
            tibble::tibble (repo_url = repo_url) [0, ]
        }
    )
}

batches <- split (repo_urls_todo, ceiling (seq_along (repo_urls_todo) / BATCH_SIZE))
for (b in seq_along (batches)) {
    batch <- batches [[b]]
    cli::cli_alert_info ("Issue authors: batch {b}/{length (batches)} ({length (batch)} repos)...")

    batch_tbl <- lapply (batch, get_issue_authors_safe) |>
        progressify () |>
        futurize::futurize ()
    batch_tbl <- purrr::map_dfr (batch, get_issue_authors_safe)
    issue_authors_tbl <- dplyr::bind_rows (issue_authors_tbl, batch_tbl)
    repo_urls_done <- c (repo_urls_done, batch)

    readr::write_csv (issue_authors_tbl, issue_authors_csv)
    saveRDS (repo_urls_done, issue_authors_done_rds)
}
cli::cli_alert_success ("Issue authors: wrote {nrow(issue_authors_tbl)} rows to {issue_authors_csv}")

repo_tbl_unique <- dplyr::distinct (repo_tbl, repo_url, .keep_all = TRUE)
issue_authors_tbl <- dplyr::left_join (issue_authors_tbl, repo_tbl_unique, by = "repo_url")
issue_authors_tbl
```
