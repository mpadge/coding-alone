# Combine output tables into a single table of repository-specific data.

Combines locally-stored tables of data for PyPI, npm, CRAN, JOSS, and
rOpenSci into a single table of names, GitHub URLs, and download/star
metrics.

## Usage

``` r
build_repo_tbl(out_dir)
```

## Arguments

- out_dir:

  Directory holding the source CSVs.

## Value

A tibble with columns `name`, `repo_url`, `downloads`, `stars`,
`source`.

## Examples

``` r
if (FALSE) { # \dontrun{
repo_tbl <- build_repo_tbl ("path/to/repo-data-out")
} # }
```
