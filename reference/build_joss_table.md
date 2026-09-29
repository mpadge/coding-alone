# Build a (issue_number, title, issue_url, repo_url, language, stars, downloads) table for every JOSS submission whose review issue carries the "accepted" label.

Build a (issue_number, title, issue_url, repo_url, language, stars,
downloads) table for every JOSS submission whose review issue carries
the "accepted" label.

## Usage

``` r
build_joss_table(pypi_tbl = NULL, npm_tbl = NULL)
```

## Arguments

- pypi_tbl:

  Optional PyPI working-sample table (as written by the README.Rmd PyPI
  chunk) to match repo URLs against for `downloads`.

- npm_tbl:

  Optional npm working-sample table, likewise.

## Value

A tibble with one row per accepted JOSS submission.

## Examples

``` r
if (FALSE) { # \dontrun{
joss_tbl <- build_joss_table ()
} # }
```
