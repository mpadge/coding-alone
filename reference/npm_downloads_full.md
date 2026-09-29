# Full npm monthly download-count population (~3.77M packages), via the `download-counts` npm package: https://www.npmjs.com/package/download-counts — a single static JSON object, republished monthly, mapping package name to last-month download count. This is npm's practical equivalent of PyPI's ClickHouse/BigQuery dataset: there is no direct npm counterpart of BigQuery's public PyPI download-log dataset.

Full npm monthly download-count population (~3.77M packages), via the
`download-counts` npm package:
https://www.npmjs.com/package/download-counts — a single static JSON
object, republished monthly, mapping package name to last-month download
count. This is npm's practical equivalent of PyPI's ClickHouse/BigQuery
dataset: there is no direct npm counterpart of BigQuery's public PyPI
download-log dataset.

## Usage

``` r
npm_downloads_full()
```

## Value

A table of all npm packages.

## Examples

``` r
if (FALSE) { # \dontrun{
npm_tbl <- npm_downloads_full ()
} # }
```
