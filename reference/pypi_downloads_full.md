# Full PyPI download-count population (~870k packages, last complete calendar month), paginated in chunks of CLICKHOUSE_PAGE_SIZE. Typically ~9 requests, well under a minute, no rate limiting encountered.

Full PyPI download-count population (~870k packages, last complete
calendar month), paginated in chunks of CLICKHOUSE_PAGE_SIZE. Typically
~9 requests, well under a minute, no rate limiting encountered.

## Usage

``` r
pypi_downloads_full()
```

## Value

A table of all PyPI packages.

## Examples

``` r
if (FALSE) { # \dontrun{
pypi_tbl <- pypi_downloads_full ()
} # }
```
