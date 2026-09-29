# Generate a working sample from a full population.

Build a working sample from a full (name, downloads) population,
stratified by download popularity: a deterministic head of the
`top_n_head` most-downloaded packages, plus a random draw of up to
`tail_size` packages from the remaining long tail.

## Usage

``` r
build_working_sample(
  downloads_tbl = NULL,
  top_n_head = 15000L,
  tail_size = 40000L,
  label = NULL
)
```

## Arguments

- downloads_tbl:

  A tibble with at least `name` and `downloads` columns, as returned by
  [`pypi_downloads_full()`](pypi_downloads_full.md) or
  [`npm_downloads_full()`](npm_downloads_full.md).

- top_n_head:

  Number of most-downloaded packages to include deterministically.

- tail_size:

  Number of packages to randomly draw from the remaining long tail
  (outside the head). Capped at the size of that tail.

- label:

  Optional string used to prefix a `cli` status message (e.g. `"PyPI"`
  or `"npm"`); no message is printed if `NULL`.

## Value

A tibble combining the head and tail samples, with the same columns as
`downloads_tbl`.

## Examples

``` r
downloads_tbl <- tibble::tibble (
    name = paste0 ("pkg", seq_len (100)),
    downloads = round (100 * exp (-seq_len (100) / 15))
)
sample_tbl <- build_working_sample (
    downloads_tbl,
    top_n_head = 10L,
    tail_size = 20L
)
nrow (sample_tbl)
#> [1] 30
```
