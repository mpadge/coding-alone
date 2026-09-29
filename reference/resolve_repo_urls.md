# Resolve GitHub repo URLs for a working sample, and filter down to the packages for which one was found.

Applies `repo_urls_fn` to the `name` column of `working_sample` (e.g.
`pypi_repo_urls_many()` or `npm_repo_urls_many()`), then drops rows for
which no GitHub repo URL could be resolved. Shared post-processing step
for both the PyPI and npm working samples.

## Usage

``` r
resolve_repo_urls(working_sample = NULL, repo_urls_fn = NULL)
```

## Arguments

- working_sample:

  A tibble with at least `name` and `downloads` columns, as returned by
  [`build_working_sample()`](build_working_sample.md).

- repo_urls_fn:

  A function taking a character vector of package names and returning a
  character vector of the same length, with a resolved GitHub repo URL
  or `NA` for each.

## Value

A tibble with `name`, `downloads`, and `repo_url` columns, filtered to
rows with a non-missing `repo_url`.

## Examples

``` r
working_sample <- tibble::tibble (
    name = c ("pkg1", "pkg2", "pkg3"),
    downloads = c (300, 200, 100)
)
fake_repo_urls_fn <- function (names_vec) {
    ifelse (
        names_vec == "pkg2",
        NA_character_,
        paste0 ("https://github.com/org/", names_vec)
    )
}
resolve_repo_urls (working_sample, fake_repo_urls_fn)
#> # A tibble: 2 × 3
#>   name  downloads repo_url                   
#>   <chr>     <dbl> <chr>                      
#> 1 pkg1        300 https://github.com/org/pkg1
#> 2 pkg3        100 https://github.com/org/pkg3
```
