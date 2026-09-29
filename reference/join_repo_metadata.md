# Left-join `repo_tbl`'s per-repo metadata (name, downloads, stars, source) onto an issue-authors table by `repo_url`.

Left-join `repo_tbl`'s per-repo metadata (name, downloads, stars,
source) onto an issue-authors table by `repo_url`.

## Usage

``` r
join_repo_metadata(issue_authors_tbl, repo_tbl)
```

## Arguments

- issue_authors_tbl:

  As returned by [`fetch_issue_authors()`](fetch_issue_authors.md).

- repo_tbl:

  As returned by [`build_repo_tbl()`](build_repo_tbl.md).

## Value

`issue_authors_tbl` with `repo_tbl`'s columns attached.

## Examples

``` r
issue_authors_tbl <- tibble::tibble (
    repo_url = c (
        "https://github.com/org/pkg1", "https://github.com/org/pkg2"
    ),
    issue_number = c (1L, 1L)
)
repo_tbl <- tibble::tibble (
    repo_url = c (
        "https://github.com/org/pkg1", "https://github.com/org/pkg2"
    ),
    name = c ("pkg1", "pkg2"),
    downloads = c (100, 200),
    stars = c (5, 10),
    source = c ("pypi", "npm")
)
join_repo_metadata (issue_authors_tbl, repo_tbl)
#> # A tibble: 2 × 6
#>   repo_url                    issue_number name  downloads stars source
#>   <chr>                              <int> <chr>     <dbl> <dbl> <chr> 
#> 1 https://github.com/org/pkg1            1 pkg1        100     5 pypi  
#> 2 https://github.com/org/pkg2            1 pkg2        200    10 npm   
```
