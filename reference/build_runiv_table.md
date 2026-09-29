# Extract repo data for an r-universe

Build a (package, title, package_url, repo_url) table for every package
currently published in a given r-universe.

## Usage

``` r
build_runiv_table(universe = c("ropensci", "cran"))
```

## Arguments

- universe:

  Which r-universe to query: `"ropensci"` (default, the rOpenSci
  r-universe) or `"cran"` (the CRAN mirror r-universe). Only the
  rOpenSci r-universe carries software-review metadata, so `reviewed`/
  `review_id` are only present in the result when
  `universe == "ropensci"`. Also, `RemoteUrl` is only a meaningful
  repo-URL candidate for packages actually hosted on an r-universe (as
  `ropensci` packages are); for `"cran"`, where it instead reflects
  CRAN's own build infrastructure, it is excluded from the URL search.

## Value

A tibble with one row per r-universe package: `package`, `repo_url`,
`downloads` (`_downloads$count`, last-month CRAN downloads), and `stars`
(`_stars`, GitHub stargazer count); plus, for `universe == "ropensci"`
only, a `reviewed` flag (`_metadata$review$status == "reviewed"`) and,
for those, the `review_id` of its rOpenSci software review (`NA`
otherwise).

## Examples

``` r
if (FALSE) { # \dontrun{
runiv_tbl <- build_runiv_table ("ropensci")
} # }
```
