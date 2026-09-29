# Download pre-generated datasets from this package's GitHub release

Download is only triggered if `out_dir` does not already exist.

## Usage

``` r
download_repo_data(
  out_dir = "repo-data-out",
  repo = "mpadge/coding-alone",
  tag = "v0.1"
)
```

## Arguments

- out_dir:

  Directory to create and download data into. Default `"repo-data-out"`
  is used in all data-reading functions in this package.

- repo:

  GitHub `owner/repo` to download release assets from.

- tag:

  Release tag holding the data assets.

## Value

`out_dir`, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
download_repo_data ()
} # }
```
