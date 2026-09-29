# Fit a quasi-Poisson GLM to analyse differences in monthly issue rates across popularity strata. The `month_num:popularity_stratum` interaction tests whether the long tail trends differently from the popular head, rather than just reporting one global trend line.

Fit a quasi-Poisson GLM to analyse differences in monthly issue rates
across popularity strata. The `month_num:popularity_stratum` interaction
tests whether the long tail trends differently from the popular head,
rather than just reporting one global trend line.

## Usage

``` r
fit_activity_model(rate_tbl)
```

## Arguments

- rate_tbl:

  As returned by [`issue_rate_tbl()`](issue_rate_tbl.md).

## Value

A fitted `glm` object.

## Examples

``` r
if (FALSE) { # \dontrun{
model <- fit_activity_model (rate_tbl)
summary (model)
} # }
```
