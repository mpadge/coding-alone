
# peerreview

Code to analyse GitHub issue activity across open-source ecosystems
(CRAN, npm, PyPI, JOSS, rOpenSci) in relation to repository popularity
and, for rOpenSci, formal peer-review status.

## Installation

``` r
# install.packages ("remotes")
remotes::install_github ("mpadge/peerreview")
```

## Vignettes

- `vignette ("data-generation", package = "peerreview")` - the pipeline
  used to fetch and assemble the underlying repository and GitHub issue
  data.
- `vignette ("review-dividend", package = "peerreview")` - the main
  analysis: does formal peer review sustain open-source community
  engagement?
