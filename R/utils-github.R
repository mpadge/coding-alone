github_url_re <- "github\\.com[/:]([^/\\s\"']+)/([^/\\s\"'#]+)"
github_shorthand_re <- "^github:([^/\\s\"']+)/([^/\\s\"'#]+)$"

#' Normalize any github.com URL, or a "github:owner/repo" shorthand
#' (npm's `repository` field allows this), to https://github.com/<owner>/<repo>
#' @noRd
normalize_github_url <- function(url) {
    if (is.null(url) || is.na(url) || !nzchar(url)) {
        return(NA_character_)
    }
    m <- stringr::str_match(url, github_url_re)
    if (is.na(m[1, 1])) {
        m <- stringr::str_match(url, github_shorthand_re)
    }
    if (is.na(m[1, 1])) {
        return(NA_character_)
    }
    owner <- m[1, 2]
    repo <- stringr::str_remove(m[1, 3], "\\.git$")
    stringr::str_glue("https://github.com/{owner}/{repo}")
}

#' First github.com URL found among a set of candidate URL strings, or NA.
#' @noRd
find_github_url <- function(candidates) {
    candidates <- candidates[!vapply(candidates, is.null, logical(1))]
    candidates <- unlist(candidates, use.names = FALSE)
    for (url in candidates) {
        norm <- normalize_github_url(url)
        if (!is.na(norm)) {
            return(norm)
        }
    }
    NA_character_
}
