github_url_re <- "github\\.com[/:]([^/\\s\"']+)/([^/\\s\"'#]+)"
github_shorthand_re <- "^github:([^/\\s\"']+)/([^/\\s\"'#]+)$"

#' Normalize any github.com URL, or a "github:owner/repo" shorthand
#' (npm's `repository` field allows this), to https://github.com/<owner>/<repo>
#' @noRd
normalize_github_url <- function (url) {

    if (is.null (url) || is.na (url) || !nzchar (url)) {
        return (NA_character_)
    }
    m <- stringr::str_match (url, github_url_re)
    if (is.na (m [1, 1])) {
        m <- stringr::str_match (url, github_shorthand_re)
    }
    if (is.na (m [1, 1])) {
        return (NA_character_)
    }

    owner <- m [1, 2]
    repo <- stringr::str_remove (m [1, 3], "\\.git$")

    stringr::str_glue ("https://github.com/{owner}/{repo}")
}

#' First github.com URL found among a set of candidate URL strings, or NA.
#' @noRd
find_github_url <- function (candidates) {

    candidates <- candidates [!vapply (candidates, is.null, logical (1))]
    candidates <- unlist (candidates, use.names = FALSE)

    for (url in candidates) {
        norm <- normalize_github_url (url)
        if (!is.na (norm)) {
            return (norm)
        }
    }

    NA_character_
}

#' Split a github.com repo URL into its `owner` and `repo` parts.
#' @noRd
parse_github_repo_url <- function (url) {
    m <- stringr::str_match (url, github_url_re)

    if (is.na (m [1, 1])) {
        stop ("Not a github.com repo URL: ", url, call. = FALSE)
    }

    list (owner = m [1, 2], repo = stringr::str_remove (m [1, 3], "\\.git$"))
}

# ---- GitHub REST API access ----------------------------------------------

#' GITHUB_TOKEN/GITHUB_PAT if set, else NA (falls back to unauthenticated
#' requests: 60/hour, vs 5000/hour authenticated).
#' @noRd
github_token <- function () {

    tok <- Sys.getenv ("GITHUB_TOKEN", Sys.getenv ("GITHUB_PAT", ""))
    if (nzchar (tok)) tok else NA_character_
}

#' Sleep until quota resets if we're about to run out, rather than erroring.
#' @noRd
github_respect_rate_limit <- function (resp) {
    remaining <- suppressWarnings (as.numeric (
        httr2::resp_header (resp, "x-ratelimit-remaining")
    ))

    if (!is.na (remaining) && remaining <= 1) {
        reset_at <- suppressWarnings (as.numeric (
            httr2::resp_header (resp, "x-ratelimit-reset")
        ))
        wait <- max (0, reset_at - as.numeric (Sys.time ())) + 2
        message (stringr::str_glue (
            "Rate limit nearly exhausted, waiting {round(wait)}s for reset..."
        ))
        Sys.sleep (wait)
    }
}

#' Fetch every page of a paginated GitHub REST API list endpoint (e.g.
#' `/repos/{owner}/{repo}/issues`), and return the concatenated list of
#' parsed JSON items across all pages. `query` carries any extra query
#' parameters (e.g. `list(state = "all", labels = "accepted")`).
#' @noRd
github_api_get_all <- function (path,
                                query = list (),
                                per_page = 100L,
                                token = github_token ()) {
    if (is.na (token)) {
        message (
            "No GITHUB_TOKEN/GITHUB_PAT set - using unauthenticated requests ",
            "(60/hour limit). Set a token in the environment to raise this ",
            "to 5000/hour."
        )
    }

    items <- list ()
    page <- 1L

    repeat {

        req <- httr2::request (
            stringr::str_glue ("https://api.github.com{path}")
        ) |>
            httr2::req_headers (
                Accept = "application/vnd.github+json",
                `User-Agent` = "codingAlone-R-package"
            ) |>
            httr2::req_retry (max_tries = 5, backoff = \ (i) 2^i)
        req <- do.call (
            httr2::req_url_query,
            c (list (req), query, list (per_page = per_page, page = page))
        )

        if (!is.na (token)) {
            req <- httr2::req_headers (
                req,
                Authorization = stringr::str_glue ("Bearer {token}")
            )
        }
        resp <- httr2::req_perform (req)

        body <- httr2::resp_body_json (resp, simplifyVector = FALSE)
        if (length (body) == 0) break
        items [[length (items) + 1]] <- body
        github_respect_rate_limit (resp)
        if (length (body) < per_page) break

        page <- page + 1L
    }

    purrr::flatten (items)
}
