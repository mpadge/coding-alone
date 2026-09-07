# Functions to extract (issue_number, title, repo_url) for every JOSS
# submission whose review issue at github.com/openjournals/joss-reviews
# carries the "accepted" label. See README.Rmd for the script that drives
# these to actually build the table.
#
# The issue body is NOT real YAML frontmatter, despite looking like a
# metadata header — it's a fixed block of markdown **Bold:** lines. The
# template has changed over JOSS's history, so the Repository line shows up
# in two different shapes depending on submission date:
#
#   - newer issues: value bracketed by matched HTML comments
#     **Repository:** <!--target-repository-->URL<!--end-target-repository-->
#   - older issues: value inside an <a href="URL">URL</a> anchor
#     **Repository:** <a href="URL" target="_blank">URL</a>
#
# Both are handled, with a bare-URL fallback for anything else.

REPO <- "openjournals/joss-reviews"
LABEL <- "accepted"
PER_PAGE <- 100L

# ---- GitHub API access --------------------------------------------------

#' GITHUB_TOKEN/GITHUB_PAT if set, else NA (falls back to unauthenticated
#' requests: 60/hour, vs 5000/hour authenticated). ~3,700 accepted issues at
#' 100/page is ~38 requests, which just fits unauthenticated with no retry
#' margin — a token is recommended but not required.
#' @noRd
github_token <- function () {
    tok <- Sys.getenv ("GITHUB_TOKEN", Sys.getenv ("GITHUB_PAT", ""))
    if (nzchar (tok)) tok else NA_character_
}

#' @noRd
fetch_issues_page <- function (page, token) {
    req <- httr2::request (stringr::str_glue ("https://api.github.com/repos/{REPO}/issues")) |>
        httr2::req_url_query (labels = LABEL, state = "all", per_page = PER_PAGE, page = page) |>
        httr2::req_headers (Accept = "application/vnd.github+json", `User-Agent` = "joss-repos.R") |>
        httr2::req_retry (max_tries = 5, backoff = \ (i) 2^i)
    if (!is.na (token)) req <- httr2::req_headers (req, Authorization = stringr::str_glue ("Bearer {token}"))
    httr2::req_perform (req)
}

#' Sleep until quota resets if we're about to run out, rather than erroring.
#' @noRd
respect_rate_limit <- function (resp) {
    remaining <- suppressWarnings (as.numeric (httr2::resp_header (resp, "x-ratelimit-remaining")))
    if (!is.na (remaining) && remaining <= 1) {
        reset_at <- suppressWarnings (as.numeric (httr2::resp_header (resp, "x-ratelimit-reset")))
        wait <- max (0, reset_at - as.numeric (Sys.time ())) + 2
        message (stringr::str_glue ("Rate limit nearly exhausted, waiting {round(wait)}s for reset..."))
        Sys.sleep (wait)
    }
}

#' All issues labeled "accepted" (raw parsed GitHub issue objects), paginated.
#' @noRd
fetch_all_accepted_issues <- function () {
    token <- github_token ()
    if (is.na (token)) {
        message (
            "No GITHUB_TOKEN/GITHUB_PAT set - using unauthenticated requests ",
            "(60/hour limit). This run needs ~38 requests, so it fits but with ",
            "no margin; set a token in the environment to avoid ever waiting."
        )
    }

    pages <- list ()
    page <- 1L
    repeat {
        resp <- fetch_issues_page (page, token)
        body <- httr2::resp_body_json (resp, simplifyVector = FALSE)
        if (length (body) == 0) break
        pages [[length (pages) + 1]] <- body
        message (stringr::str_glue ("Fetched page {page} ({length(body)} issues)"))
        respect_rate_limit (resp)
        if (length (body) < PER_PAGE) break
        page <- page + 1L
    }
    purrr::flatten (pages)
}

# ---- repo URL extraction -------------------------------------------------

repo_url_comment_re <- stringr::regex (
    "<!--target-repository-->\\s*(.*?)\\s*<!--end-target-repository-->",
    dotall = TRUE
)
repo_url_anchor_re <- "\\*\\*Repository:\\*\\*[^\\n]*?href=[\"']([^\"']+)[\"']"
repo_url_bare_re <- "\\*\\*Repository:\\*\\*\\s*([^\\s<]+)"

#' Pull the Repository: URL out of a JOSS review issue body, trying the
#' newer HTML-comment-delimited form first, then the older anchor-tag form,
#' then a bare-URL fallback. Returns NA if none match.
#' @noRd
extract_repo_url <- function (body) {
    if (is.null (body) || is.na (body)) {
        return (NA_character_)
    }
    for (re in list (repo_url_comment_re, repo_url_anchor_re, repo_url_bare_re)) {
        m <- stringr::str_match (body, re)
        if (!is.na (m [1, 2])) {
            return (stringr::str_trim (m [1, 2]))
        }
    }
    NA_character_
}

# ---- main -----------------------------------------------------------------

#' Build a (issue_number, title, issue_url, repo_url) table for every JOSS
#' submission whose review issue carries the "accepted" label.
#'
#' @return A tibble with one row per accepted JOSS submission.
#' @export
build_joss_table <- function () {
    message ("Fetching all '", LABEL, "'-labeled issues from ", REPO, "...")
    issues <- fetch_all_accepted_issues ()
    issues <- purrr::keep (issues, \ (i) is.null (i$pull_request)) # this repo shouldn't have any, but be safe

    message ("Extracting repo URLs from ", length (issues), " issue bodies...")
    tbl <- purrr::map_dfr (issues, \ (i) {
        tibble::tibble (
            issue_number = i$number,
            title = i$title,
            issue_url = i$html_url,
            repo_url = extract_repo_url (i$body)
        )
    })

    n_missing <- sum (is.na (tbl$repo_url))
    if (n_missing > 0) {
        message (stringr::str_glue (
            "Warning: {n_missing} of {nrow(tbl)} issues had no repo URL extracted."
        ))
    }
    tbl
}
