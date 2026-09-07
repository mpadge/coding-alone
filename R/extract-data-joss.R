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
    issues <- github_api_get_all (
        stringr::str_glue ("/repos/{REPO}/issues"),
        query = list (labels = LABEL, state = "all")
    )
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
