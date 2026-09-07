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

# ---- language label extraction -------------------------------------------

# Every other label on openjournals/joss-reviews issues is either a Linguist
# language name (applied to flag the submission's primary language) or one of
# these fixed editorial-workflow/track tags. Blocklisting the fixed, known-small
# set of non-language tags is far more robust than trying to allowlist the
# open-ended set of Linguist language names.
JOSS_NON_LANGUAGE_LABELS <- c (
    "accepted", "bug", "duplicate", "enhancement", "invalid", "question", "wontfix",
    "review", "pre-review", "recommend-accept", "rejected", "waitlisted", "withdrawn",
    "published", "paused", "in-danger-of-rejection", "looking for a second reviewer",
    "out of scope", "pending-major-enhancements", "pending-minor-enhancements",
    "pre-2026-submission", "query-scope", "reviewer-completed-1st-round",
    "rOpenSci", "pyOpenSci", "RSECon26", "AAS",
    "Track: 1 (AASS)", "Track: 2 (BCM)", "Track: 3 (PE)", "Track: 4 (SBCS)",
    "Track: 5 (DSAIS)", "Track: 6 (ESE)", "Track: 7 (CSISM)", "Track: 8 (MISC)"
)

#' Pull the submission's language label(s) out of an issue's label list,
#' i.e. every label that isn't one of the fixed editorial-workflow/track tags
#' above. Multiple matches (rare) are comma-joined; none gives NA.
#' @noRd
extract_language <- function (labels) {
    label_names <- purrr::map_chr (labels, "name")
    lang <- label_names [!label_names %in% JOSS_NON_LANGUAGE_LABELS]
    if (length (lang) == 0) NA_character_ else paste (lang, collapse = ", ")
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

# ---- stargazer counts (GraphQL) -------------------------------------------

# One REST call per repo (`/repos/{owner}/{repo}`) would cost as many
# requests as there are repos. GraphQL lets many repos share a single HTTP
# request instead, by aliasing a `repository()` field per repo in one query
# - so a few thousand repos cost a few dozen requests rather than a few
# thousand. Uses `gh::gh_gql()` rather than a hand-built httr2 request, which
# also picks up GITHUB_TOKEN/GITHUB_PAT (or a stored gitcreds credential)
# automatically.
STARS_BATCH_SIZE <- 50L

#' One aliased `repository(){ stargazerCount }` field per repo, keyed by
#' `ids` (the repo's position in the overall repo_url vector) so results can
#' be scattered back into place regardless of batching.
#' @noRd
build_stars_query <- function (owners, repos, ids) {
    fields <- purrr::pmap_chr (list (owners, repos, ids), \ (owner, repo, id) {
        stringr::str_glue ('r{id}: repository(owner: "{owner}", name: "{repo}") {{ stargazerCount }}')
    })
    stringr::str_glue ("query {{ {paste (fields, collapse = ' ')} }}")
}

#' Stargazer counts for many GitHub repo URLs at once, via batched GraphQL
#' requests. Entries that aren't parseable github.com URLs, or that GraphQL
#' can't resolve (renamed/deleted/private repos), come back as NA rather
#' than failing the whole batch.
#'
#' @param repo_urls Character vector of repo URLs.
#' @param batch_size Repos per GraphQL request (aliased fields per query).
#' @return Integer vector of stargazer counts, same length/order as `repo_urls`.
#' @noRd
github_stars_many <- function (repo_urls, batch_size = STARS_BATCH_SIZE) {
    parsed <- purrr::map (repo_urls, purrr::possibly (parse_github_repo_url, otherwise = NULL))
    valid <- !vapply (parsed, is.null, logical (1))
    stars <- rep (NA_integer_, length (repo_urls))
    if (!any (valid)) {
        return (stars)
    }

    owners <- vapply (parsed [valid], `[[`, character (1), "owner")
    repos <- vapply (parsed [valid], `[[`, character (1), "repo")
    ids <- which (valid)

    batches <- split (seq_along (ids), ceiling (seq_along (ids) / batch_size))
    message ("Fetching stargazer counts for ", length (ids), " repos via GraphQL (", length (batches), " batched requests)...")

    for (b in batches) {
        body <- tryCatch (
            gh::gh_gql (build_stars_query (owners [b], repos [b], ids [b])),
            error = function (e) NULL
        )
        if (is.null (body)) {
            next
        }
        for (i in b) {
            node <- body$data [[stringr::str_glue ("r{ids[i]}")]]
            if (!is.null (node)) {
                stars [ids [i]] <- node$stargazerCount
            }
        }
    }

    stars
}

# ---- registry downloads join ----------------------------------------------

#' Left-join `downloads` onto `tbl` by matching `repo_url` against PyPI
#' and/or npm working-sample tables (see `pypi_repo_urls_many()` /
#' `npm_repo_urls_many()` and the README.Rmd chunks that build those). Where
#' a repo URL appears in both, the PyPI figure wins (arbitrary but
#' consistent tie-break); either table may be omitted.
#' @noRd
join_registry_downloads <- function (tbl, pypi_tbl = NULL, npm_tbl = NULL) {
    registry_tbl <- dplyr::bind_rows (
        pypi_tbl [, c ("repo_url", "downloads")],
        npm_tbl [, c ("repo_url", "downloads")]
    )
    if (nrow (registry_tbl) == 0) {
        tbl$downloads <- NA_integer_
        return (tbl)
    }
    registry_tbl <- dplyr::distinct (registry_tbl, repo_url, .keep_all = TRUE)
    dplyr::left_join (tbl, registry_tbl, by = "repo_url")
}

# ---- main -----------------------------------------------------------------

#' Build a (issue_number, title, issue_url, repo_url, language, stars,
#' downloads) table for every JOSS submission whose review issue carries the
#' "accepted" label.
#'
#' @param pypi_tbl Optional PyPI working-sample table (as written by the
#' README.Rmd PyPI chunk) to match repo URLs against for `downloads`.
#' @param npm_tbl Optional npm working-sample table, likewise.
#' @return A tibble with one row per accepted JOSS submission.
#' @export
build_joss_table <- function (pypi_tbl = NULL, npm_tbl = NULL) {
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
            repo_url = extract_repo_url (i$body),
            language = extract_language (i$labels)
        )
    })

    n_missing <- sum (is.na (tbl$repo_url))
    if (n_missing > 0) {
        message (stringr::str_glue (
            "Warning: {n_missing} of {nrow(tbl)} issues had no repo URL extracted."
        ))
    }

    tbl$stars <- github_stars_many (tbl$repo_url)

    join_registry_downloads (tbl, pypi_tbl, npm_tbl)
}
