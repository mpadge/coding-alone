# Functions to extract every issue opened against a single GitHub repo,
# together with a cheap "is this person a primary contributor" flag for
# each issue's author.
#
# A fully correct flag would need to know, for each issue, whether its
# author had already contributed code *before* opening it. That is not viable
# at the scale this is meant to run at. Instead this uses GitHub's
# `/contributors` endpoint: a single cheap paginated call listing everyone who
# has ever landed a commit on the default branch, with no timing information at
# all. The tradeoff: someone who only contributed *after* their first issue is
# misclassified as a contributor. Accepted here as the cheaper heuristic, since
# it costs one extra endpoint per repo rather than a full commit-history walk.
#
# `is_contributor` is restricted to "primary" contributors: the smallest
# prefix of the contributions-sorted list whose cumulative commit count
# covers `primary_coverage` (default 80%) of all commits ever landed - a
# Pareto-style core-team cutoff, still just the one cheap endpoint per repo.

#' Primary contributor logins for a GitHub repo: the smallest prefix of the
#' contributions-sorted contributor list whose cumulative commit count
#' covers `coverage` of all commits ever landed on the default branch (see
#' note above for why raw "ever committed once" is too noisy a signal).
#' Excludes GitHub's anonymous-contributor placeholder entries, which carry
#' no `login`, just a name/email pulled from the commit itself.
#' @noRd
github_repo_contributors <- function (owner, repo, coverage = 0.95) {
    items <- github_api_get_all (stringr::str_glue ("/repos/{owner}/{repo}/contributors"))
    items <- purrr::keep (items, \ (x) !is.null (x$login))
    if (length (items) == 0) {
        return (character ())
    }

    logins <- purrr::map_chr (items, "login")
    contributions <- purrr::map_dbl (items, "contributions")
    ord <- order (contributions, decreasing = TRUE)
    logins <- logins [ord]
    contributions <- contributions [ord]

    n_primary <- which (cumsum (contributions) / sum (contributions) >= coverage) [1]
    logins [seq_len (n_primary)]
}

#' Extract every issue (pull requests excluded) opened against a single GitHub
#' repo, with the opener's handle and a cheap `is_contributor` flag: whether
#' that handle is among the repo's primary contributors (see the note at the
#' top of this file for how "primary" is defined from contribution counts).
#'
#' @param repo_url A GitHub repo URL, e.g. `"https://github.com/owner/repo"`.
#' @param primary_coverage Cumulative share (0-1) of all commits that the
#' flagged "primary" contributors must account for, taken in descending
#' order of commit count. Default 0.8 (the top contributors covering 80%
#' of all commits).
#'
#' @return A tibble with one row per issue: `issue_number`, `author`,
#' `created_at`, and `is_contributor`.
#' @export
github_issue_authors <- function (repo_url = NULL, primary_coverage = 0.95) {

    issue_number <- NULL # rm no visible binding note

    repo <- parse_github_repo_url (repo_url)

    contributors <- github_repo_contributors (repo$owner, repo$repo, coverage = primary_coverage)

    issues <- github_api_get_all (
        stringr::str_glue ("/repos/{repo$owner}/{repo$repo}/issues"),
        query = list (state = "all")
    )
    issues <- purrr::keep (issues, \ (i) is.null (i$pull_request))

    if (length (issues) == 0) {
        return (tibble::tibble (
            repo_url = character (),
            issue_number = integer (),
            author = character (),
            created_at = character (),
            is_contributor = logical ()
        ))
    }

    purrr::map_dfr (issues, \ (i) {
        author <- i$user$login
        tibble::tibble (
            issue_number = i$number,
            author = author,
            created_at = i$created_at,
            is_contributor = author %in% contributors
        )
    }) |>
        dplyr::mutate (repo_url = repo_url, .before = issue_number)
}
