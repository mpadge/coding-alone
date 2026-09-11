# Functions to extract every issue opened against a single GitHub repo,
# together with a cheap continuous "how core a contributor is this person"
# score for each issue's author.
#
# A fully correct score would need to know, for each issue, whether its
# author had already contributed code *before* opening it. That is not viable
# at the scale this is meant to run at. Instead this uses GitHub's
# `/contributors` endpoint: a single cheap paginated call listing everyone who
# has ever landed a commit on the default branch, with no timing information at
# all. The tradeoff: someone who only contributed *after* their first issue is
# credited with a nonzero score anyway. Accepted here as the cheaper
# heuristic, since it costs one extra endpoint per repo rather than a full
# commit-history walk.
#
# `contribution` is that person's share of all commits ever landed, expressed
# as a fraction (0-1) of the repo's total commit count - a continuous
# stand-in for the coarser "primary contributor" cutoff this used to compute.
#
# Issue data itself is fetched via GraphQL rather than the REST `/issues`
# endpoint: REST returns the entire issue object (labels, body, reactions,
# etc.) just to get the author login and creation timestamp, and mixes PRs in
# with issues that then have to be paged through and filtered back out.
# GraphQL's `issues` connection is issues-only and lets the query ask for
# exactly the two fields needed, cutting both payload size and page count.

#' Every contributor to a GitHub repo's default branch, with each one's
#' share of all commits ever landed, expressed as a fraction (0-1) of the
#' total. Excludes GitHub's anonymous-contributor placeholder entries, which
#' carry no `login`, just a name/email pulled from the commit itself.
#' @return A tibble with columns `login` and `contribution` (fraction of
#' all commits ever landed), or a zero-row tibble if the repo has no
#' (non-anonymous) contributors.
#' @noRd
github_repo_contributors <- function (owner, repo) {
    items <- github_api_get_all (
        stringr::str_glue ("/repos/{owner}/{repo}/contributors")
    )
    items <- purrr::keep (items, \ (x) !is.null (x$login))
    if (length (items) == 0) {
        return (tibble::tibble (login = character (), contribution = double ()))
    }

    logins <- purrr::map_chr (items, "login")
    contributions <- purrr::map_dbl (items, "contributions")

    tibble::tibble (
        login = logins, contribution = contributions / sum (contributions)
    )
}

github_issues_page_size <- function () {

    if (identical (Sys.getenv ("PEERREVIEW_TESTS"), "true")) {
        5L
    } else {
        100L
    }
}

#' GraphQL query for one page of a repo's issues (creator login, creation
#' timestamp, and comment count), plus the repo's own creation timestamp.
#' Values are interpolated directly into the query string (as elsewhere in
#' this package, e.g. `build_stars_query()` in extract-data-joss.R) rather
#' than passed as separate GraphQL variables, since `gh::gh_gql()` has no
#' support for the latter.
#' @noRd
build_issues_query <- function (owner, repo, cursor = NULL) {

    after <- if (is.null (cursor)) {
        ""
    } else {
        stringr::str_glue (', after: "{cursor}"')
    }

    first <- github_issues_page_size ()

    stringr::str_glue (
        'query {{
            repository(owner: "{owner}", name: "{repo}") {{
                createdAt
                issues(first: {first}{after}, orderBy: {{field: CREATED_AT, direction: ASC}}) {{
                    pageInfo {{ hasNextPage endCursor }}
                    nodes {{ number createdAt author {{ login }} comments {{ totalCount }} }}
                }}
            }}
        }}'
    )
}

#' Every issue (pull requests excluded by construction - GraphQL keeps
#' issues and pull requests in separate connections, unlike the REST
#' `/issues` endpoint which mixes them) opened against a single GitHub repo,
#' with the opener's login and creation timestamp, plus the repo's own
#' GitHub creation timestamp (fetched in the same query, cheaper than a
#' separate REST `/repos/{owner}/{repo}` call just for that one field).
#' Pages via GraphQL cursors until `hasNextPage` is `FALSE`.
#' @return A list with `repo_created_at` (an ISO-8601 timestamp string) and
#' `issues` (a tibble with `issue_number`, `author`, `created_at`,
#' `n_comments`).
#' @noRd
github_repo_issues_graphql <- function (owner, repo) {

    cursor <- NULL
    repo_created_at <- NULL
    pages <- list ()
    single_page_only <- identical (Sys.getenv ("PEERREVIEW_TESTS"), "true")

    repeat {

        body <- gh::gh_gql (build_issues_query (owner, repo, cursor))
        node <- body$data$repository
        if (is.null (repo_created_at)) {
            repo_created_at <- node$createdAt
        }

        issue_nodes <- node$issues$nodes
        if (length (issue_nodes) > 0) {
            pages [[length (pages) + 1]] <- tibble::tibble (
                issue_number = purrr::map_int (issue_nodes, "number"),
                author = purrr::map_chr (
                    issue_nodes,
                    purrr::pluck, "author", "login",
                    .default = NA_character_
                ),
                created_at = purrr::map_chr (issue_nodes, "createdAt"),
                n_comments = purrr::map_int (
                    issue_nodes, \ (n) n$comments$totalCount
                )
            )
        }

        if (single_page_only || !isTRUE (node$issues$pageInfo$hasNextPage)) {
            break
        }
        cursor <- node$issues$pageInfo$endCursor
    }

    issues <- if (length (pages) == 0) {
        tibble::tibble (
            issue_number = integer (), author = character (),
            created_at = character (), n_comments = integer ()
        )
    } else {
        dplyr::bind_rows (pages)
    }

    list (repo_created_at = repo_created_at, issues = issues)
}

#' Extract every issue (pull requests excluded) opened against a single
#' GitHub repo, with the opener's handle and a `contribution` score: that
#' handle's fractional share (0-1) of all commits ever landed on the repo's
#' default branch, or 0 if the author isn't a contributor at all (see the
#' note at the top of this file for why this is a coarser but far cheaper
#' substitute for "was this author already a contributor at the time they
#' opened the issue"). Also carries each issue's comment count, and the
#' repo's own GitHub creation timestamp, used elsewhere as the start of a
#' repo's exposure window.
#'
#' @param repo_url A GitHub repo URL, e.g. `"https://github.com/owner/repo"`.
#'
#' @return A tibble with one row per issue: `repo_url`, `issue_number`,
#' `author`, `created_at`, `n_comments`, `contribution`, and
#' `repo_created_at` (the repo's own GitHub creation timestamp, repeated on
#' every row).
#'
#' @examples
#' \dontrun{
#' issue_authors <- github_issue_authors ("https://github.com/ropensci/targets")
#' }
#' @export
github_issue_authors <- function (repo_url = NULL) {

    issue_number <- contribution <- NULL # rm no visible binding notes

    repo <- parse_github_repo_url (repo_url)

    contributors <- github_repo_contributors (repo$owner, repo$repo)
    result <- github_repo_issues_graphql (repo$owner, repo$repo)

    if (nrow (result$issues) == 0) {
        return (tibble::tibble (
            repo_url = character (),
            issue_number = integer (),
            author = character (),
            created_at = character (),
            n_comments = integer (),
            contribution = double (),
            repo_created_at = character ()
        ))
    }

    result$issues |>
        dplyr::left_join (contributors, by = c (author = "login")) |>
        dplyr::mutate (contribution = dplyr::coalesce (contribution, 0)) |>
        dplyr::mutate (repo_url = repo_url, .before = issue_number) |>
        dplyr::mutate (repo_created_at = result$repo_created_at)
}
