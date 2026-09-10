#' Perform many GET-JSON requests concurrently. Returns a list the same
#' length/order as `urls`; NULL for any request that errored or came back
#' 4xx/5xx. Individual failures don't abort the batch (on_error = "continue")
#' — most failures here are legitimate 404s (deleted/renamed packages), not
#' the registries actually rate-limiting concurrent traffic.
#'
#' @noRd
perform_json_parallel <- function (urls, max_active = 40L) {

    reqs <- purrr::map (urls, \ (u) {
        httr2::request (u) |>
            httr2::req_retry (max_tries = 3) |>
            httr2::req_error (is_error = \ (resp) FALSE)
    })

    # httptest2 can only trace/record plain req_perform() calls, not
    # req_perform_parallel() (the parallel queue bypasses req_perform()
    # entirely, submitting straight to a curl pool) - so a missing fixture
    # can never be dynamically (re-)recorded through the parallel path; it
    # would just silently fall through to a live, uncached call on every
    # run instead. LONGTAIL_TESTS == "true" (see fetch_issue_authors() in
    # R/analyses.R for the same convention) switches to sequential
    # req_perform() calls instead - slower, but otherwise identical
    # (same per-request error handling below) and traceable/recordable.
    if (identical (Sys.getenv ("LONGTAIL_TESTS"), "true")) {

        resps <- lapply (reqs, \ (req) {
            tryCatch (httr2::req_perform (req), error = \ (e) e)
        })

    } else {

        resps <- httr2::req_perform_parallel (
            reqs,
            on_error = "continue", max_active = max_active
        )
    }

    purrr::map (resps, \ (resp) {

        if (inherits (resp, "error") || httr2::resp_status (resp) >= 400) {
            return (NULL)
        }

        tryCatch (
            httr2::resp_body_json (resp, simplifyVector = FALSE),
            error = \ (e) NULL
        )
    })
}

#' Repo URLs for many packages at once (concurrent requests), generic across
#' registries. `url_fn` builds the per-package metadata URL from `names_vec`;
#' `extract_candidates` pulls the candidate repo/homepage URL string(s) out
#' of a package's parsed JSON body. Shared by `pypi_repo_urls_many()` and
#' `npm_repo_urls_many()`, which differ only in those two arguments.
#'
#' @noRd
registry_repo_urls_many <- function (names_vec,
                                     url_fn,
                                     extract_candidates,
                                     max_active = 40L) {

    urls <- url_fn (names_vec)
    bodies <- perform_json_parallel (urls, max_active = max_active)

    purrr::map_chr (bodies, \ (body) {
        if (is.null (body)) {
            return (NA_character_)
        }
        find_github_url (extract_candidates (body))
    })
}
