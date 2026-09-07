#' Perform many GET-JSON requests concurrently. Returns a list the same
#' length/order as `urls`; NULL for any request that errored or came back
#' 4xx/5xx. Individual failures don't abort the batch (on_error = "continue")
#' — most failures here are legitimate 404s (deleted/renamed packages), not
#' the registries actually rate-limiting concurrent traffic.
#'
#' @noRd
perform_json_parallel <- function(urls, max_active = MAX_ACTIVE_META) {
    reqs <- purrr::map(urls, \(u) {
        httr2::request(u) |>
            httr2::req_retry(max_tries = 3) |>
            httr2::req_error(is_error = \(resp) FALSE)
    })
    resps <- httr2::req_perform_parallel(reqs, on_error = "continue", max_active = max_active)
    purrr::map(resps, \(resp) {
        if (inherits(resp, "error") || httr2::resp_status(resp) >= 400) {
            return(NULL)
        }
        tryCatch(httr2::resp_body_json(resp, simplifyVector = FALSE), error = \(e) NULL)
    })
}
