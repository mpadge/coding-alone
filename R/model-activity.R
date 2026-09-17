#' Fit a quasi-Poisson GLM to analyse differences in monthly issue rates across
#' popularity strata. The `month_num:popularity_stratum` interaction tests
#' whether the long tail trends differently from the popular head, rather than
#' just reporting one global trend line.
#'
#' @param rate_tbl As returned by `issue_rate_tbl()`.
#' @return A fitted `glm` object.
#'
#' @examples
#' \dontrun{
#' model <- fit_activity_model (rate_tbl)
#' summary (model)
#' }
#' @export
fit_activity_model <- function (rate_tbl) {

    n_repo_months <- NULL # rm no visible binding note

    rate_tbl <- dplyr::filter (rate_tbl, n_repo_months > 0)
    rate_tbl$month_num <-
        as.numeric (rate_tbl$month - min (rate_tbl$month)) / 365.25

    stats::glm (
        n_metric ~ month_num * popularity_stratum +
            offset (log (n_repo_months)),
        data = rate_tbl,
        family = stats::quasipoisson ()
    )
}
