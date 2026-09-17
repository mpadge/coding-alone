# Statistical modelling on top of the activity rate tables in
# table-activity.R.

#' Fit the quasi-Poisson GLM described in analysis-plan.md: does the
#' month-over-month trend in issue rate differ across popularity strata?
#' The `month_num:popularity_stratum` interaction is the term of interest -
#' it's what tests whether the long tail is trending differently from the
#' popular head, rather than just reporting one global trend line.
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
