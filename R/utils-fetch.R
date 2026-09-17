# Shared helper for the fetch-*.R checkpointed/batched fetchers.

#' Split a vector into `n_batches` groups so that fetching group 1, then
#' group 2, etc. gives even coverage of the whole vector at any stopping
#' point, rather than exhausting one end of it first. Assumes `x` arrives
#' already prioritised (e.g. `repo_tbl`'s PyPI/npm rows, built by
#' `build_working_sample()` as a deterministic head of the most-downloaded
#' packages followed by a random tail).
#'
#' @param x Vector already ordered by priority (highest first).
#' @param n_batches Number of interleaved groups to split `x` into - in
#' practice `fetch_issue_authors()`'s number of batches, so each
#' checkpointed batch is itself one such group.
#' @return A list of `n_batches` groups (as from `split()`), each an evenly
#' spread subsample of `x`, in group order.
#' @noRd
interlace_for_even_coverage <- function (x, n_batches) {

    if (n_batches <= 1 || length (x) == 0) {
        return (list (x))
    }

    split (x, rep (seq_len (n_batches), length.out = length (x)))
}
