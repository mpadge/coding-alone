#' Download the `repo-data-out/` dataset from this package's GitHub
#' release, if it doesn't already exist locally
#'
#' Download is only triggered if `out_dir` does not already exist. If `out_dir`
#' does not exist, it is created and every asset attached to the given release
#' `tag` is downloaded into it via `piggyback::pb_download()`.
#'
#' @param out_dir Directory to create and download data into. Default
#' `"repo-data-out"`, matching every other function in this package that
#' reads from it.
#' @param repo GitHub `owner/repo` to download release assets from.
#' @param tag Release tag holding the data assets.
#' @return `out_dir`, invisibly.
#'
#' @examples
#' \dontrun{
#' download_repo_data ()
#' }
#' @export
download_repo_data <- function (out_dir = "repo-data-out",
                                repo = "mpadge/coding-alone",
                                tag = "v0.1") {

    if (dir.exists (out_dir)) {
        cli::cli_alert_info (
            "{out_dir} already exists - skipping download."
        )
        return (invisible (out_dir))
    }

    fs::dir_create (out_dir)

    cli::cli_alert_info (
        "Downloading data from {repo}@{tag} into {out_dir}..."
    )
    piggyback::pb_download (repo = repo, tag = tag, dest = out_dir)

    cli::cli_alert_success (
        "Downloaded {length (fs::dir_ls (out_dir))} file(s) into {out_dir}."
    )

    invisible (out_dir)
}
