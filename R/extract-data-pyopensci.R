#' Extract repo data for pyOpenSci
#'
#' @export
build_pyos_table <- function () {

    requireNamespace ("yaml", quietly = TRUE)

    u <- paste0 (
        "https://raw.githubusercontent.com/pyOpenSci/pyosMeta/",
        "refs/heads/main/data/packages.yml"
    )
    destfile <- fs::path (fs::path_temp (), basename (u))
    f <- utils::download.file (u, destfile = destfile, quiet = TRUE)
    dat <- yaml::read_yaml (destfile)

    approved <- vapply (
        dat,
        function (i) any (grepl ("approved", i$labels)),
        logical (1L)
    )
    dat <- dat [which (approved)]

    tbl <- data.frame (
        package = vapply (dat, function (i) i$package_name, character (1L)),
        repo_url = vapply (dat, function (i) i$repository_link, character (1L))
    )
    tbl$stars <- github_stars_many (tbl$repo_url)

    tbl
}
