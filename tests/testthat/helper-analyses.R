three_gh_urls <- c (
    "https://github.com/o/a",
    "https://github.com/o/b",
    "https://github.com/o/c"
)

write_test_analyses_data <- function (out_dir) {

    readr::write_csv (
        tibble::tibble (
            package = c ("p1", "p2", "p3"),
            repo_url = three_gh_urls,
            downloads = c (100, 200, 400)
        ),
        file.path (out_dir, "pypi.csv")
    )
    readr::write_csv (
        tibble::tibble (
            name = "n1",
            repo_url = "https://github.com/o/n1",
            downloads = 300
        ),
        file.path (out_dir, "npm.csv")
    )
    readr::write_csv (
        tibble::tibble (
            title = "[REVIEW]: Some Tool",
            repo_url = "https://github.com/o/joss1",
            stars = 5
        ),
        file.path (out_dir, "joss.csv")
    )
    # Not one of the recognized source filenames - must be ignored:
    readr::write_csv (
        tibble::tibble (repo_url = "https://github.com/o/ignored"),
        file.path (out_dir, "issue-authors.csv")
    )
}
