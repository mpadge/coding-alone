test_that ("extract_language finds language label(s), excluding workflow tags", {
    labels <- list (
        list (name = "accepted"),
        list (name = "R"),
        list (name = "review")
    )
    expect_identical (extract_language (labels), "R")
})

test_that ("extract_language joins multiple language labels", {
    labels <- list (
        list (name = "R"),
        list (name = "Python"),
        list (name = "accepted")
    )
    expect_identical (extract_language (labels), "R, Python")
})

test_that ("extract_language returns NA when no language label present", {
    labels <- list (list (name = "accepted"), list (name = "bug"))
    expect_true (is.na (extract_language (labels)))
})

test_that ("extract_repo_url parses the HTML-comment-delimited form", {
    body <- paste0 (
        "**Repository:** <!--target-repository-->",
        "https://github.com/owner/repo<!--end-target-repository-->\n",
        "**Version:** v1.0.0"
    )
    expect_identical (extract_repo_url (body), "https://github.com/owner/repo")
})

test_that ("extract_repo_url parses the anchor-tag form", {

    body <- paste0 (
        "**Repository:** <a href=\"https://github.com/owner/repo\" ",
        "target=\"_blank\">",
        "https://github.com/owner/repo</a>"
    )
    expect_identical (extract_repo_url (body), "https://github.com/owner/repo")
})

test_that ("extract_repo_url falls back to a bare URL", {
    body <- "**Repository:** https://github.com/owner/repo\n**Version:** v1.0.0"
    expect_identical (extract_repo_url (body), "https://github.com/owner/repo")
})

test_that ("extract_repo_url returns NA for NULL/NA/no-match input", {
    expect_true (is.na (extract_repo_url (NULL)))
    expect_true (is.na (extract_repo_url (NA_character_)))
    expect_true (is.na (extract_repo_url ("no repository line here")))
})

test_that ("join_registry_dls left-joins by repo_url, PyPI winning ties", {
    tbl <- tibble::tibble (repo_url = three_gh_urls [1:2])
    pypi_tbl <- tibble::tibble (
        repo_url = "https://github.com/o/a",
        downloads = 100
    )
    npm_tbl <- tibble::tibble (
        repo_url = three_gh_urls [1:2],
        downloads = c (999, 200)
    )

    out <- join_registry_downloads (tbl, pypi_tbl, npm_tbl)
    expect_identical (out$downloads [out$repo_url == "https://github.com/o/a"], 100)
    expect_identical (out$downloads [out$repo_url == "https://github.com/o/b"], 200)
})

test_that ("join_registry_downloads handles a single source", {
    tbl <- tibble::tibble (repo_url = three_gh_urls [1:2])
    pypi_tbl <- tibble::tibble (
        repo_url = "https://github.com/o/a",
        downloads = 100
    )

    out <- join_registry_downloads (tbl, pypi_tbl, NULL)
    expect_identical (out$downloads [out$repo_url == "https://github.com/o/a"], 100)
    expect_true (is.na (
        out$downloads [out$repo_url == "https://github.com/o/b"]
    ))
})

test_that ("join_registry_dls gives all-NA when sources are NULL", {
    tbl <- tibble::tibble (repo_url = three_gh_urls)
    out <- join_registry_downloads (tbl, NULL, NULL)
    expect_true (all (is.na (out$downloads)))
    expect_identical (nrow (out), nrow (tbl))
})

test_that ("build_stars_query builds one aliased field per repo", {
    q <- build_stars_query (c ("o1", "o2"), c ("r1", "r2"), c (1L, 2L))
    expect_type (q, "character")
    expect_match (q, "r1: repository\\(owner: \"o1\", name: \"r1\"\\)")
    expect_match (q, "r2: repository\\(owner: \"o2\", name: \"r2\"\\)")
    expect_match (q, "stargazerCount")
})

test_that ("github_stars_many returns all-NA when not resolvable", {
    out <- github_stars_many (c ("not-a-url", NA_character_))
    expect_identical (out, c (NA_integer_, NA_integer_))
})

test_that ("build_joss_issues_query filters by label/state and pages via cursor", {
    q <- build_joss_issues_query ("openjournals", "joss-reviews")
    expect_type (q, "character")
    expect_match (q, 'repository\\(owner: "openjournals", name: "joss-reviews"\\)')
    expect_match (q, 'labels: \\["accepted"\\]')
    expect_match (q, "states: \\[OPEN, CLOSED\\]")
    expect_no_match (q, "after:")

    q_cursor <- build_joss_issues_query ("openjournals", "joss-reviews", cursor = "CURSOR1")
    expect_match (q_cursor, 'after: "CURSOR1"')
})

# github_stars_many()'s real-repo/GraphQL-fixture case, and build_joss_table()
# (which uses github_stars_many() and fetch_joss_issues() internally) are in
# test-live-graphql.R, gated behind test_all.
