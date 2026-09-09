test_that ("extract_language finds language label(s), excluding workflow tags", {
    labels <- list (
        list (name = "accepted"),
        list (name = "R"),
        list (name = "review")
    )
    expect_equal (longtail:::extract_language (labels), "R")
})

test_that ("extract_language joins multiple language labels", {
    labels <- list (list (name = "R"), list (name = "Python"), list (name = "accepted"))
    expect_equal (longtail:::extract_language (labels), "R, Python")
})

test_that ("extract_language returns NA when no language label present", {
    labels <- list (list (name = "accepted"), list (name = "bug"))
    expect_true (is.na (longtail:::extract_language (labels)))
})

test_that ("extract_repo_url parses the HTML-comment-delimited form", {
    body <- paste0 (
        "**Repository:** <!--target-repository-->",
        "https://github.com/owner/repo<!--end-target-repository-->\n",
        "**Version:** v1.0.0"
    )
    expect_equal (longtail:::extract_repo_url (body), "https://github.com/owner/repo")
})

test_that ("extract_repo_url parses the anchor-tag form", {
    body <- paste0 (
        "**Repository:** <a href=\"https://github.com/owner/repo\" target=\"_blank\">",
        "https://github.com/owner/repo</a>"
    )
    expect_equal (longtail:::extract_repo_url (body), "https://github.com/owner/repo")
})

test_that ("extract_repo_url falls back to a bare URL", {
    body <- "**Repository:** https://github.com/owner/repo\n**Version:** v1.0.0"
    expect_equal (longtail:::extract_repo_url (body), "https://github.com/owner/repo")
})

test_that ("extract_repo_url returns NA for NULL/NA/no-match input", {
    expect_true (is.na (longtail:::extract_repo_url (NULL)))
    expect_true (is.na (longtail:::extract_repo_url (NA_character_)))
    expect_true (is.na (longtail:::extract_repo_url ("no repository line here")))
})

test_that ("join_registry_downloads left-joins by repo_url, PyPI winning ties", {
    tbl <- tibble::tibble (repo_url = c ("https://github.com/a/a", "https://github.com/b/b"))
    pypi_tbl <- tibble::tibble (repo_url = "https://github.com/a/a", downloads = 100)
    npm_tbl <- tibble::tibble (
        repo_url = c ("https://github.com/a/a", "https://github.com/b/b"),
        downloads = c (999, 200)
    )

    out <- longtail:::join_registry_downloads (tbl, pypi_tbl, npm_tbl)
    expect_equal (out$downloads [out$repo_url == "https://github.com/a/a"], 100)
    expect_equal (out$downloads [out$repo_url == "https://github.com/b/b"], 200)
})

test_that ("join_registry_downloads handles a single source", {
    tbl <- tibble::tibble (repo_url = c ("https://github.com/a/a", "https://github.com/b/b"))
    pypi_tbl <- tibble::tibble (repo_url = "https://github.com/a/a", downloads = 100)

    out <- longtail:::join_registry_downloads (tbl, pypi_tbl, NULL)
    expect_equal (out$downloads [out$repo_url == "https://github.com/a/a"], 100)
    expect_true (is.na (out$downloads [out$repo_url == "https://github.com/b/b"]))
})

test_that ("join_registry_downloads gives all-NA downloads when both sources are NULL", {
    tbl <- tibble::tibble (repo_url = c ("https://github.com/a/a", "https://github.com/b/b"))
    out <- longtail:::join_registry_downloads (tbl, NULL, NULL)
    expect_true (all (is.na (out$downloads)))
    expect_equal (nrow (out), nrow (tbl))
})

test_that ("build_stars_query builds one aliased field per repo", {
    q <- longtail:::build_stars_query (c ("o1", "o2"), c ("r1", "r2"), c (1L, 2L))
    expect_type (q, "character")
    expect_match (q, "r1: repository\\(owner: \"o1\", name: \"r1\"\\)")
    expect_match (q, "r2: repository\\(owner: \"o2\", name: \"r2\"\\)")
    expect_match (q, "stargazerCount")
})

# ---- github_stars_many (GraphQL, hand-crafted httptest2 fixture) -----------
#
# Unauthenticated GraphQL requests get a rate limit of 0 (verified against
# the live API), so this fixture is hand-crafted rather than recorded - see
# the equivalent note in test-github-issues.R for how the request hash in
# the fixture filename was computed without ever performing the request.

test_that ("github_stars_many resolves real repos and NAs out unresolvable ones", {
    out <- httptest2::with_mock_dir ("graphql_stars", {
        suppressMessages (longtail:::github_stars_many (c (
            "https://github.com/hypertidy/ncmeta", # real -> 42 stars in fixture
            "https://github.com/o/deleted-repo", # syntactically valid, GraphQL node is null
            "not-a-github-url" # fails parse_github_repo_url() before any request
        )))
    })
    expect_equal (out, c (42L, NA_integer_, NA_integer_))
})

test_that ("github_stars_many returns all-NA without any request when nothing is resolvable", {
    out <- longtail:::github_stars_many (c ("not-a-url", NA_character_))
    expect_equal (out, c (NA_integer_, NA_integer_))
})

# ---- build_joss_table (HTTP, hand-crafted httptest2 fixture) ---------------
#
# The real openjournals/joss-reviews "accepted" issue list runs to
# thousands of issues and, with github_api_get_all()'s fixed per_page = 100,
# would take many dozens of live requests to page through fully - both slow
# to record and far too large to check in as a fixture. This fixture is
# hand-crafted instead: a single (already-final, < 100 items) REST page of
# 5 synthetic issues covering all three extract_repo_url() body forms (the
# HTML-comment form, the anchor-tag form, and the bare-URL fallback), one
# issue with no parseable Repository line at all (repo_url -> NA), and one
# entry carrying a `pull_request` field to check that it's filtered out
# despite carrying the "accepted" label. The REST fixture is a `.R` file
# (not the usual plain `.json`) for the same rate-limit-header reason noted
# in test-utils-github.R. The stargazer-count GraphQL fixture is hand-
# crafted for the same reason as github_stars_many()'s, above.

test_that ("build_joss_table extracts repo/language/stars for accepted submissions, dropping PRs", {
    withr::local_envvar (c (GITHUB_TOKEN = NA, GITHUB_PAT = NA))
    out <- suppressMessages (httptest2::with_mock_dir ("joss_mock", {
        longtail::build_joss_table ()
    }))

    expect_equal (nrow (out), 4L) # the pull_request-tagged 5th entry is dropped
    expect_equal (out$issue_number, c (1000L, 1001L, 1002L, 1003L))
    expect_equal (
        out$repo_url,
        c (
            "https://github.com/testauthor/toolA", "https://github.com/testauthor/toolB",
            "https://github.com/testauthor/toolC", NA_character_
        )
    )
    expect_equal (out$language, c ("R", "Python", NA_character_, "C++"))
    expect_equal (out$stars, c (10L, 20L, 30L, NA_integer_))
    expect_true (all (is.na (out$downloads))) # no pypi_tbl/npm_tbl supplied
})
