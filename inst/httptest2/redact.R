function (resp) {

    resp <- httptest2::gsub_response (
        resp,
        "Bearer [^\"]+",
        "Bearer <redacted>",
        fixed = FALSE
    )

    resp <- httptest2::gsub_response (
        resp,
        "https://api.github.com/repos/",
        "ghrepos/",
        fixed = TRUE
    )

    resp <- httptest2::gsub_response (
        resp,
        "https://api.github.com/graphql",
        "graphql",
        fixed = TRUE
    )

    resp <- httptest2::gsub_response (
        resp,
        "https://registry.npmjs.org/",
        "npm/",
        fixed = TRUE
    )

    resp <- httptest2::gsub_response (
        resp,
        "https://pypi.org/pypi/",
        "pypi/",
        fixed = TRUE
    )

    resp <- httptest2::gsub_response (
        resp,
        "https://sql-clickhouse.clickhouse.com",
        "clickhouse",
        fixed = TRUE
    )

    resp <- httptest2::gsub_response (
        resp,
        "\\.r-universe\\.dev/api/packages",
        ".r-universe.dev/packages",
        fixed = FALSE
    )

    resp <- httptest2::gsub_response (
        resp,
        "https://cranlogs.r-pkg.org/downloads/total/last-month/",
        "cranlogs/",
        fixed = TRUE
    )

    return (resp)
}
