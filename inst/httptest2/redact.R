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
        "",
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

    resp <- httptest2::gsub_response (
        resp,
        "ropensci.r-universe.dev",
        "ropensci",
        fixed = TRUE
    )

    # PyPi JSON data is mostly huge amounts of detail on every release. This
    # 'gsub's away all release information. That's always followed by 'urls'.
    resp <- httptest2::gsub_response (
        resp,
        '(?s)"releases":.*?"urls":',
        '"releases": {}, "urls":',
        perl = TRUE,
        fixed = FALSE
    )

    # r-universe package records carry a build-log "_jobs" array (always
    # immediately followed by "_host") and a per-platform "_binaries" array
    # (always the record's last field, so bounded by the "}" that closes
    # the whole package object rather than another key) - together often
    # a third or more of one package's JSON, and unused by build_runiv_table().
    resp <- httptest2::gsub_response (
        resp,
        '(?s)"_jobs":.*?"_host":',
        '"_jobs": [], "_host":',
        perl = TRUE,
        fixed = FALSE
    )
    resp <- httptest2::gsub_response (
        resp,
        '(?s)"_binaries":\\s*\\[.*?\\]\\s*\\}',
        '"_binaries": []}',
        perl = TRUE,
        fixed = FALSE
    )

    return (resp)
}
