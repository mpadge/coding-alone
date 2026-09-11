test_that ("build_working_sample combines a deterministic head and tail", {
    downloads_tbl <- tibble::tibble (
        name = paste0 ("pkg", 1:100),
        downloads = 100:1
    )
    set.seed (1)
    out <- build_working_sample (
        downloads_tbl,
        top_n_head = 10L, tail_size = 20L
    )

    expect_identical (nrow (out), 30L)
    expect_true (all (paste0 ("pkg", 1:10) %in% out$name)) # deterministic head
    expect_length (unique (out$name), 30L) # no duplication between head/tail
})

test_that ("build_working_sample caps tail_size at the remaining pool size", {
    downloads_tbl <- tibble::tibble (
        name = paste0 ("pkg", 1:15), downloads = 15:1
    )
    out <- build_working_sample (
        downloads_tbl,
        top_n_head = 10L, tail_size = 100L
    )

    expect_identical (nrow (out), 15L) # 10 head + only 5 remain in the pool
})

test_that ("build_working_sample prints a cli message only when labelled", {
    downloads_tbl <- tibble::tibble (
        name = paste0 ("pkg", 1:5), downloads = 5:1
    )
    expect_silent (
        build_working_sample (
            downloads_tbl,
            top_n_head = 2L, tail_size = 1L
        )
    )
    expect_message (
        build_working_sample (
            downloads_tbl,
            top_n_head = 2L, tail_size = 1L, label = "PyPI"
        ),
        "PyPI"
    )
})

test_that ("resolve_repo_urls filters to rows with a resolved repo_url", {
    working_sample <- tibble::tibble (
        name = c ("a", "b", "c"), downloads = c (3, 2, 1)
    )
    fake_resolver <- function (names_vec) {
        ifelse (
            names_vec == "b",
            NA_character_,
            paste0 ("https://github.com/x/", names_vec)
        )
    }

    out <- resolve_repo_urls (working_sample, fake_resolver)

    expect_named (out, c ("name", "downloads", "repo_url"))
    expect_identical (out$name, c ("a", "c"))
    expect_identical (
        out$repo_url, c ("https://github.com/x/a", "https://github.com/x/c")
    )
})
