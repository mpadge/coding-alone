test_that ("build_working_sample combines a deterministic head and random tail", {
    downloads_tbl <- tibble::tibble (
        name = paste0 ("pkg", 1:100),
        downloads = 100:1
    )
    set.seed (1)
    out <- longtail::build_working_sample (downloads_tbl, top_n_head = 10L, tail_size = 20L)

    expect_equal (nrow (out), 30L)
    expect_true (all (paste0 ("pkg", 1:10) %in% out$name)) # deterministic head
    expect_length (unique (out$name), 30L) # no duplication between head/tail
})

test_that ("build_working_sample caps tail_size at the size of the remaining pool", {
    downloads_tbl <- tibble::tibble (name = paste0 ("pkg", 1:15), downloads = 15:1)
    out <- longtail::build_working_sample (downloads_tbl, top_n_head = 10L, tail_size = 100L)

    expect_equal (nrow (out), 15L) # 10 head + only 5 remain in the pool
})

test_that ("build_working_sample prints a cli message only when label is given", {
    downloads_tbl <- tibble::tibble (name = paste0 ("pkg", 1:5), downloads = 5:1)
    expect_silent (
        longtail::build_working_sample (downloads_tbl, top_n_head = 2L, tail_size = 1L)
    )
    expect_message (
        longtail::build_working_sample (downloads_tbl, top_n_head = 2L, tail_size = 1L, label = "PyPI"),
        "PyPI"
    )
})

test_that ("resolve_repo_urls filters to rows with a resolved repo_url", {
    working_sample <- tibble::tibble (name = c ("a", "b", "c"), downloads = c (3, 2, 1))
    fake_resolver <- function (names_vec) {
        ifelse (names_vec == "b", NA_character_, paste0 ("https://github.com/x/", names_vec))
    }

    out <- longtail::resolve_repo_urls (working_sample, fake_resolver)

    expect_equal (names (out), c ("name", "downloads", "repo_url"))
    expect_equal (out$name, c ("a", "c"))
    expect_equal (out$repo_url, c ("https://github.com/x/a", "https://github.com/x/c"))
})
