test_that("gpt_column E2E on real data (schema)", {
    testthat::skip_on_cran()
    testthat::skip_if_not(run_gptr_integration(), "Set options(gptr.run_integration=TRUE) to run")
    #skip_if_no_backend()

    # Load fixtures
    df <- readRDS(testthat::test_path("fixtures", "tci_dummy"))
    prompt <- paste(readLines(testthat::test_path("fixtures", "prompt_tci.txt"),
                              warn = FALSE,
                              encoding = "UTF-8"), collapse = "\n")

    # Keep it fast
    df <- utils::head(df, getOption("gptr.it_rows", 5L))

    out <- gptr::gpt_column(
        data       = df[1:3,],
        col        = tci,  # adjust to your column name
        prompt     = prompt,
        keys       = list(
            impulsivite         = "integer",
            hypersexualite      = "integer",
            trouble_alimentaire = "integer",
            jeu_pathologique    = "integer"
        ),
        provider             = gptr_test_provider(),
        base_url = "http://127.0.0.1:1234/v1/chat/completions",
        temperature          = 0,
        auto_correct_keys    = TRUE,
        keep_unexpected_keys = FALSE,
        return_debug         = TRUE
    )
    expect_true(all(c("impulsivite","hypersexualite","trouble_alimentaire","jeu_pathologique") %in% names(out)))
    expect_type(out$impulsivite, "integer")
    expect_type(out$hypersexualite, "integer")
    expect_type(out$trouble_alimentaire, "integer")
    expect_type(out$jeu_pathologique, "integer")
    expect_false(any(out$.invalid_rows %in% TRUE))  # rows should parse cleanly

})
