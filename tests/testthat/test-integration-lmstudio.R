test_that("LM Studio fast path works (pinned)", {
    testthat::skip_if(Sys.getenv("GPTR_INTEGRATION") != "1")
    options(
        gpt.provider       = "auto",
        gpt.local_base_url = "http://127.0.0.1:1234/v1/chat/completions",
        gpt.local_model    = "mistralai/mistral-7b-instruct-v0.3",
        gpt.verbose_preflight = TRUE
    )
    out <- gpt("ping", provider = "lmstudio", print_raw = FALSE, temperature = 0)
    expect_type(out, "character")
    u <- attr(out, "usage", exact = TRUE)
    expect_true(is.list(u) || is.null(u)) # some servers omit usage
})

test_that("Default URL with helpful error if LM Studio down", {
    testthat::skip_if(Sys.getenv("GPTR_INTEGRATION") != "1") # run when you want to check message
    options(gpt.local_base_url = NULL)
    if (!curl::has_internet()) skip("no internet utils")
    # If LM Studio is not running, this should error with guidance.
    # If it is running, this should pass and return text.
    res <- try(gpt("ping", provider = "lmstudio", model = "mistralai/mistral-7b-instruct-v0.3", temperature = 0), silent = TRUE)
    if (inherits(res, "try-error")) {
        expect_match(conditionMessage(attr(res, "condition")),
                     "Pass `base_url=` or set options\\(gpt\\.lmstudio_base_url=\\)\\.")
    } else {
        expect_true(is.character(res))
    }
})
