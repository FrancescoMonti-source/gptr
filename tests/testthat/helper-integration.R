# Opt-in flag: options(gptr.run_integration = TRUE) to run these
run_gptr_integration <- function() {
    isTRUE(getOption("gptr.run_integration", FALSE))
}

# Choose backend via option; default "local". For OpenAI, ensure API key present.
gptr_test_provider <- function() getOption("gptr.test.provider", "local")

skip_if_no_backend <- function() {
    prov <- gptr_test_provider()
    if (identical(prov, "openai") && Sys.getenv("OPENAI_API_KEY") == "") {
        testthat::skip("OPENAI_API_KEY not set")
    }
    # Tiny probe: if gpt() fails we skip
    ok <- TRUE
    probe <- try(gptr::gpt(prompt = "ping", provider = prov, temperature = 0, seed = 1234), silent = TRUE)
    if (inherits(probe, "try-error") || !is.character(probe)) ok <- FALSE
    if (!ok) testthat::skip(paste("No working", prov, "backend"))
}
