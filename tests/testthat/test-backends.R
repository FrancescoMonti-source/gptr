test_that(".resolve_base_url prefers explicit base_url arg", {
    expect_equal(
        .resolve_base_url("lmstudio", "http://x:1/v1/chat/completions"),
        "http://x:1/v1/chat/completions"
    )
})

test_that(".resolve_base_url respects pinned local_base_url for local-like providers", {
    withr::with_options(list(gpt.local_base_url = "http://127.0.0.1:1234/v1"), {
        expect_match(.resolve_base_url("auto", NULL), "1234")
        expect_match(.resolve_base_url("lmstudio", NULL), "1234")
        expect_match(.resolve_base_url("ollama", NULL), "1234")
    })
})

test_that(".resolve_base_url falls back to provider defaults when not pinned", {
    withr::with_options(list(gpt.local_base_url = NULL), {
        expect_match(.resolve_base_url("lmstudio", NULL), "127.0.0.1:1234")
        expect_match(.resolve_base_url("ollama",   NULL), "127.0.0.1:11434")
        expect_match(.resolve_base_url("localai",  NULL), "127.0.0.1:8080")
        expect_match(.resolve_base_url("openai",   NULL), "openai.com")
    })
})


test_that("Sending pings to different combinations of providers and models", {
    # OpenAI explicitly – should hit OpenAI regardless of local servers
    expect_true(gpt(prompt = "hi", provider = "openai", model = "gpt-4o-mini") %>% nzchar())
    expect_true(gpt(prompt = "hi", provider = "openai") %>% nzchar())

    # Auto with an OpenAI-looking model – will prefer local if running
    expect_true(gpt(prompt = "hi", model = "gpt-4o-mini") %>% nzchar())

    # Force local – should succeed if any local backend is available
    expect_true(gpt(prompt = "hi", provider = "local") %>% nzchar())
    expect_true(gpt(prompt = "hi", provider = "lmstudio") %>% nzchar())
    expect_true(gpt(prompt = "hi", provider = "local", backend = "lmstudio", base_url = NULL) %>% nzchar())

    # Auto with a local model name – should pick the appropriate local backend
    expect_true(gpt(prompt = "hi", provider = "auto", model = "mistralai/mistral-7b-instruct-v0.3") %>% nzchar())

    # Auto with no model specified – will pick whichever is available; if both are
    # available, local will generally win. This line is intentionally last
    # because it’s the broadest case.
    expect_true(gpt(prompt = "hi", provider = "auto") %>% nzchar())
})


test_that("Explicit OpenAI base_url must include /chat/completions", {
    # Should succeed – full endpoint specified
    expect_true(gpt("hi", provider = "openai",
                    base_url = "https://api.openai.com/v1/chat/completions") %>% nzchar())
    # Should error or return empty because /v1 alone is not a chat endpoint
    expect_error(gpt("hi", provider = "openai",
                     base_url = "https://api.openai.com/v1"))
})
