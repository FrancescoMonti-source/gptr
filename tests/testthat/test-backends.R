test_that(".normalize_provider maps local->auto and keeps others", {
    expect_equal(.normalize_provider("local"), "auto")
    expect_equal(.normalize_provider("auto"), "auto")
    expect_equal(.normalize_provider("lmstudio"), "lmstudio")
    expect_equal(.normalize_provider("ollama"), "ollama")
})

test_that(".resolve_base_url prefers explicit base_url arg", {
    expect_equal(
        .resolve_base_url("lmstudio", "http://x:1/v1/chat/completions"),
        "http://x:1/v1/chat/completions"
    )
})

test_that(".resolve_base_url respects pinned local_base_url for local-like providers", {
    withr::with_options(list(gpt.local_base_url = "http://127.0.0.1:1234/v1/chat/completions"), {
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
