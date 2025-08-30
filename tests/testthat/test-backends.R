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
  expect_true(gpt("hi",
    provider = "openai",
    base_url = "https://api.openai.com/v1/chat/completions"
  ) %>% nzchar())
  # Should error or return empty because /v1 alone is not a chat endpoint
  expect_error(gpt("hi",
    provider = "openai",
    base_url = "https://api.openai.com/v1"
  ))
})
