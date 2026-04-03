test_that("auto with strict_model validates only the chosen local route", {
  testthat::local_mocked_bindings(
    .fetch_models_cached = function(provider = NULL, base_url = NULL, openai_api_key = "", ...) {
      expect_identical(provider, "lmstudio")
      expect_identical(base_url, "http://127.0.0.1:1234")
      data.frame(model_id = "mistral", stringsAsFactors = FALSE)
    },
    .package = "gptr"
  )

  expect_error(
    gpt("hi", model = "missing", provider = "auto", print_raw = FALSE),
    "Model 'missing' not found on http://127.0.0.1:1234.",
    fixed = TRUE
  )
})

test_that("strict_model = FALSE sends the requested OpenAI model unchanged", {
  used <- NULL
  testthat::local_mocked_bindings(
    openai_send_request = function(payload, base_url, api_key, timeout = 30, ssl_cert = NULL) {
      used <<- payload$model
      fake_resp(model = payload$model %||% "gpt-4o-mini")
    },
    .package = "gptr"
  )

  gpt(
    "hi",
    model = "unknown",
    provider = "openai",
    openai_api_key = "sk-test",
    strict_model = FALSE,
    allow_remote = TRUE,
    print_raw = FALSE
  )

  expect_identical(used, "unknown")
})

test_that("strict_model errors when OpenAI model is unavailable", {
  testthat::local_mocked_bindings(
    .fetch_models_cached = function(provider = NULL, base_url = NULL, openai_api_key = "", ...) {
      data.frame(model_id = "gpt-4o-mini", stringsAsFactors = FALSE)
    },
    .package = "gptr"
  )

  expect_error(
    gpt(
      "hi",
      model = "unknown",
      provider = "openai",
      openai_api_key = "sk-test",
      strict_model = TRUE,
      allow_remote = TRUE,
      print_raw = FALSE
    ),
    "Model 'unknown' not found for OpenAI."
  )
})

test_that("strict_model errors when model not installed (local)", {
  testthat::local_mocked_bindings(
    .fetch_models_cached = function(provider = NULL, base_url = NULL, openai_api_key = "", ...) {
      data.frame(model_id = "mistral", stringsAsFactors = FALSE)
    },
    .request_local = function(payload, base_url, timeout = 30, ssl_cert = NULL) {
      fake_resp(model = payload$model %||% "mistral")
    },
    .package = "gptr"
  )

  expect_error(
    gpt("hi", provider = "local", model = "llama3:latest", strict_model = TRUE, print_raw = FALSE),
    "Model 'llama3:latest' not found"
  )
})

test_that("strict_model still errors if the server swaps models after listing failure", {
  called_models <- FALSE
  called_chat <- FALSE
  testthat::local_mocked_bindings(
    .fetch_models_cached = function(provider = NULL, base_url = NULL, openai_api_key = "", refresh = FALSE, ssl_cert = NULL, ...) {
      called_models <<- TRUE
      data.frame(model_id = character(), stringsAsFactors = FALSE)
    },
    .request_local = function(payload, base_url, timeout_sec = 180, max_tries = 2, user_agent = "ua", debug_http = FALSE, ssl_cert = NULL, ...) {
      called_chat <<- TRUE
      fake_resp(model = "fallback")
    },
    .package = "gptr"
  )

  expect_error(
    gpt(
      "hi",
      provider = "local",
      model = "llama3:latest",
      openai_api_key = "",
      strict_model = TRUE,
      print_raw = FALSE
    ),
    "Server used model 'fallback' instead of requested 'llama3:latest'.",
    fixed = TRUE
  )
  expect_true(called_models)
  expect_true(called_chat)
})

test_that("OpenAI strict_model matching is case-insensitive", {
  called <- NULL
  testthat::local_mocked_bindings(
    .fetch_models_cached = function(provider = NULL, base_url = NULL, openai_api_key = "", ...) {
      data.frame(model_id = "GPT-4O-MINI", stringsAsFactors = FALSE)
    },
    openai_send_request = function(payload, base_url, api_key, timeout = 30, ssl_cert = NULL) {
      called <<- "openai"
      fake_resp(model = payload$model %||% "GPT-4O-MINI")
    },
    .package = "gptr"
  )

  gpt("hi", model = "gpt-4o-mini", provider = "openai", openai_api_key = "sk-test", allow_remote = TRUE, print_raw = FALSE)

  expect_identical(called, "openai")
})
