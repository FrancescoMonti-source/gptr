test_that("backend-specific model options override local_model", {
  withr::local_options(list(gptr.local_model = "modelA", gptr.ollama_model = "modelB"))

  called <- NULL
  testthat::local_mocked_bindings(
    .fetch_models_cached = function(provider = NULL, base_url = NULL, openai_api_key = "", ...) {
      data.frame(model_id = c("modelA", "modelB"), stringsAsFactors = FALSE)
    },
    .request_local = function(payload, base_url, timeout = 30, ssl_cert = NULL) {
      called <<- payload$model %||% ""
      fake_resp(model = payload$model %||% "")
    },
    .package = "gptr"
  )

  gpt("hi", provider = "ollama", print_raw = FALSE)

  expect_identical(called, "modelB")
})

test_that("auto chooses the preferred local route without provider discovery", {
  called <- NULL
  testthat::local_mocked_bindings(
    .fetch_models_cached = function(provider = NULL, base_url = NULL, openai_api_key = "", refresh = FALSE, ssl_cert = NULL, ...) {
      stop("should not probe model listings")
    },
    .request_local = function(payload, base_url, timeout = 30, ssl_cert = NULL) {
      called <<- base_url
      fake_resp(model = payload$model %||% "local-default")
    },
    openai_send_request = function(payload, base_url, api_key, timeout = 30, ssl_cert = NULL) {
      stop("should not call OpenAI")
    },
    .package = "gptr"
  )

  withr::local_options(list(
    gptr.local_prefer = c("ollama", "lmstudio", "localai"),
    gptr.local_model = "local-default"
  ))

  gpt("hi", provider = "auto", strict_model = FALSE, print_raw = FALSE)

  expect_identical(called, "http://127.0.0.1:11434")
})

test_that("auto keeps using the chosen local route even when a model looks remote", {
  called <- NULL
  testthat::local_mocked_bindings(
    .fetch_models_cached = function(provider = NULL, base_url = NULL, openai_api_key = "", ...) {
      stop("should not probe model listings")
    },
    openai_send_request = function(payload, base_url, api_key, timeout = 30, ssl_cert = NULL) {
      called <<- c(called, "openai")
      fake_resp(model = payload$model %||% "gpt")
    },
    .request_local = function(payload, base_url, timeout = 30, ssl_cert = NULL) {
      called <<- c(called, paste0("local@", base_url), payload$model %||% "")
      fake_resp(model = payload$model %||% "gpt-4o-mini")
    },
    .package = "gptr"
  )

  gpt(
    "hi",
    model = "gpt-4o-mini",
    provider = "auto",
    openai_api_key = "sk-test",
    allow_remote = TRUE,
    strict_model = FALSE,
    print_raw = FALSE
  )

  expect_identical(called, c("local@http://127.0.0.1:1234", "gpt-4o-mini"))
})

test_that("auto honors an explicit local backend", {
  called <- NULL
  testthat::local_mocked_bindings(
    .fetch_models_cached = function(provider = NULL, base_url = NULL, openai_api_key = "", ...) {
      data.frame(model_id = "mistralai/mistral-7b-instruct-v0.3", stringsAsFactors = FALSE)
    },
    .request_local = function(payload, base_url, timeout = 30, ssl_cert = NULL) {
      called <<- c(called, paste0("local@", base_url))
      fake_resp(model = payload$model %||% "mistral")
    },
    openai_send_request = function(payload, base_url, api_key, timeout = 30, ssl_cert = NULL) {
      called <<- c(called, "openai")
      fake_resp(model = payload$model %||% "gpt")
    },
    .package = "gptr"
  )

  gpt(
    "hi",
    model = "mistralai/mistral-7b-instruct-v0.3",
    provider = "auto",
    backend = "ollama",
    print_raw = FALSE
  )

  expect_true(length(called) == 1L)
  expect_match(called, "^local@http://127\\.0\\.0\\.1:11434$")
})

test_that("provider=openai is blocked unless remote transmission is explicitly allowed", {
  called <- FALSE
  testthat::local_mocked_bindings(
    openai_send_request = function(payload, base_url, api_key, timeout = 30, ssl_cert = NULL) {
      called <<- TRUE
      fake_resp(model = payload$model %||% "gpt-4o-mini")
    },
    .package = "gptr"
  )

  expect_error(
    gpt(
      "hi",
      provider = "openai",
      model = "gpt-4o-mini",
      openai_api_key = "sk-test",
      print_raw = FALSE
    ),
    "would send data to a non-local endpoint"
  )
  expect_false(called)
})

test_that("auto falls back to OpenAI only when no local route is configured", {
  called <- NULL
  withr::local_options(list(
    gptr.local_base_url = NULL,
    gptr.lmstudio_base_url = "",
    gptr.ollama_base_url = "",
    gptr.localai_base_url = "",
    gptr.local_prefer = character()
  ))
  testthat::local_mocked_bindings(
    .fetch_models_cached = function(provider = NULL, base_url = NULL, openai_api_key = "", ...) {
      expect_identical(provider, "openai")
      data.frame(model_id = "gpt-4o-mini", stringsAsFactors = FALSE)
    },
    openai_send_request = function(payload, base_url, api_key, timeout = 30, ssl_cert = NULL) {
      called <<- c(called, "openai")
      fake_resp(model = payload$model %||% "gpt-4o-mini")
    },
    .request_local = function(payload, base_url, timeout = 30, ssl_cert = NULL) {
      called <<- c(called, "local")
      fake_resp(model = payload$model %||% "local")
    },
    .package = "gptr"
  )

  gpt("hi", provider = "auto", openai_api_key = "sk", allow_remote = TRUE, print_raw = FALSE)

  expect_identical(called, "openai")
})

test_that("allow_backend_autoswitch is ignored for backward compatibility", {
  called <- NULL
  testthat::local_mocked_bindings(
    .fetch_models_cached = function(provider = NULL, base_url = NULL, openai_api_key = "", ...) {
      stop("should not probe")
    },
    .request_local = function(payload, base_url, timeout = 30, ssl_cert = NULL) {
      called <<- c(called, paste0("local@", base_url))
      fake_resp(model = payload$model %||% "fallback")
    },
    openai_send_request = function(payload, base_url, api_key, timeout = 30, ssl_cert = NULL) {
      called <<- c(called, "openai")
      fake_resp(model = payload$model %||% "fallback")
    },
    .package = "gptr"
  )

  withr::local_options(list(gptr.local_prefer = c("ollama", "lmstudio", "localai")))
  gpt("hi", provider = "auto", allow_backend_autoswitch = FALSE, strict_model = FALSE, print_raw = FALSE)

  expect_identical(called, "local@http://127.0.0.1:11434")
})

test_that("provider=openai routes to OpenAI even if locals have models", {
  called <- NULL
  testthat::local_mocked_bindings(
    .fetch_models_cached = function(provider = NULL, base_url = NULL, openai_api_key = "", ...) {
      data.frame(model_id = "gpt-4o-mini", stringsAsFactors = FALSE)
    },
    openai_send_request = function(payload, base_url, api_key, timeout = 30, ssl_cert = NULL) {
      called <<- c(called, "openai")
      fake_resp(model = payload$model %||% "gpt-4o-mini")
    },
    .package = "gptr"
  )

  gpt("hi", model = "gpt-4o-mini", provider = "openai", openai_api_key = "sk-test", allow_remote = TRUE, print_raw = FALSE)

  expect_identical(called, "openai")
})

test_that("explicit non-loopback local base_url is blocked unless remote transmission is allowed", {
  expect_error(
    gpt(
      "hi",
      provider = "local",
      base_url = "http://192.168.1.50:1234",
      model = "mistralai/mistral-7b-instruct-v0.3",
      strict_model = FALSE,
      print_raw = FALSE
    ),
    "would send data to a non-local endpoint"
  )
})

test_that("explicit local base_url is honored", {
  called <- NULL
  testthat::local_mocked_bindings(
    .fetch_models_cached = function(provider = NULL, base_url = NULL, openai_api_key = "", ...) {
      data.frame(model_id = "mistralai/mistral-7b-instruct-v0.3", stringsAsFactors = FALSE)
    },
    .request_local = function(payload, base_url, timeout = 30, ssl_cert = NULL) {
      called <<- c(called, paste0("local@", base_url))
      fake_resp(model = payload$model %||% "mistral")
    },
    .package = "gptr"
  )

  gpt(
    "hi",
    provider = "local",
    base_url = "http://192.168.1.50:1234",
    model = "mistralai/mistral-7b-instruct-v0.3",
    strict_model = TRUE,
    allow_remote = TRUE,
    print_raw = FALSE
  )

  expect_identical(called, "local@http://192.168.1.50:1234")
})

test_that("local base_url path variants are normalized", {
  urls <- c("http://custom:1234/v1", "http://custom:1234/v1/chat/completions")
  testthat::local_mocked_bindings(
    .fetch_models_cached = function(provider = NULL, base_url = NULL, openai_api_key = "", ...) {
      data.frame(model_id = "custom-model", stringsAsFactors = FALSE)
    },
    .request_local = function(payload, base_url, timeout = 30, ssl_cert = NULL) {
      captured <<- c(captured, base_url)
      fake_resp(model = payload$model %||% "custom-model")
    },
    .package = "gptr"
  )

  for (url in urls) {
    captured <- character()
    gpt(
      "hi",
      provider = "local",
      base_url = url,
      model = "custom-model",
      strict_model = TRUE,
      allow_remote = TRUE,
      print_raw = FALSE
    )
    expect_identical(captured, "http://custom:1234")
  }
})
