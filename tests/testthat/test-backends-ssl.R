test_that("gpt forwards ssl_cert to OpenAI backend", {
  cert <- withr::local_tempfile(fileext = ".crt")
  writeLines("dummy", cert)

  called <- NULL
  testthat::local_mocked_bindings(
    .fetch_models_cached = function(provider = NULL, base_url = NULL, openai_api_key = "", ...) {
      data.frame(model_id = "gpt-4o-mini", stringsAsFactors = FALSE)
    },
    openai_send_request = function(payload, base_url, api_key, timeout = 30, ssl_cert = NULL) {
      called <<- ssl_cert
      fake_resp(model = payload$model %||% "gpt-4o-mini")
    },
    .package = "gptr"
  )

  gpt("hi", provider = "openai", openai_api_key = "sk-test", ssl_cert = cert, allow_remote = TRUE, print_raw = FALSE)

  expect_identical(called, cert)
})

test_that("gpt forwards ssl_cert to local backend", {
  cert <- withr::local_tempfile(fileext = ".crt")
  writeLines("dummy", cert)

  called <- NULL
  testthat::local_mocked_bindings(
    .fetch_models_cached = function(provider = NULL, base_url = NULL, openai_api_key = "", ...) {
      data.frame(model_id = "mistral", stringsAsFactors = FALSE)
    },
    .request_local = function(payload, base_url, timeout_sec = 180, max_tries = 2, user_agent = "ua", debug_http = FALSE, ssl_cert = NULL) {
      called <<- ssl_cert
      fake_resp(model = payload$model %||% "mistral")
    },
    .package = "gptr"
  )

  gpt("hi", provider = "local", base_url = "http://127.0.0.1:1234", model = "mistral", strict_model = FALSE, ssl_cert = cert, print_raw = FALSE)

  expect_identical(called, cert)
})

test_that("gpt forwards ssl_cert to strict OpenAI model validation", {
  cert <- withr::local_tempfile(fileext = ".crt")
  writeLines("dummy", cert)

  recorded <- NULL
  testthat::local_mocked_bindings(
    .fetch_models_cached = function(provider = NULL, base_url = NULL, openai_api_key = "", refresh = FALSE, ssl_cert = NULL, ...) {
      recorded <<- ssl_cert
      data.frame(model_id = "gpt-4o-mini", stringsAsFactors = FALSE)
    },
    openai_send_request = function(payload, base_url, api_key, timeout = 30, ssl_cert = NULL) {
      fake_resp(model = payload$model %||% "gpt-4o-mini")
    },
    .request_local = function(payload, base_url, timeout = 30, ssl_cert = NULL) {
      fake_resp(model = payload$model %||% "local-model")
    },
    .package = "gptr"
  )

  gpt("hi", model = "gpt-4o-mini", provider = "openai", openai_api_key = "sk-test", ssl_cert = cert, allow_remote = TRUE, print_raw = FALSE)

  expect_identical(recorded, cert)
})

test_that("gpt forwards ssl_cert to strict local model validation", {
  cert <- withr::local_tempfile(fileext = ".crt")
  writeLines("dummy", cert)

  seen <- new.env(parent = emptyenv())
  seen$local <- NULL

  testthat::local_mocked_bindings(
    .fetch_models_cached = function(provider = NULL, base_url = NULL, openai_api_key = "", refresh = FALSE, ssl_cert = NULL, ...) {
      if (identical(provider, "ollama")) {
        seen$local <- ssl_cert
      }
      data.frame(model_id = "mistral", stringsAsFactors = FALSE)
    },
    .request_local = function(payload, base_url, timeout_sec = 180, max_tries = 2, user_agent = "ua", debug_http = FALSE, ssl_cert = NULL) {
      fake_resp(model = payload$model %||% "mistral")
    },
    openai_send_request = function(payload, base_url, api_key, timeout = 30, ssl_cert = NULL) {
      fake_resp(model = payload$model %||% "gpt")
    },
    .package = "gptr"
  )

  withr::local_options(list(gptr.local_model = "mistral", gptr.ollama_model = "mistral"))
  gpt("hi", provider = "ollama", ssl_cert = cert, strict_model = TRUE, print_raw = FALSE)

  expect_identical(seen$local, cert)
})

test_that("openai_send_request sets cainfo when ssl_cert supplied", {
  cert <- withr::local_tempfile(fileext = ".crt")
  writeLines("dummy", cert)

  captured <- NULL
  res <- testthat::with_mocked_bindings(
    openai_send_request(
      payload = list(model = "dummy", messages = list()),
      base_url = "https://api.openai.com/v1/chat/completions",
      api_key = "sk-test",
      ssl_cert = cert
    ),
    .req_apply_ssl_cert = function(req, ssl_cert = NULL) {
      captured <<- ssl_cert
      req
    },
    .httr_req_perform = function(req) {
      body <- list(choices = list(list(message = list(content = "ok"))))
      httr2::response(
        status = 200L,
        headers = list("content-type" = "application/json"),
        body = charToRaw(jsonlite::toJSON(body, auto_unbox = TRUE))
      )
    },
    .package = "gptr"
  )

  expect_identical(captured, cert)
  expect_s3_class(res$resp, "httr2_response")
})

test_that(".request_local sets cainfo when ssl_cert supplied", {
  cert <- withr::local_tempfile(fileext = ".crt")
  writeLines("dummy", cert)

  captured <- NULL
  res <- testthat::with_mocked_bindings(
    .request_local(payload = list(model = "dummy"), base_url = "http://127.0.0.1:1234", ssl_cert = cert),
    .req_apply_ssl_cert = function(req, ssl_cert = NULL) {
      captured <<- ssl_cert
      req
    },
    .httr_req_perform = function(req) {
      fake_resp()$resp
    },
    .package = "gptr"
  )

  expect_identical(captured, cert)
  expect_equal(res$status, 200L)
})
