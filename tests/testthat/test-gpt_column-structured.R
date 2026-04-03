test_that("gpt_column flags invalid enum values and coerces them to NA", {
  testthat::local_mocked_bindings(
    gpt = function(prompt, ...) '{"severity":"severe"}',
    .package = "gptr"
  )

  out <- gpt_column(
    data = data.frame(txt = "note", stringsAsFactors = FALSE),
    col = txt,
    prompt = function(text, keys) "Return JSON only",
    keys = list(severity = c("low", "medium")),
    progress = FALSE,
    return_debug = TRUE
  )

  expect_true(is.na(out$severity[[1]]))
  expect_true(out$.invalid_rows[[1]])
  expect_identical(out$.structured_mode[[1]], "repair")
  expect_false(out$.invalid_detail[[1]]$allowed[[1]])
})

test_that("gpt_column uses shared explicit logical coercion", {
  testthat::local_mocked_bindings(
    gpt = function(prompt, ...) '{"flag":"oui"}',
    .package = "gptr"
  )

  out <- gpt_column(
    data = data.frame(txt = "note", stringsAsFactors = FALSE),
    col = txt,
    prompt = function(text, keys) "Return JSON only",
    keys = list(flag = "logical"),
    progress = FALSE,
    return_debug = TRUE
  )

  expect_true(out$flag[[1]])
  expect_false(out$.invalid_rows[[1]])
})

test_that("gpt_column auto mode uses native structured outputs on the chosen OpenAI route", {
  seen_response_format <- NULL
  seen_provider <- NULL

  withr::local_options(list(
    gptr.local_base_url = NULL,
    gptr.lmstudio_base_url = "",
    gptr.ollama_base_url = "",
    gptr.localai_base_url = "",
    gptr.local_prefer = character()
  ))
  testthat::local_mocked_bindings(
    gpt = function(prompt, response_format = NULL, provider = NULL, ...) {
      seen_response_format <<- response_format
      seen_provider <<- provider
      '{"age":64}'
    },
    .package = "gptr"
  )

  out <- gpt_column(
    data = data.frame(txt = "patient is 64", stringsAsFactors = FALSE),
    col = txt,
    prompt = "Extract {json_format} from {text}",
    keys = list(age = "integer"),
    provider = "auto",
    structured = "auto",
    openai_api_key = "sk-test",
    allow_remote = TRUE,
    progress = FALSE,
    return_debug = TRUE
  )

  expect_identical(seen_provider, "openai")
  expect_true(is.list(seen_response_format))
  expect_identical(seen_response_format$type, "json_schema")
  expect_identical(out$age[[1]], 64L)
  expect_identical(out$.structured_mode[[1]], "native")
})

test_that("gpt_column auto mode keeps smart defaults on the chosen local route", {
  seen_response_format <- "__unset__"
  seen_provider <- NULL
  seen_backend <- NULL
  seen_base_url <- NULL

  withr::local_options(list(
    gptr.local_prefer = c("ollama", "lmstudio", "localai"),
    gptr.ollama_model = "mistral"
  ))
  testthat::local_mocked_bindings(
    gpt = function(prompt, response_format = NULL, provider = NULL, backend = NULL, base_url = NULL, model = NULL, ...) {
      seen_response_format <<- response_format
      seen_provider <<- provider
      seen_backend <<- backend
      seen_base_url <<- base_url
      expect_null(model)
      '{"age":64}'
    },
    .package = "gptr"
  )

  out <- gpt_column(
    data = data.frame(txt = "patient is 64", stringsAsFactors = FALSE),
    col = txt,
    prompt = "Extract {json_format} from {text}",
    keys = list(age = "integer"),
    provider = "auto",
    structured = "auto",
    progress = FALSE,
    return_debug = TRUE
  )

  expect_identical(seen_provider, "local")
  expect_identical(seen_backend, "ollama")
  expect_identical(seen_base_url, "http://127.0.0.1:11434")
  expect_null(seen_response_format)
  expect_identical(out$age[[1]], 64L)
  expect_identical(out$.structured_mode[[1]], "repair")
})

test_that("gpt_column auto mode uses native structured outputs for opted-in local backends only", {
  seen_response_format <- "__unset__"
  seen_backend <- NULL

  withr::local_options(list(
    gptr.local_prefer = c("ollama", "lmstudio", "localai"),
    gptr.ollama_model = "mistral",
    gptr.native_structured_backends = "ollama"
  ))
  testthat::local_mocked_bindings(
    gpt = function(prompt, response_format = NULL, provider = NULL, backend = NULL, ...) {
      seen_response_format <<- response_format
      seen_backend <<- backend
      '{"age":64}'
    },
    .package = "gptr"
  )

  out <- gpt_column(
    data = data.frame(txt = "patient is 64", stringsAsFactors = FALSE),
    col = txt,
    prompt = "Extract {json_format} from {text}",
    keys = list(age = "integer"),
    provider = "auto",
    structured = "auto",
    progress = FALSE,
    return_debug = TRUE
  )

  expect_identical(seen_backend, "ollama")
  expect_true(is.list(seen_response_format))
  expect_identical(out$age[[1]], 64L)
  expect_identical(out$.structured_mode[[1]], "native")
})

test_that("gpt_column native mode errors with actionable guidance on unsupported routes", {
  testthat::local_mocked_bindings(
    gpt = function(prompt, response_format = NULL, ...) '{"age":64}',
    .package = "gptr"
  )

  expect_error(
    gpt_column(
      data = data.frame(txt = "patient is 64", stringsAsFactors = FALSE),
      col = txt,
      prompt = "Extract {json_format} from {text}",
      keys = list(age = "integer"),
      provider = "local",
      structured = "native",
      progress = FALSE
    ),
    "Use `structured = \"repair\"`, switch to a route that supports native structured outputs, or configure `options\\(gptr.native_structured_backends = \\.\\.\\.\\)`",
    perl = TRUE
  )
})

test_that("gpt_column auto mode errors clearly when no route is configured", {
  testthat::local_mocked_bindings(
    gpt = function(prompt, response_format = NULL, ...) '{"age":64}',
    .package = "gptr"
  )

  withr::local_options(list(
    gptr.local_base_url = NULL,
    gptr.lmstudio_base_url = "",
    gptr.ollama_base_url = "",
    gptr.localai_base_url = "",
    gptr.local_prefer = character()
  ))

  expect_error(
    gpt_column(
      data = data.frame(txt = "patient is 64", stringsAsFactors = FALSE),
      col = txt,
      prompt = "Extract {json_format} from {text}",
      keys = list(age = "integer"),
      provider = "auto",
      structured = "auto",
      progress = FALSE
    ),
    "No route is configured for `provider = \"auto\"`; set `backend` or `base_url`, set `options\\(gptr.local_base_url = \\.\\.\\.\\)`, or use `provider = \"openai\"` with `allow_remote = TRUE`\\.",
    perl = TRUE
  )
})

