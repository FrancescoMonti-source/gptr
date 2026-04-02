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

test_that("gpt_column auto mode uses native structured outputs for OpenAI", {
  seen_response_format <- NULL

  testthat::local_mocked_bindings(
    gpt = function(prompt, response_format = NULL, ...) {
      seen_response_format <<- response_format
      '{"age":64}'
    },
    .package = "gptr"
  )

  out <- gpt_column(
    data = data.frame(txt = "patient is 64", stringsAsFactors = FALSE),
    col = txt,
    prompt = "Extract {json_format} from {text}",
    keys = list(age = "integer"),
    provider = "openai",
    structured = "auto",
    progress = FALSE,
    return_debug = TRUE
  )

  expect_true(is.list(seen_response_format))
  expect_identical(seen_response_format$type, "json_schema")
  expect_identical(out$age[[1]], 64L)
  expect_identical(out$.structured_mode[[1]], "native")
})

test_that("gpt_column auto mode falls back to repair when native structured output is unavailable", {
  seen_response_format <- "__unset__"

  testthat::local_mocked_bindings(
    gpt = function(prompt, response_format = NULL, ...) {
      seen_response_format <<- response_format
      '{"age":64}'
    },
    .package = "gptr"
  )

  out <- gpt_column(
    data = data.frame(txt = "patient is 64", stringsAsFactors = FALSE),
    col = txt,
    prompt = "Extract {json_format} from {text}",
    keys = list(age = "integer"),
    provider = "local",
    structured = "auto",
    progress = FALSE,
    return_debug = TRUE
  )

  expect_null(seen_response_format)
  expect_identical(out$age[[1]], 64L)
  expect_identical(out$.structured_mode[[1]], "repair")
})

test_that("gpt_column native mode errors when provider does not support structured extraction", {
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
    "Native structured extraction is not available"
  )
})

