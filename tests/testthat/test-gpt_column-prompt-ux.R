test_that("instruction-only prompt follows repair scaffold on the chosen local auto route", {
  seen_prompt <- NULL

  withr::local_options(list(
    gptr.local_base_url = NULL,
    gptr.lmstudio_base_url = "http://127.0.0.1:1234",
    gptr.ollama_base_url = "http://127.0.0.1:11434",
    gptr.localai_base_url = "http://127.0.0.1:8080",
    gptr.local_prefer = "ollama"
  ))
  testthat::local_mocked_bindings(
    gpt = function(prompt, ...) {
      seen_prompt <<- prompt
      '{"age":64}'
    },
    .package = "gptr"
  )

  out <- gpt_column(
    data = data.frame(txt = "patient is 64", stringsAsFactors = FALSE),
    col = txt,
    instruction = "Extract age.",
    keys = list(age = "integer"),
    provider = "auto",
    structured = "auto",
    progress = FALSE,
    return_debug = TRUE
  )

  expect_match(seen_prompt, "Task:\nExtract age\\.")
  expect_match(seen_prompt, "Required keys:")
  expect_match(seen_prompt, "Value rules:")
  expect_match(seen_prompt, "Text:\npatient is 64")
  expect_identical(out$.structured_mode[[1]], "repair")
  expect_identical(out$age[[1]], 64L)
})

test_that("instruction-only prompt follows native scaffold on the chosen OpenAI auto route", {
  seen_prompt <- NULL
  seen_response_format <- NULL

  withr::local_options(list(
    gptr.local_base_url = NULL,
    gptr.lmstudio_base_url = "",
    gptr.ollama_base_url = "",
    gptr.localai_base_url = "",
    gptr.local_prefer = character()
  ))
  testthat::local_mocked_bindings(
    gpt = function(prompt, response_format = NULL, provider = NULL, ...) {
      seen_prompt <<- prompt
      seen_response_format <<- response_format
      '{"age":64}'
    },
    .package = "gptr"
  )

  out <- gpt_column(
    data = data.frame(txt = "patient is 64", stringsAsFactors = FALSE),
    col = txt,
    instruction = "Extract age.",
    keys = list(age = "integer"),
    provider = "auto",
    structured = "auto",
    openai_api_key = "sk-test",
    allow_remote = TRUE,
    progress = FALSE,
    return_debug = TRUE
  )

  expect_match(seen_prompt, "Task:\nExtract age\\.")
  expect_match(seen_prompt, "Structured output:")
  expect_false(grepl("Required keys:", seen_prompt, fixed = TRUE))
  expect_false(grepl("Value rules:", seen_prompt, fixed = TRUE))
  expect_true(is.list(seen_response_format))
  expect_identical(out$.structured_mode[[1]], "native")
  expect_identical(out$age[[1]], 64L)
})

test_that("instruction plus template appends text when the template omits it", {
  seen_prompt <- NULL

  testthat::local_mocked_bindings(
    gpt = function(prompt, ...) {
      seen_prompt <<- prompt
      '{"age":64}'
    },
    .package = "gptr"
  )

  out <- gpt_column(
    data = data.frame(txt = "patient is 64", stringsAsFactors = FALSE),
    col = txt,
    instruction = "Extract age.",
    template = "You are reviewing a note.\n\nTask reminder:\n{instruction}",
    keys = list(age = "integer"),
    provider = "local",
    backend = "ollama",
    structured = "repair",
    progress = FALSE,
    return_debug = TRUE
  )

  expect_match(seen_prompt, "You are reviewing a note\\.")
  expect_match(seen_prompt, "Task reminder:\nExtract age\\.")
  expect_match(seen_prompt, "Text:\npatient is 64")
  expect_match(seen_prompt, "Required keys:")
  expect_identical(out$age[[1]], 64L)
})

test_that("legacy raw prompt path remains unchanged", {
  seen_prompt <- NULL

  testthat::local_mocked_bindings(
    gpt = function(prompt, ...) {
      seen_prompt <<- prompt
      '{"age":64}'
    },
    .package = "gptr"
  )

  out <- gpt_column(
    data = data.frame(txt = "patient is 64", stringsAsFactors = FALSE),
    col = txt,
    prompt = "Legacy {json_format}\nText: {text}",
    keys = list(age = "integer"),
    provider = "local",
    backend = "ollama",
    structured = "repair",
    progress = FALSE,
    return_debug = TRUE
  )

  expect_match(seen_prompt, "Legacy \\{ age: \"0\"\\|\"1\"\\|\"NA\" \\}\nText: patient is 64")
  expect_identical(out$age[[1]], 64L)
})

test_that("gpt_column rejects mixed raw and managed prompt interfaces", {
  expect_error(
    gpt_column(
      data = data.frame(txt = "patient is 64", stringsAsFactors = FALSE),
      col = txt,
      prompt = "Legacy {text}",
      instruction = "Extract age.",
      keys = list(age = "integer"),
      progress = FALSE
    ),
    "either `prompt` \\(legacy/raw mode\\) or `instruction`/`template`, not both",
    perl = TRUE
  )
})
