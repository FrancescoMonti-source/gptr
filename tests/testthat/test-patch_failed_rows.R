test_that("patch_failed_rows forwards instruction/template to gpt_column", {
  seen <- new.env(parent = emptyenv())
  seen$prompt <- "__unset__"
  seen$instruction <- NULL
  seen$template <- NULL

  testthat::local_mocked_bindings(
    gpt_column = function(data, col, prompt = NULL, keys = NULL, instruction = NULL, template = NULL, ...) {
      seen$prompt <- prompt
      seen$instruction <- instruction
      seen$template <- template
      tibble::as_tibble(data) |>
        dplyr::mutate(age = 64L, .invalid_rows = FALSE)
    },
    .package = "gptr"
  )

  df <- tibble::tibble(
    id = 1L,
    text = "patient is 64",
    age = NA_integer_,
    .invalid_rows = TRUE
  )
  attr(df, "invalid_rows") <- 1L

  out <- patch_failed_rows(
    data = df,
    col = text,
    id_col = id,
    keys = list(age = "integer"),
    instruction = "Extract age.",
    template = "Review this note.\n\nTask:\n{instruction}",
    max_attempts = 1,
    print_retry = FALSE
  )

  expect_null(seen$prompt)
  expect_identical(seen$instruction, "Extract age.")
  expect_identical(seen$template, "Review this note.\n\nTask:\n{instruction}")
  expect_identical(out$age[[1]], 64L)
  expect_length(attr(out, "invalid_rows"), 0L)
})

test_that("patch_failed_rows rejects mixed raw and managed prompt interfaces", {
  df <- tibble::tibble(id = 1L, text = "patient is 64")
  attr(df, "invalid_rows") <- 1L

  expect_error(
    patch_failed_rows(
      data = df,
      col = text,
      id_col = id,
      prompt = "Legacy {text}",
      instruction = "Extract age."
    ),
    "either `prompt` \\(legacy/raw mode\\) or `instruction`/`template`, not both",
    perl = TRUE
  )
})
