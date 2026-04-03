test_that("managed repair prompt scaffold includes task, schema rules, and text", {
  key_specs <- gptr:::.normalize_key_specs(list(
    age = "integer",
    diagnosis = "character",
    severity = c("low", "medium")
  ))

  prompt <- gptr:::.build_managed_extraction_prompt(
    text = "Patient is 64 years old.",
    instruction = "Extract age, diagnosis, and severity.",
    key_specs = key_specs,
    mode = "repair"
  )

  expect_match(prompt, "Task:\nExtract age, diagnosis, and severity\\.")
  expect_match(prompt, "Required keys:")
  expect_match(prompt, "- age", fixed = TRUE)
  expect_match(prompt, "Value rules:")
  expect_match(prompt, "- age: integer, or NA if not stated", fixed = TRUE)
  expect_match(prompt, "- diagnosis: short text, or NA if not stated", fixed = TRUE)
  expect_match(prompt, "- severity: one of low, medium, or NA if not stated", fixed = TRUE)
  expect_match(prompt, "Output constraints:")
  expect_match(prompt, "Text:\nPatient is 64 years old\\.")
})

test_that("managed native prompt scaffold stays light", {
  key_specs <- gptr:::.normalize_key_specs(list(age = "integer"))

  prompt <- gptr:::.build_managed_extraction_prompt(
    text = "Patient is 64 years old.",
    instruction = "Extract age.",
    key_specs = key_specs,
    mode = "native"
  )

  expect_match(prompt, "Task:\nExtract age\\.")
  expect_match(prompt, "Structured output:")
  expect_false(grepl("Required keys:", prompt, fixed = TRUE))
  expect_false(grepl("Value rules:", prompt, fixed = TRUE))
})

test_that("managed template path renders placeholders and appends missing text block", {
  key_specs <- gptr:::.normalize_key_specs(list(age = "integer"))

  with_text <- gptr:::.build_managed_extraction_prompt(
    text = "Patient is 64 years old.",
    instruction = "Extract age.",
    template = "Review this note.\n\nTask reminder:\n{instruction}\n\nClinical text:\n{text}",
    key_specs = key_specs,
    mode = "repair"
  )

  expect_match(with_text, "Review this note\\.")
  expect_match(with_text, "Clinical text:\nPatient is 64 years old\\.")
  expect_equal(length(gregexpr("Text:\n", with_text, fixed = TRUE)[[1]][gregexpr("Text:\n", with_text, fixed = TRUE)[[1]] > 0]), 0L)

  without_text <- gptr:::.build_managed_extraction_prompt(
    text = "Patient is 64 years old.",
    instruction = "Extract age.",
    template = "Review this note.\n\nTask reminder:\n{instruction}",
    key_specs = key_specs,
    mode = "repair"
  )

  expect_match(without_text, "Task reminder:\nExtract age\\.")
  expect_match(without_text, "Text:\nPatient is 64 years old\\.")
})

test_that("prompt strategy rejects mixed legacy and managed interfaces", {
  expect_error(
    gptr:::.resolve_prompt_strategy(
      prompt = "Return JSON only",
      instruction = "Extract age.",
      caller = "gpt_column"
    ),
    "either `prompt` \\(legacy/raw mode\\) or `instruction`/`template`, not both",
    perl = TRUE
  )

  expect_error(
    gptr:::.resolve_prompt_strategy(
      template = "Task:\n{instruction}",
      caller = "gpt_column"
    ),
    "`template` requires `instruction`.",
    fixed = TRUE
  )
})
