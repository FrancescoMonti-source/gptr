test_that("build_prompt renders template with keys", {
  template <- "{text} -> {json_format}"
  keys <- list(a = "integer", b = "numeric")
  res <- build_prompt(template, text = "hello", keys = keys)
  expect_equal(res, "hello -> { a: \"0\"|\"1\"|\"NA\", b: a number (ex: 3.14) }")
})

test_that("build_prompt omits json_format when keys is NULL", {
  template <- "{text} -> {json_format}"
  res <- build_prompt(template, text = "hello", keys = NULL)
  expect_equal(res, "hello -> ")
})

test_that("build_prompt handles empty text", {
  template <- "{text} -> {json_format}"
  expect_equal(build_prompt(template, text = "", keys = NULL), "Texte manquant")
  expect_equal(build_prompt(template, text = NA, keys = NULL), "Texte manquant")
  expect_equal(build_prompt(template, text = NULL, keys = NULL), "Texte manquant")
})
