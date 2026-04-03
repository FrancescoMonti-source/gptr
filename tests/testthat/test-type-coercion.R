test_that(".parse_key_spec normalizes type aliases", {
  parse <- getFromNamespace(".parse_key_spec", "gptr")

  expect_identical(parse("int")$type, "integer")
  expect_identical(parse("bool")$type, "logical")
  expect_identical(parse("text")$type, "character")
  expect_identical(parse(c("yes", "no"))$allowed_type, "logical")
})

test_that(".coerce_type applies explicit logical tokens consistently", {
  coerce <- getFromNamespace(".coerce_type", "gptr")

  expect_identical(
    coerce(c("true", "t", "yes", "y", "1", "oui", "false", "f", "no", "n", "0", "non", "maybe"), "logical"),
    c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, NA)
  )
  expect_identical(
    coerce(c(1, 0, 2, NA_real_), "logical"),
    c(TRUE, FALSE, NA, NA)
  )
})

test_that(".coerce_type preserves explicit integer, numeric, and character behavior", {
  coerce <- getFromNamespace(".coerce_type", "gptr")

  expect_identical(coerce(c("1", "2", NA_character_), "integer"), c(1L, 2L, NA_integer_))
  expect_equal(coerce(c("1.5", "2"), "numeric"), c(1.5, 2))
  expect_identical(coerce(NULL, "character"), NA_character_)
})

test_that(".infer_vector_type stays conservative for 1/0 and French boolean words", {
  infer_vec <- getFromNamespace(".infer_vector_type", "gptr")

  expect_identical(infer_vec(c("1", "0", "1")), c(1L, 0L, 1L))
  expect_identical(infer_vec(c("true", "false", "", "NA")), c(TRUE, FALSE, NA, NA))
  expect_identical(infer_vec(c("oui", "non")), c("oui", "non"))
})

test_that("json_fix_parse_validate uses the shared explicit logical coercion", {
  normalize_specs <- getFromNamespace(".normalize_key_specs", "gptr")
  specs <- normalize_specs(list(flag = "logical"))

  out <- json_fix_parse_validate('{"flag":"oui"}', key_specs = specs, .coerce_types = TRUE)

  expect_true(out$ok)
  expect_identical(out$value$flag, TRUE)
})
