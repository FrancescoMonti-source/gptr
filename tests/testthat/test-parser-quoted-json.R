test_that("json_fix_parse_validate unwraps quoted JSON", {
  bad <- "\"{\\\"impulsivite\\\":1,\\\"hypersexualite\\\":0,\\\"trouble_alimentaire\\\":1,\\\"jeu_pathologique\\\":0}\""
  specs <- purrr::map(
    list(
      impulsivite = "integer", hypersexualite = "integer",
      trouble_alimentaire = "integer", jeu_pathologique = "integer"
    ),
    .parse_key_spec
  )
  rp <- json_fix_parse_validate(bad, key_specs = specs, .coerce_types = FALSE)
  expect_true(rp$ok)
  expect_equal(
    sort(names(rp$value)),
    sort(c("impulsivite", "hypersexualite", "trouble_alimentaire", "jeu_pathologique"))
  )
})
