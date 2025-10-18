test_that("row_to_tibble default keeps first element when multi-value", {
  res <- gptr:::row_to_tibble(list(col = c("first", "second"), other = 2))
  expect_equal(res$col, "first")
  expect_equal(res$other, 2)
})

test_that("row_to_tibble can throw on multi-value inputs", {
  expect_error(
    gptr:::row_to_tibble(list(col = c(1, 2)), multi_value = "error"),
    "Column 'col' has multiple values"
  )
})

test_that("row_to_tibble can emit list-columns for multi-value inputs", {
  res <- gptr:::row_to_tibble(list(col = c(1, 2)), multi_value = "list")
  expect_equal(res$col[[1]], c(1, 2))
})

test_that("row_to_tibble honours schema with multi-value handling", {
  res <- gptr:::row_to_tibble(
    list(col = c("x", "y")),
    expected_keys = c("col", "missing"),
    multi_value = "list"
  )
  expect_equal(res$col[[1]], c("x", "y"))
  expect_equal(res$missing[[1]], NA)
})
