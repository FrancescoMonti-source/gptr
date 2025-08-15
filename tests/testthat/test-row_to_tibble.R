test_that("row_to_tibble keeps values as-is (first element if vector)", {
    r <- row_to_tibble(list(a = c(1,2), b = "x"), expected_keys = c("a","b"))
    expect_s3_class(r, "tbl_df")
    expect_identical(r$a[[1]], 1)
    expect_identical(r$b[[1]], "x")
})

test_that("row_to_tibble fills missing expected keys with NA when names absent", {
    r <- row_to_tibble(list(), expected_keys = c("a","b"))
    expect_named(r, c("a","b"))
    expect_true(all(is.na(r)))
})
