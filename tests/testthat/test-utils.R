test_that("exports.txt up to date", {
  # dev/structure/exports.txt is a development-only invariant; it is excluded
  # from the built package (.Rbuildignore), so skip when it is unavailable
  # (e.g. under R CMD check or on CRAN).
  skip_on_cran()
  exports_path <- testthat::test_path("..", "..", "dev", "structure", "exports.txt")
  skip_if_not(file.exists(exports_path), "dev/structure/exports.txt not available")

  cur <- sort(getNamespaceExports("gptr"))
  exp <- readLines(exports_path, warn = FALSE)
  expect_equal(cur, sort(exp), info = "Run /dev/structure/generate.R to update this file")
})
