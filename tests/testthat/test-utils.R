test_that("exports.txt up to date", {
  cur <- sort(getNamespaceExports("gptr"))
  exp <- readLines(
    testthat::test_path("..", "dev", "structure", "exports.txt"),
    warn = FALSE
  )
  expect_equal(cur, sort(exp), info = "Run /dev/structure/generate.R to update this file")

})
