test_that("exports.txt up to date", {
  cur <- sort(getNamespaceExports("gptr"))
  exp <- readLines("dev/structure/exports.txt", warn = FALSE)
  expect_equal(cur, sort(exp))
})
