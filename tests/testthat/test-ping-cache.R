test_that(".ensure_backend_up returns immediately when cache says TRUE", {
  gpt_clear_caches()
  base <- "http://127.0.0.1:1234/v1" # root (not .../chat/completions)
  assign(base, TRUE, envir = .gptr_state$ping_cache)
  expect_invisible(.ensure_backend_up(paste0(base, "/chat/completions"), "lmstudio"))
})

test_that(".ensure_backend_up errors with cached failure message", {
  gpt_clear_caches()
  base <- "http://127.0.0.1:9999/v1"
  fail <- FALSE
  attr(fail, "msg") <- "cached unreachable"
  assign(base, fail, envir = .gptr_state$ping_cache)
  expect_error(
    .ensure_backend_up(paste0(base, "/chat/completions"), "lmstudio"),
    "cached unreachable"
  )
})
