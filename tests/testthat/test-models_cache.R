# api_root()
# normalization trims trailing /v1/chat/completions
# keeps scheme/host
# collapses duplicate slashes
test_that(".api_root - normalizes urls", {
  f <- getFromNamespace(".api_root", "gptr")
  expect_equal(f("http://127.0.0.1:1234/v1/chat/completions"), "http://127.0.0.1:1234")
  expect_equal(f("http://127.0.0.1:1234////v1/chat/completions"), "http://127.0.0.1:1234")
  expect_equal(f("https://api.openai.com/v1/models"), "https://api.openai.com")
})


# .cache_key()
# same inputs --> same key
# different base_url or provider <U+2192> different key
test_that(".cache_key", {
  ck <- getFromNamespace(".cache_key", "gptr")
  a1 <- ck("openai", "https://api.openai.com")
  a2 <- ck("openai", "https://api.openai.com")
  b <- ck("ollama", "http://127.0.0.1:11434")
  expect_identical(a1, a2)
  expect_false(identical(a1, b))
})

# cache put / get / del
test_that("cache put / get / del", {
  local_cache_store()
  get <- getFromNamespace(".cache_get", "gptr")
  put <- getFromNamespace(".cache_put", "gptr")
  del <- getFromNamespace(".cache_del", "gptr")

  expect_null(get("openai", "https://api.openai.com"))
  put("openai", "https://api.openai.com", data.frame(id = "x", created = 1))
  ent <- get("openai", "https://api.openai.com")
  expect_true(is.list(ent) && "models" %in% names(ent) && "ts" %in% names(ent))
  del("openai", "https://api.openai.com")
  expect_null(get("openai", "https://api.openai.com"))
})

test_that("refresh with no models returns empty data", {
  fake_cache <- make_fake_cache()
  testthat::local_mocked_bindings(
    .cache_get = function(p, u) fake_cache$get(p, u),
    .cache_put = function(p, u, m) fake_cache$put(p, u, m),
    .cache_del = function(...) invisible(TRUE),
    .fetch_models_live = function(...) list(df = data.frame(id = character(), created = numeric()), status = "ok"),
    .api_root = function(x) x,
    .env = asNamespace("gptr")
  )
  out <- list_models(provider = "lmstudio", refresh = TRUE)
  expect_equal(nrow(out), 0)
})


test_that("openai non-JSON -> non_json", {
  f <- getFromNamespace(".fetch_models_live", "gptr")
  withr::local_envvar(OPENAI_API_KEY = "sk-test")
  mock_http_openai(status = 200L, json_throws = TRUE)
  o <- f("openai", "https://api.openai.com")
  expect_equal(o$status, "non_json")
})

test_that("openai network error -> unreachable", {
  f <- getFromNamespace(".fetch_models_live", "gptr")
  withr::local_envvar(OPENAI_API_KEY = "sk-test")
  mock_http_openai(perform_throws = TRUE)
  o <- f("openai", "https://api.openai.com")
  expect_equal(o$status, "unreachable")
})

test_that("openai 5xx -> http_503", {
  f <- getFromNamespace(".fetch_models_live", "gptr")
  withr::local_envvar(OPENAI_API_KEY = "sk-test")
  mock_http_openai(status = 503L)
  o <- f("openai", "https://api.openai.com")
  expect_equal(o$status, "http_503")
})

test_that("openai ok -> df parsed and status ok", {
  f <- getFromNamespace(".fetch_models_live", "gptr")
  withr::local_envvar(OPENAI_API_KEY = "sk-test")
  payload <- list(data = list(
    list(id = "gpt-4o", created = 1683758102),
    list(id = "gpt-4.1-mini", created = 1686558896)
  ))
  mock_http_openai(status = 200L, json = payload)
  o <- f("openai", "https://api.openai.com")
  expect_equal(o$status, "ok")
  expect_true(is.data.frame(o$df))
  expect_setequal(o$df$id, c("gpt-4o", "gpt-4.1-mini"))
})

test_that("openai empty model list -> empty_cache", {
  local_cache_store()
  payload <- list(data = list())
  mock_http_openai(status = 200L, json = payload)
  out <- list_models(provider = "openai", refresh = TRUE, openai_api_key = "sk-test")
  expect_identical(nrow(out), 0L)
  diag <- attr(out, "diagnostics")[[1]]
  expect_identical(diag$status, "empty_cache")
  expect_identical(diag$provider, "openai")
})

test_that("openai fallback semantics via .fetch_models_live", {
  f <- getFromNamespace(".fetch_models_live", "gptr")
  withr::local_envvar(OPENAI_API_KEY = "sk")

  mock_http_openai(status = 200L, json_throws = TRUE)
  o1 <- f("openai", "https://api.openai.com")
  expect_equal(o1$status, "non_json")

  mock_http_openai(perform_throws = TRUE)
  o2 <- f("openai", "https://api.openai.com")
  expect_equal(o2$status, "unreachable")

  mock_http_openai(status = 503L)
  o3 <- f("openai", "https://api.openai.com")
  expect_equal(o3$status, "http_503")
})


test_that("refresh_models hits live endpoint for openai", {
  live_called <- FALSE
  live_mock <- function(provider, base_url, refresh = FALSE, openai_api_key = "", ...) {
    live_called <<- TRUE
    list(df = data.frame(id = c("gpt-4o", "gpt-4.1-mini"), created = c(1, 2)), status = "ok")
  }
  testthat::local_mocked_bindings(
    .fetch_models_live = live_mock,
    .cache_get = function(...) stop("cache_get called"),
    .cache_put = function(...) stop("cache_put called"),
    .cache_del = function(...) stop("cache_del called"),
    .env = asNamespace("gptr")
  )
  out <- refresh_models(provider = "openai", openai_api_key = "sk-test")
  expect_true(live_called)
  expect_equal(nrow(out), 2L)
  expect_true(all(out$source == "live"))
})

test_that("refresh_models bypasses cache when unreachable", {
  live_called <- FALSE
  live_mock <- function(provider, base_url, refresh = FALSE, ...) {
    live_called <<- TRUE
    list(df = data.frame(id = character(), created = numeric()), status = "unreachable")
  }
  testthat::local_mocked_bindings(
    .fetch_models_live = live_mock,
    .cache_get = function(...) stop("cache_get called"),
    .cache_put = function(...) stop("cache_put called"),
    .cache_del = function(...) stop("cache_del called"),
    .env = asNamespace("gptr")
  )
  out <- refresh_models(provider = "lmstudio", base_url = "http://127.0.0.1:1234")
  expect_true(live_called)
  expect_identical(nrow(out), 1L)
  expect_identical(out$status, "unreachable")
  expect_true(all(is.na(out$model_id)))
})

test_that("list_models refresh=TRUE bypasses cache for locals", {
  live_called <- FALSE
  testthat::local_mocked_bindings(
    .fetch_models_live = function(provider, base_url, refresh = FALSE, ...) {
      live_called <<- TRUE
      list(df = data.frame(id = "m1", created = 1), status = "ok")
    },
    .fetch_models_cached = function(...) stop("cached called"),
    .cache_get = function(...) stop("cache_get called"),
    .cache_put = function(...) stop("cache_put called"),
    .cache_del = function(...) stop("cache_del called"),
    .env = asNamespace("gptr")
  )
  out <- list_models(provider = "lmstudio", refresh = TRUE)
  expect_true(live_called)
  expect_identical(out$source, "live")
})

test_that("list_models refresh=TRUE bypasses cache for openai", {
  live_called <- FALSE
  testthat::local_mocked_bindings(
    .fetch_models_live = function(provider, base_url, refresh = FALSE, openai_api_key = "", ...) {
      live_called <<- TRUE
      list(df = data.frame(id = "gpt-4o", created = 1), status = "ok")
    },
    .fetch_models_cached = function(...) stop("cached called"),
    .cache_get = function(...) stop("cache_get called"),
    .cache_put = function(...) stop("cache_put called"),
    .cache_del = function(...) stop("cache_del called"),
    .env = asNamespace("gptr")
  )
  out <- list_models(provider = "openai", refresh = TRUE, openai_api_key = "sk-test")
  expect_true(live_called)
  expect_identical(out$source, "live")
})

test_that(".fetch_models_cached skips cache when unreachable", {
  fake_cache <- make_fake_cache()
  live_mock <- function(provider, base_url) {
    list(df = data.frame(id = character(), created = numeric()), status = "unreachable")
  }
  f <- getFromNamespace(".fetch_models_cached", "gptr")
  testthat::local_mocked_bindings(
    .fetch_models_live = live_mock,
    .cache_get = function(p, u) fake_cache$get(p, u),
    .cache_put = function(p, u, m) fake_cache$put(p, u, m),
    .env = asNamespace("gptr")
  )
  out <- f("lmstudio", "http://127.0.0.1:1234")
  diag <- attr(out, "diagnostic")
  expect_identical(diag$status, "unreachable")
  expect_identical(nrow(out$df), 0L)
  expect_null(fake_cache$get("lmstudio", "http://127.0.0.1:1234"))
})

test_that(".fetch_models_cached retries after unreachable and caches", {
  fake_cache <- make_fake_cache()
  calls <- 0
  live_mock <- function(provider, base_url) {
    calls <<- calls + 1
    if (calls == 1) {
      list(df = data.frame(id = character(), created = numeric()), status = "unreachable")
    } else {
      list(df = data.frame(id = "m1", created = 1), status = "ok")
    }
  }
  f <- getFromNamespace(".fetch_models_cached", "gptr")
  testthat::local_mocked_bindings(
    .fetch_models_live = live_mock,
    .cache_get = function(p, u) fake_cache$get(p, u),
    .cache_put = function(p, u, m) fake_cache$put(p, u, m),
    .env = asNamespace("gptr")
  )
  out <- f("lmstudio", "http://127.0.0.1:1234")
  expect_identical(out$status, "ok")
  cached <- fake_cache$get("lmstudio", "http://127.0.0.1:1234")
  expect_identical(cached$models$id, "m1")
})



# list models
test_that("list-models", {
  local_cache_store()
  out <- list_models(provider = "openai", refresh = FALSE, openai_api_key = "sk-test")
  expect_true(all(out$provider == "openai"))
})

test_that("list_models - manual override options", {
  local_cache_store()
  withr::local_options(list(gptr.lmstudio_base_url = "http://127.0.0.1:9999"))
  out <- list_models(provider = "lmstudio", base_url = "http://10.0.0.2:1234")
  expect_true(all(out$base_url == "http://10.0.0.2:1234"))
})

test_that("list_models - stable ordering by provider/availability/created/id", {
  local_cache_store()
  out <- list_models(refresh = FALSE, openai_api_key = "sk-test")
  prov_order <- rle(out$provider)$values
  expect_identical(prov_order[1], "lmstudio") # adjust if others present
})

# list_models() helper
test_that(".as_models_df - handles df/character/named vectors/lists", {
  f <- getFromNamespace(".as_models_df", "gptr")
  # data.frame
  df <- data.frame(id = c("a", "b"), created = c(1, 2))
  expect_equal(f(df)$id, c("a", "b"))
  expect_equal(f(df)$created, c(1, 2))

  # character vector
  expect_equal(f(c("x", "y"))$created, c(NA_real_, NA_real_))

  # named atomic vector (names = id, values = created)
  named <- c("gpt-4o" = 1683758102, "gpt-4.1-mini" = 1686558896)
  out <- f(named)
  expect_equal(out$id, c("gpt-4o", "gpt-4.1-mini"))
  expect_equal(out$created, as.numeric(named))

  # list of records with id/created
  lst <- list(list(id = "m1", created = 11), list(id = "m2", created = 22))
  out2 <- .as_models_df(lst)
  expect_equal(out2$id, c("m1", "m2"))
  expect_equal(out2$created, c(11, 22))
})

# list_models() helper
test_that(".row_df repeats provider/base/url and status", {
  f <- getFromNamespace(".row_df", "gptr")
  models <- .as_models_df(data.frame(id = c("a", "b"), created = c(10, 20)))
  r <- f("openai", "https://api.openai.com", models, "catalog", "live", fixed_ts, status = "ok")
  expect_equal(unique(r$provider), "openai")
  expect_equal(nrow(r), 2)
  expect_equal(r$status, rep("ok", 2))
  expect_equal(attr(r, "diagnostic")$status, "ok")
})

test_that(".row_df returns zero rows when no models and status is NA", {
  f <- getFromNamespace(".row_df", "gptr")
  r <- f("openai", "https://api.openai.com", data.frame(id = character(), created = numeric()),
         "catalog", "live", fixed_ts)
  expect_equal(nrow(r), 0)
})

test_that(".row_df preserves status when no models", {
  f <- getFromNamespace(".row_df", "gptr")
  r <- f("openai", "https://api.openai.com", data.frame(id = character(), created = numeric()),
         "catalog", "live", fixed_ts, status = "auth_missing")
  expect_equal(nrow(r), 0)
  diag <- attr(r, "diagnostic")
  expect_equal(diag$status, "auth_missing")
  expect_equal(diag$provider, "openai")
})


test_that(".fetch_models_cached enumerates cache contents", {
  f <- getFromNamespace(".fetch_models_cached", "gptr")
  cache <- getFromNamespace(".gptr_cache", "gptr")
  key_fun <- getFromNamespace(".cache_key", "gptr")
  cache$reset()
  cache$set(key_fun("openai", "https://api.openai.com"),
            list(provider = "openai", base_url = "https://api.openai.com",
                 models = data.frame(id = "gpt-4o", created = 1), ts = 1))
  out <- f()
  expect_true("openai" %in% out$provider)
  expect_equal(out$n_models[out$provider == "openai"], 1L)
})

test_that(".fetch_models_cached defaults base_url per provider", {
  f <- getFromNamespace(".fetch_models_cached", "gptr")
  testthat::local_mocked_bindings(
    .fetch_models_live = function(provider, base_url, ...) {
      list(df = data.frame(id = "m1", created = 1), status = "ok")
    },
    .cache_get = function(...) NULL,
    .cache_put = function(...) NULL,
    .env = asNamespace("gptr")
  )
  withr::local_envvar(OPENAI_API_KEY = "sk-test")

  out1 <- f("lmstudio")
  expect_equal(unique(out1$base_url), "http://127.0.0.1:1234")

  out2 <- f("openai")
  expect_equal(unique(out2$base_url), "https://api.openai.com")
})

test_that("invalidate clears cache", {
  inv <- getFromNamespace("delete_models_cache", "gptr")
  cache <- getFromNamespace(".gptr_cache", "gptr")
  key_fun <- getFromNamespace(".cache_key", "gptr")
  cache$reset()
  base <- "https://api.openai.com"
  cache$set(key_fun("openai", base),
            list(provider = "openai", base_url = base,
                 models = data.frame(id = "x", created = 1), ts = 1))
  expect_false(is.null(cache$get(key_fun("openai", base), missing = NULL)))
  inv()
  expect_null(cache$get(key_fun("openai", base), missing = NULL))
})

test_that("delete_models_cache removes by provider", {
  inv <- getFromNamespace("delete_models_cache", "gptr")
  cache <- getFromNamespace(".gptr_cache", "gptr")
  key_fun <- getFromNamespace(".cache_key", "gptr")
  cache$reset()
  cache$set(key_fun("openai", "https://api.openai.com"),
            list(provider = "openai", base_url = "https://api.openai.com", models = list(), ts = 1))
  cache$set(key_fun("lmstudio", "http://127.0.0.1:1234"),
            list(provider = "lmstudio", base_url = "http://127.0.0.1:1234", models = list(), ts = 1))
  calls <- list()
  testthat::local_mocked_bindings(
    .cache_del = function(p, u) {
      calls <<- append(calls, list(c(p, u)))
      cache$remove(key_fun(p, u))
      invisible(TRUE)
    },
    .env = asNamespace("gptr")
  )
  inv(provider = "openai")
  expect_null(cache$get(key_fun("openai", "https://api.openai.com"), missing = NULL))
  expect_false(is.null(cache$get(key_fun("lmstudio", "http://127.0.0.1:1234"), missing = NULL)))
  expect_identical(calls, list(c("openai", "https://api.openai.com")))
})

test_that("delete_models_cache removes by base_url", {
  inv <- getFromNamespace("delete_models_cache", "gptr")
  cache <- getFromNamespace(".gptr_cache", "gptr")
  key_fun <- getFromNamespace(".cache_key", "gptr")
  cache$reset()
  cache$set(key_fun("openai", "https://api.openai.com"),
            list(provider = "openai", base_url = "https://api.openai.com", models = list(), ts = 1))
  cache$set(key_fun("openai", "https://alt.openai.com"),
            list(provider = "openai", base_url = "https://alt.openai.com", models = list(), ts = 1))
  calls <- list()
  testthat::local_mocked_bindings(
    .cache_del = function(p, u) {
      calls <<- append(calls, list(c(p, u)))
      cache$remove(key_fun(p, u))
      invisible(TRUE)
    },
    .env = asNamespace("gptr")
  )
  inv(base_url = "https://api.openai.com")
  expect_null(cache$get(key_fun("openai", "https://api.openai.com"), missing = NULL))
  expect_false(is.null(cache$get(key_fun("openai", "https://alt.openai.com"), missing = NULL)))
  expect_identical(calls, list(c("openai", "https://api.openai.com")))
})

test_that("delete_models_cache removes by provider and base_url", {
  inv <- getFromNamespace("delete_models_cache", "gptr")
  cache <- getFromNamespace(".gptr_cache", "gptr")
  key_fun <- getFromNamespace(".cache_key", "gptr")
  cache$reset()
  cache$set(key_fun("openai", "https://api.openai.com"),
            list(provider = "openai", base_url = "https://api.openai.com", models = list(), ts = 1))
  cache$set(key_fun("openai", "https://alt.openai.com"),
            list(provider = "openai", base_url = "https://alt.openai.com", models = list(), ts = 1))
  calls <- list()
  testthat::local_mocked_bindings(
    .cache_del = function(p, u) {
      calls <<- append(calls, list(c(p, u)))
      cache$remove(key_fun(p, u))
      invisible(TRUE)
    },
    .env = asNamespace("gptr")
  )
  inv(provider = "openai", base_url = "https://api.openai.com")
  expect_null(cache$get(key_fun("openai", "https://api.openai.com"), missing = NULL))
  expect_false(is.null(cache$get(key_fun("openai", "https://alt.openai.com"), missing = NULL)))
  expect_identical(calls, list(c("openai", "https://api.openai.com")))
})



# Idempotence
# Two consecutive list_models(refresh = FALSE) calls with the same cache
# state produce identical data frames (ignoring .cached_at if that varies).
# source and status are operational metadata (live vs cache) and aren’t guaranteed to be identical across two calls—even when the content is identical. Your diff shows exactly that:
# out1: OpenAI rows came from live (status = "ok").
# out2: you expected cache (status = "ok_cache").
# Two good ways to make this test deterministic:
#
# Option A — Compare only content columns (ignore operational metadata)
test_that("list_models - idempotence", {
  local_cache_store()
  payload <- list(data = list(
    list(id = "gpt-4o", created = 1683758102),
    list(id = "gpt-4.1-mini", created = 1686558896)
  ))
  mock_http_openai(status = 200L, json = payload)

  out1 <- list_models(refresh = FALSE, openai_api_key = "sk")
  out2 <- list_models(refresh = FALSE, openai_api_key = "sk")

  ignore <- c("cached_at", "source", "status")
  expect_equal(
    out1[setdiff(names(out1), ignore)],
    out2[setdiff(names(out2), ignore)]
  )
})

# Option B : Assert that the second call uses cache (stronger)
# Warm the cache once, then make live fail; the second call must fall back to cache
test_that("list_models - second call uses cache when available", {
  local_cache_store()
  payload <- list(data = list(
    list(id = "gpt-4o", created = 1683758102),
    list(id = "gpt-4.1-mini", created = 1686558896)
  ))
  mock_http_openai(status = 200L, json = payload)
  invisible(list_models(refresh = FALSE, openai_api_key = "sk")) # warm cache

  # Now force live to fail; list_models should still succeed from cache
  mock_http_openai(perform_throws = TRUE)
  out2 <- list_models(refresh = FALSE, openai_api_key = "sk")

  oi <- subset(out2, provider == "openai")
  expect_true(nrow(oi) >= 2)
  expect_true(all(oi$source == "cache"))
  expect_true(all(oi$status == "ok_cache"))
})





test_that(".fetch_models_cached helper is present", {
  f <- getFromNamespace(".fetch_models_cached", "gptr")
  expect_type(f, "closure")
})

