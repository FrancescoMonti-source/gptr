# .flatten_model_ids()
test_that(".flatten_model_ids handles response shapes", {
  f <- getFromNamespace(".flatten_model_ids", "gptr")

  obj1 <- list(data = list(
    list(id = "a"),
    list(id = "b"),
    list(id = "a"),
    list(id = "")
  ))
  expect_identical(f(obj1), c("a", "b"))

  obj2 <- list(models = list(
    list(id = "c"),
    list(id = "c"),
    list(id = "d")
  ))
  expect_identical(f(obj2), c("c", "d"))

  obj3 <- list(
    list(id = "e"),
    list(id = ""),
    list(id = "f")
  )
  expect_identical(f(obj3), c("e", "f"))

  obj4 <- c("g", "", "h", "g")
  expect_identical(f(obj4), c("g", "h"))

  expect_identical(f(42), character(0))
})

# api_root()
# normalization trims trailing /v1/chat/completions
# keeps scheme/host
# collapses duplicate slashes
test_that(".api_root - normalizes urls", {
  f <- getFromNamespace(".api_root", "gptr")
  expect_equal(f("http://127.0.0.1:1234/v1/chat/completions"), "http://127.0.0.1:1234")
  expect_equal(f("http://127.0.0.1:1234////v1/chat/completions"), "http://127.0.0.1:1234")
  expect_equal(f("http://127.0.0.1:1234/v1"), "http://127.0.0.1:1234")
  expect_equal(f("https://api.openai.com/v1/models"), "https://api.openai.com")
})


# .cache_key()
# same inputs --> same key
# different base_url or provider --> different key
test_that(".cache_key", {
  ck <- getFromNamespace(".cache_key", "gptr")
  a1 <- ck("openai", "https://api.openai.com")
  a2 <- ck("openai", "https://api.openai.com")
  b <- ck("lmstudio", "http://127.0.0.1:1234")
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
    .cache_get = function(p, u, ...) fake_cache$get(p, u),
    .cache_put = function(p, u, m, ...) fake_cache$put(p, u, m),
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

test_that(".fetch_models_live applies ssl_cert to request options", {
  cert <- withr::local_tempfile(fileext = ".crt")
  writeLines("dummy", cert)

  captured <- NULL

  testthat::local_mocked_bindings(
    req_options = function(req, ...) {
      captured <<- list(...)
      req
    },
    req_perform = function(req) {
      httr2::response(
        status = 200L,
        headers = list("content-type" = "application/json"),
        body = charToRaw(jsonlite::toJSON(
          list(data = list(list(id = "m", created = 1))),
          auto_unbox = TRUE
        ))
      )
    },
    .env = asNamespace("httr2")
  )

  res <- getFromNamespace(".fetch_models_live", "gptr")(
    provider = "openai",
    base_url = "https://api.openai.com",
    openai_api_key = "sk-test",
    ssl_cert = cert
  )

  expect_identical(captured, list(cainfo = cert))
  expect_equal(res$status, "ok")
  expect_identical(res$df$id, "m")
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
    .cache_get = function(...) stop("cache_get called"),
    .cache_put = function(...) stop("cache_put called"),
    .cache_del = function(...) stop("cache_del called"),
    .env = asNamespace("gptr")
  )
  out <- list_models(provider = "openai", refresh = TRUE, openai_api_key = "sk-test")
  expect_true(live_called)
  expect_identical(out$source, "live")
})

test_that("list_models forwards ssl_cert to cached probes", {
  cert <- withr::local_tempfile(fileext = ".crt")
  writeLines("dummy", cert)

  captured <- list()
  testthat::local_mocked_bindings(
    .fetch_models_cached = function(provider = NULL, base_url = NULL,
                                    openai_api_key = "", refresh = FALSE,
                                    ssl_cert = NULL, ...) {
      captured[[length(captured) + 1L]] <<- list(provider = provider, ssl_cert = ssl_cert)
      data.frame(
        provider = provider,
        base_url = base_url,
        model_id = "model",
        availability = if (provider == "openai") "catalog" else "installed",
        cached_when = Sys.time(),
        source = "live",
        status = "ok",
        stringsAsFactors = FALSE
      )
    },
    .env = asNamespace("gptr")
  )

  out <- list_models(refresh = FALSE, openai_api_key = "sk-test", ssl_cert = cert)

  expect_true(nrow(out) >= 1L)
  expect_true(all(vapply(captured, function(x) identical(x$ssl_cert, cert), logical(1))))
})

test_that(".fetch_models_cached refresh=TRUE bypasses cache", {
  live_called <- FALSE
  f <- getFromNamespace(".fetch_models_cached", "gptr")
  testthat::local_mocked_bindings(
    .fetch_models_live = function(provider, base_url, refresh = FALSE, ...) {
      live_called <<- TRUE
      list(df = data.frame(id = "m1", created = 1), status = "ok")
    },
    .cache_get = function(...) stop("cache_get called"),
    .cache_put = function(...) stop("cache_put called"),
    .env = asNamespace("gptr")
  )
  out <- f("lmstudio", "http://127.0.0.1:1234", refresh = TRUE)
  expect_true(live_called)
  expect_identical(out$source, "live")
})

test_that(".fetch_models_cached forwards ssl_cert to live probe", {
  cert <- withr::local_tempfile(fileext = ".crt")
  writeLines("dummy", cert)

  received <- NULL
  f <- getFromNamespace(".fetch_models_cached", "gptr")
  testthat::local_mocked_bindings(
    .fetch_models_live = function(provider, base_url, refresh = FALSE,
                                  openai_api_key = "", ssl_cert = NULL, ...) {
      received <<- ssl_cert
      list(df = data.frame(id = "m", created = 1), status = "ok")
    },
    .cache_get = function(...) NULL,
    .cache_put = function(...) invisible(TRUE),
    .env = asNamespace("gptr")
  )

  out <- f("openai", "https://api.openai.com", openai_api_key = "sk-test", ssl_cert = cert)
  expect_identical(received, cert)
  expect_true(nrow(out) >= 1L)
})

test_that(".fetch_models_cached skips cache when unreachable", {
  fake_cache <- make_fake_cache()
  live_mock <- function(provider, base_url) {
    list(df = data.frame(id = character(), created = numeric()), status = "unreachable")
  }
  f <- getFromNamespace(".fetch_models_cached", "gptr")
  testthat::local_mocked_bindings(
    .fetch_models_live = live_mock,
    .cache_get = function(p, u, ...) fake_cache$get(p, u),
    .cache_put = function(p, u, m, ...) fake_cache$put(p, u, m),
    .env = asNamespace("gptr")
  )
  out <- f("lmstudio", "http://127.0.0.1:1234")
  expect_identical(attr(out, "diagnostic")$status, "unreachable")
  expect_identical(nrow(out), 0L)
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
    .cache_get = function(p, u, ...) fake_cache$get(p, u),
    .cache_put = function(p, u, m, ...) fake_cache$put(p, u, m),
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
test_that(".normalize_models_df - handles df/character/named vectors/lists", {
  f <- getFromNamespace(".normalize_models_df", "gptr")
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
  out2 <- .normalize_models_df(lst)
  expect_equal(out2$id, c("m1", "m2"))
  expect_equal(out2$created, c(11, 22))
})

# list_models() helper
test_that(".row_df repeats provider/base/url and status", {
  f <- getFromNamespace(".row_df", "gptr")
  models <- .normalize_models_df(data.frame(id = c("a", "b"), created = c(10, 20)))
  r <- f("openai", "https://api.openai.com", models, "catalog", "live", fixed_ts, status = "ok")
  expect_equal(unique(r$provider), "openai")
  expect_equal(nrow(r), 2)
  expect_equal(r$status, rep("ok", 2))
  expect_equal(attr(r, "diagnostic")$status, "ok")
})

test_that(".assemble_models_df returns zero rows when no models and status is NA", {
  f <- getFromNamespace(".assemble_models_df", "gptr")
  r <- f("openai", "https://api.openai.com", data.frame(id = character(), created = numeric()),
         "catalog", "live", fixed_ts)
  expect_equal(nrow(r), 0)
})

test_that(".assemble_models_df preserves status when no models", {
  f <- getFromNamespace(".assemble_models_df", "gptr")
  r <- f("openai", "https://api.openai.com", data.frame(id = character(), created = numeric()),
         "catalog", "live", fixed_ts, status = "auth_missing")
  expect_equal(nrow(r), 0)
  diag <- attr(r, "diagnostic")
  expect_equal(diag$status, "auth_missing")
  expect_equal(diag$provider, "openai")
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
    .cache_del = function(p, u, ...) {
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
    .cache_del = function(p, u, ...) {
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
    .cache_del = function(p, u, ...) {
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





test_that(".fetch_models_cached handles local and openai", {
  f <- getFromNamespace(".fetch_models_cached", "gptr")

  fake_cache <- make_fake_cache()
  testthat::local_mocked_bindings(
    .fetch_models_live = function(provider, base_url, refresh = FALSE, openai_api_key = "", ...) {
      if (provider == "openai") {
        list(df = data.frame(id = "gpt-4o", created = 1), status = "ok")
      } else {
        list(df = data.frame(id = "m1", created = 1), status = "ok")
      }
    },
    .cache_get = function(p, u, ...) fake_cache$get(p, u),
    .cache_put = function(p, u, m, ...) fake_cache$put(p, u, m),
    .env = asNamespace("gptr")
  )

  out_local <- f("lmstudio", "http://127.0.0.1:1234")
  expect_equal(out_local$availability, "installed")

  out_openai <- f("openai", "https://api.openai.com", openai_api_key = "sk")
  expect_equal(out_openai$availability, "catalog")
})



# new tests for base_url normalization and provider resolution

test_that(".fetch_models_cached normalizes base_url once", {
  calls <- 0
  cache <- new.env(parent = emptyenv())
  live <- function(provider, base_url, ...) {
    list(df = data.frame(id = "m", created = 1), status = "ok")
  }
  f <- getFromNamespace(".fetch_models_cached", "gptr")
  testthat::local_mocked_bindings(
    .fetch_models_live = live,
    .cache_get = function(p, u, ...) {
      key <- paste(p, u)
      if (exists(key, envir = cache, inherits = FALSE)) cache[[key]] else NULL
    },
    .cache_put = function(p, u, m, ...) {
      key <- paste(p, u)
      cache[[key]] <- list(ts = fixed_ts, models = m)
      invisible(TRUE)
    },
    .api_root = function(x) { calls <<- calls + 1; gptr:::.api_root(x) },
    .env = asNamespace("gptr")
  )
  out <- f("lmstudio", "http://127.0.0.1:1234/v1/models/")
  expect_equal(out$base_url, "http://127.0.0.1:1234")
  expect_identical(calls, 1L)
})

test_that(".get_cached_model_ids respects base_url_normalized", {
  stub_fetch <- function(provider, base_url, ...) {
    list(df = data.frame(id = "m", created = 1), status = "ok")
  }
  testthat::local_mocked_bindings(
    .fetch_models_live = stub_fetch,
    .api_root = function(x) stop("should not be called"),
    .env = asNamespace("gptr")
  )
  out <- getFromNamespace(".get_cached_model_ids", "gptr")(
    "lmstudio", "http://127.0.0.1:1234", base_url_normalized = TRUE
  )
  expect_identical(out, "m")
})

test_that(".get_cached_model_ids forwards ssl_cert to live fetch", {
  cert <- withr::local_tempfile(fileext = ".crt")
  writeLines("dummy", cert)

  received <- NULL
  testthat::local_mocked_bindings(
    .fetch_models_live = function(provider, base_url, refresh = FALSE,
                                  openai_api_key = "", ssl_cert = NULL, ...) {
      received <<- ssl_cert
      list(df = data.frame(id = "m", created = 1), status = "ok")
    },
    .cache_get = function(...) NULL,
    .cache_put = function(...) invisible(TRUE),
    .env = asNamespace("gptr")
  )

  out <- getFromNamespace(".get_cached_model_ids", "gptr")(
    "openai",
    "https://api.openai.com",
    openai_api_key = "sk-test",
    ssl_cert = cert
  )

  expect_identical(received, cert)
  expect_identical(out, "m")
})

test_that(".resolve_model_provider normalizes each URL once", {
  count <- 0
  stub_fetch <- function(provider, base_url, ...) {
    list(df = data.frame(id = "m", created = 1), status = "ok")
  }
  testthat::local_envvar(OPENAI_API_KEY = "sk-test")
  withr::local_options(
    gptr.lmstudio_base_url = "http://127.0.0.1:1234/v1/",
    gptr.ollama_base_url   = "http://127.0.0.1:11434/",
    gptr.localai_base_url  = "http://127.0.0.1:8080/v1/"
  )
  testthat::local_mocked_bindings(
    .fetch_models_live = stub_fetch,
    .api_root = function(x) { count <<- count + 1; gptr:::.api_root(x) },
    .env = asNamespace("gptr")
  )
  out <- getFromNamespace(".resolve_model_provider", "gptr")("m", openai_api_key = "sk-test")
  expect_setequal(out$provider, c("lmstudio", "ollama", "localai", "openai"))
  expect_true(all(out$base_url %in% c(
    "http://127.0.0.1:1234",
    "http://127.0.0.1:11434",
    "http://127.0.0.1:8080",
    "https://api.openai.com"
  )))
  expect_identical(count, 4L)
})

test_that(".resolve_model_provider forwards ssl_cert to cache helper", {
  cert <- withr::local_tempfile(fileext = ".crt")
  writeLines("dummy", cert)

  seen <- list()
  testthat::local_mocked_bindings(
    .get_cached_model_ids = function(provider, base_url, openai_api_key = "",
                                     base_url_normalized = FALSE, ssl_cert = NULL) {
      seen[[provider]] <<- ssl_cert
      character()
    },
    .env = asNamespace("gptr")
  )

  out <- getFromNamespace(".resolve_model_provider", "gptr")(
    "demo-model",
    openai_api_key = "sk-test",
    ssl_cert = cert
  )

  expect_true(length(seen) >= 1L)
  expect_true(all(vapply(seen, identical, logical(1), cert)))
  expect_identical(nrow(out), 0L)
})

