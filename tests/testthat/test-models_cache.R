# shared store (keep yours)
.gptr_test_cache_store <- cachem::cache_mem()

# normalize a base url to the cache "root"
.cache_root_for_test <- function(x) sub("/v1/.*$", "", x)

# Monotonic timestamp counter so snapshots differ predictably
.get_next_ts <- local({
  seq <- 0L
  function() {
    seq <<- seq + 1L
    1755806518 + seq
  }
})

# 2) Airtight httr2 mock for OpenAI model listing
mock_http_openai <- function(status = 200L,
                             json = NULL,
                             json_throws = FALSE,
                             perform_throws = FALSE) {
    # mocks for your wrapper layer
    binds <- list(
        .http_request = function(url) list(.url = url),
        .http_req_headers = function(req, ...) req,
        .http_req_timeout = function(req, ...) req,
        .http_req_retry = function(req, ...) req,
        .http_req_perform = if (perform_throws) {
            function(req, ...) stop("network fail")
        } else {
            function(req, ...) structure(list(.url = req$.url), class = "httr2_response")
        },
        .http_resp_status = function(resp) status,
        .http_resp_body_json = if (json_throws) {
            function(...) stop("boom")
        } else {
            function(resp, simplifyVector = FALSE) json
        }
    )
    .patch_pkg(binds) # patch in gptr namespace only
}

# Patch only if the binding exists in the package
.patch_pkg <- function(bindings) {
  pkg_ns <- asNamespace("gptr")
  have <- intersect(names(bindings), ls(pkg_ns, all.names = TRUE))
  if (!length(have)) {
    return(invisible(FALSE))
  }
  do.call(
    testthat::local_mocked_bindings,
    c(bindings[have], list(.env = pkg_ns))
  )
  invisible(TRUE)
}

# Cache: tolerate any signature using ...
.patch_pkg(list(
  .cache_get = function(...) {
    a <- list(...)
    key_fun <- getFromNamespace('.cache_key', 'gptr')
    if (length(a) == 1L && is.character(a[[1L]])) {
      key <- a[[1L]]
    } else {
      provider <- as.character(a[[1L]])
      base_url <- .cache_root_for_test(as.character(a[[2L]]))
      key <- key_fun(provider, base_url)
    }
    .gptr_test_cache_store$get(key)
  },
  .cache_put = function(...) {
    a <- list(...)
    key_fun <- getFromNamespace('.cache_key', 'gptr')
    provider <- as.character(a[[1L]])
    base_url <- .cache_root_for_test(as.character(a[[2L]]))
    models <- a[[3L]]
    key <- key_fun(provider, base_url)
    .gptr_test_cache_store$set(key,
      list(provider = provider, base_url = base_url, models = models, ts = .get_next_ts()))
    invisible(TRUE)
  },
  .cache_del = function(...) {
    a <- list(...)
    key_fun <- getFromNamespace('.cache_key', 'gptr')
    provider <- as.character(a[[1L]])
    base_url <- .cache_root_for_test(as.character(a[[2L]]))
    key <- key_fun(provider, base_url)
    .gptr_test_cache_store$remove(key)
    invisible(TRUE)
  }
))

# Airtight mock for OpenAI model listing via httr2 wrappers
mock_http_openai <- function(status = 200L,
                             json = NULL,
                             json_throws = FALSE,
                             perform_throws = FALSE) {
  binds <- list(
    .http_request = function(url) list(.url = url),
    .http_req_headers = function(req, ...) req,
    .http_req_timeout = function(req, ...) req,
    .http_req_retry = function(req, ...) req,
    .http_req_perform = if (perform_throws) {
      function(req, ...) stop("network fail")
    } else {
      function(req, ...) structure(list(.url = req$.url), class = "httr2_response")
    },
    .http_resp_status = function(resp) status,
    .http_resp_body_json = if (json_throws) {
      function(...) stop("boom")
    } else {
      function(resp, simplifyVector = FALSE) json
    }
  )
  .patch_pkg(binds)
  invisible(TRUE)
}

# Deterministic clock
fixed_ts <- 1755806518
fix_time <- function(expr) {
  withr::local_envvar(c(TZ = "UTC"))
  withr::with_options(list(gpt.timeout = 2, gptr.request_timeout = 2), {
    withr::with_seed(1, {
      # Freeze Sys.time() via a small shim you use inside your code when stamping cache
      # If you currently call Sys.time() directly, we’ll just set created_at from fixed numbers in tests.
      force(expr)
    })
  })
}

# Minimal in-memory cache to mock your real cache layer
make_fake_cache <- function() {
  store <- cachem::cache_mem()
  key_fun <- getFromNamespace('.cache_key', 'gptr')
  list(
    get = function(provider, base_url) {
      key <- key_fun(provider, .cache_root_for_test(base_url))
      store$get(key, missing = NULL)
    },
    put = function(provider, base_url, models) {
      root <- .cache_root_for_test(base_url)
      key <- key_fun(provider, root)
      store$set(key, list(provider = provider, base_url = root, models = models, ts = fixed_ts))
      invisible(TRUE)
    },
    cache = store
  )
}

# Small helpers to build fake OpenAI/Ollama replies
openai_models_payload <- function() {
  list(
    data = list(
      list(id = "gpt-4o", created = 1683758102),
      list(id = "gpt-4.1-mini", created = 1686558896)
    )
  )
}

ollama_tags_payload <- function() {
  list(models = data.frame(
    name = c("mistral:instruct", "llama3.1"),
    stringsAsFactors = FALSE
  ))
}


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
  local_mocked_bindings(
    .cache_get = function(p, u) fake_cache$get(p, u),
    .cache_put = function(p, u, m) fake_cache$put(p, u, m),
    .cache_del = function(...) invisible(TRUE),
    .fetch_models_live = function(...) list(df = data.frame(id = character(), created = numeric()), status = "ok"),
    .api_root = function(x) x
  )
  out <- list_models(provider = "lmstudio", refresh = TRUE)
  expect_equal(nrow(out), 0)
})


test_that("openai non-JSON -> non_json", {
  f <- getFromNamespace(".fetch_models_live", "gptr")
  live <- function(key) f("openai", "https://api.openai.com", key)
  mock_http_openai(status = 200L, json_throws = TRUE)
  o <- live("sk-test")
  expect_equal(o$status, "non_json")
})

test_that("openai network error -> unreachable", {
  f <- getFromNamespace(".fetch_models_live", "gptr")
  live <- function(key) f("openai", "https://api.openai.com", key)
  mock_http_openai(perform_throws = TRUE)
  o <- live("sk-test")
  expect_equal(o$status, "unreachable")
})

test_that("openai 5xx -> http_503", {
  f <- getFromNamespace(".fetch_models_live", "gptr")
  live <- function(key) f("openai", "https://api.openai.com", key)
  mock_http_openai(status = 503L)
  o <- live("sk-test")
  expect_equal(o$status, "http_503")
})

test_that("openai ok -> df parsed and status ok", {
  f <- getFromNamespace(".fetch_models_live", "gptr")
  live <- function(key) f("openai", "https://api.openai.com", key)
  payload <- list(data = list(
    list(id = "gpt-4o", created = 1683758102),
    list(id = "gpt-4.1-mini", created = 1686558896)
  ))
  mock_http_openai(status = 200L, json = payload)
  o <- live("sk-test")
  expect_equal(o$status, "ok")
  expect_true(is.data.frame(o$df))
  expect_setequal(o$df$id, c("gpt-4o", "gpt-4.1-mini"))
})

test_that("openai empty model list -> empty_cache", {
  payload <- list(data = list())
  mock_http_openai(status = 200L, json = payload)
  out <- list_models(provider = "openai", refresh = TRUE, openai_api_key = "sk-test")
  expect_identical(nrow(out), 0L)
})

test_that("openai fallback semantics via .fetch_models_live", {
  f <- getFromNamespace(".fetch_models_live", "gptr")
  live <- function(key) f("openai", "https://api.openai.com", key)

  mock_http_openai(status = 200L, json_throws = TRUE)
  o1 <- live("sk")
  expect_equal(o1$status, "non_json")

  mock_http_openai(perform_throws = TRUE)
  o2 <- live("sk")
  expect_equal(o2$status, "unreachable")

  mock_http_openai(status = 503L)
  o3 <- live("sk")
  expect_equal(o3$status, "http_503")
})


test_that("refresh_models handles openai provider", {
  fake_cache <- make_fake_cache()
  payload <- openai_models_payload()
  mock_http_openai(status = 200L, json = payload)
  testthat::local_mocked_bindings(
    .cache_get = function(p, u) fake_cache$get(p, u),
    .cache_put = function(p, u, m) fake_cache$put(p, u, m),
    .cache_del = function(...) invisible(TRUE),
    .env = asNamespace("gptr")
  )
  out <- refresh_models(provider = "openai", openai_api_key = "sk-test")
  expect_true(all(out$provider == "openai"))
  expect_equal(nrow(out), 2L)
  expect_true(all(out$status == "ok"))
  cached <- fake_cache$get("openai", "https://api.openai.com")
  expect_true(NROW(cached$models) == 2)
})

test_that("refresh_models skips cache when unreachable", {
  fake_cache <- make_fake_cache()
  live_mock <- function(provider, base_url) {
    list(df = data.frame(id = character(), created = numeric()), status = "unreachable")
  }
  testthat::local_mocked_bindings(
    .fetch_models_live = live_mock,
    .cache_put = function(p, u, m) fake_cache$put(p, u, m),
    .cache_get = function(p, u) fake_cache$get(p, u),
    .cache_del = function(...) invisible(TRUE),
    .env = asNamespace("gptr")
  )
  out <- refresh_models(provider = "lmstudio", base_url = "http://127.0.0.1:1234")
  expect_identical(nrow(out), 1L)
  expect_identical(out$status, "unreachable")
  expect_true(all(is.na(out$model_id)))
  expect_null(fake_cache$get("lmstudio", "http://127.0.0.1:1234"))
})

test_that("refresh_models retries after unreachable and caches", {
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
  testthat::local_mocked_bindings(
    .fetch_models_live = live_mock,
    .cache_put = function(p, u, m) fake_cache$put(p, u, m),
    .cache_get = function(p, u) fake_cache$get(p, u),
    .cache_del = function(...) invisible(TRUE),
    .env = asNamespace("gptr")
  )
  out <- refresh_models(provider = "lmstudio", base_url = "http://127.0.0.1:1234")
  expect_identical(nrow(out), 1L)
  expect_identical(out$status, "ok")
  expect_identical(out$model_id, "m1")
  cached <- fake_cache$get("lmstudio", "http://127.0.0.1:1234")
  expect_identical(cached$models$id, "m1")
})

test_that(".list_models_cached skips cache when unreachable", {
  fake_cache <- make_fake_cache()
  live_mock <- function(provider, base_url) {
    list(df = data.frame(id = character(), created = numeric()), status = "unreachable")
  }
  f <- getFromNamespace(".list_models_cached", "gptr")
  testthat::local_mocked_bindings(
    .fetch_models_live = live_mock,
    .cache_get = function(p, u) fake_cache$get(p, u),
    .cache_put = function(p, u, m) fake_cache$put(p, u, m),
    .env = asNamespace("gptr")
  )
  out <- f("lmstudio", "http://127.0.0.1:1234")
  expect_identical(out$status, "unreachable")
  expect_identical(nrow(out$df), 0L)
  expect_null(fake_cache$get("lmstudio", "http://127.0.0.1:1234"))
})

test_that(".list_models_cached retries after unreachable and caches", {
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
  f <- getFromNamespace(".list_models_cached", "gptr")
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
  out <- list_models(provider = "openai", refresh = FALSE, openai_api_key = "sk-test")
  expect_true(all(out$provider == "openai"))
})

test_that("list_models - manual override options", {
  withr::local_options(list(gptr.lmstudio_base_url = "http://127.0.0.1:9999"))
  out <- list_models(provider = "lmstudio", base_url = "http://10.0.0.2:1234")
  expect_true(all(out$base_url == "http://10.0.0.2:1234"))
})

test_that("list_models - stable ordering by provider/availability/created/id", {
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
  expect_equal(nrow(r), 1)
  expect_true(is.na(r$model_id))
  expect_equal(r$status, "auth_missing")
})


test_that(".list_models_cached enumerates cache contents", {
  f <- getFromNamespace(".list_models_cached", "gptr")
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
  inv(provider = "openai")
  expect_null(cache$get(key_fun("openai", "https://api.openai.com"), missing = NULL))
  expect_false(is.null(cache$get(key_fun("lmstudio", "http://127.0.0.1:1234"), missing = NULL)))
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
  inv(base_url = "https://api.openai.com")
  expect_null(cache$get(key_fun("openai", "https://api.openai.com"), missing = NULL))
  expect_false(is.null(cache$get(key_fun("openai", "https://alt.openai.com"), missing = NULL)))
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
  inv(provider = "openai", base_url = "https://api.openai.com")
  expect_null(cache$get(key_fun("openai", "https://api.openai.com"), missing = NULL))
  expect_false(is.null(cache$get(key_fun("openai", "https://alt.openai.com"), missing = NULL)))
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




