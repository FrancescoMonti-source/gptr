local_clean_models_cache <- function(env = parent.frame()) {
  cache <- getFromNamespace(".gptr_cache", "gptr")
  cache$reset()
  withr::defer(cache$reset(), env = env)
  invisible(cache)
}

seed_models_cache <- function(provider, base_url, ids = "model", ts = 1, env = parent.frame()) {
  cache <- local_clean_models_cache(env = env)
  key_fun <- getFromNamespace(".cache_key", "gptr")
  root <- gptr:::.api_root(base_url)
  models <- data.frame(
    id = as.character(ids),
    created = seq_along(ids),
    stringsAsFactors = FALSE
  )
  cache$set(
    key_fun(provider, root),
    list(provider = provider, base_url = root, models = models, ts = ts)
  )
  invisible(root)
}

make_models_df <- function(provider,
                           base_url,
                           ids = "model",
                           availability = if (identical(provider, "openai")) "catalog" else "installed",
                           source = "cache",
                           status = "ok_cache",
                           ts = 1) {
  root <- gptr:::.api_root(base_url)
  n <- length(ids)
  df <- data.frame(
    provider = rep(provider, n),
    base_url = rep(root, n),
    model_id = as.character(ids),
    created = seq_len(n),
    availability = rep(availability, n),
    cached_at = as.POSIXct(rep(ts, n), origin = "1970-01-01", tz = "UTC"),
    source = rep(source, n),
    status = rep(status, n),
    stringsAsFactors = FALSE
  )
  if (n == 0L) {
    df <- data.frame(
      provider = character(),
      base_url = character(),
      model_id = character(),
      created = numeric(),
      availability = character(),
      cached_at = as.POSIXct(numeric(), origin = "1970-01-01", tz = "UTC"),
      source = character(),
      status = character(),
      stringsAsFactors = FALSE
    )
  }
  attr(df, "diagnostic") <- list(provider = provider, base_url = root, status = status)
  df
}

test_that(".api_root normalizes urls", {
  f <- getFromNamespace(".api_root", "gptr")

  expect_identical(f("http://127.0.0.1:1234/v1/chat/completions"), "http://127.0.0.1:1234")
  expect_identical(f("http://127.0.0.1:1234////v1/chat/completions"), "http://127.0.0.1:1234")
  expect_identical(f("http://127.0.0.1:1234/v1"), "http://127.0.0.1:1234")
  expect_identical(f("https://api.openai.com/v1/models"), "https://api.openai.com")
})

test_that(".flatten_model_ids handles response shapes", {
  f <- getFromNamespace(".flatten_model_ids", "gptr")

  expect_identical(f(list(data = list(list(id = "a"), list(id = "b"), list(id = "a")))), c("a", "b"))
  expect_identical(f(list(models = list(list(id = "c"), list(id = "c"), list(id = "d")))), c("c", "d"))
  expect_identical(f(list(list(id = "e"), list(id = ""), list(id = "f"))), c("e", "f"))
  expect_identical(f(c("g", "", "h", "g")), c("g", "h"))
  expect_identical(f(42), character(0))
})

test_that(".fetch_models_live applies ssl_cert to request options", {
  cert <- withr::local_tempfile(fileext = ".crt")
  writeLines("dummy", cert)

  captured <- NULL
  out <- testthat::with_mocked_bindings(
    getFromNamespace(".fetch_models_live", "gptr")(
      provider = "openai",
      base_url = "https://api.openai.com",
      openai_api_key = "sk-test",
      ssl_cert = cert
    ),
    .req_apply_ssl_cert = function(req, ssl_cert = NULL) {
      captured <<- ssl_cert
      req
    },
    .httr_req_perform = function(req) {
      httr2::response(
        status = 200L,
        headers = list("content-type" = "application/json"),
        body = charToRaw(jsonlite::toJSON(
          list(data = list(list(id = "m", created = 1))),
          auto_unbox = TRUE
        ))
      )
    },
    .package = "gptr"
  )

  expect_identical(captured, cert)
  expect_identical(out$status, "ok")
  expect_identical(out$df$id, "m")
})

test_that("list_models returns normalized rows and diagnostics", {
  testthat::local_mocked_bindings(
    .fetch_models_cached = function(provider = NULL, base_url = NULL, refresh = FALSE, openai_api_key = "", ssl_cert = NULL, ...) {
      make_models_df(
        provider = provider,
        base_url = base_url,
        ids = paste0(provider, "-model"),
        availability = if (identical(provider, "openai")) "catalog" else "installed",
        source = "cache",
        status = "ok_cache"
      )
    },
    .package = "gptr"
  )

  out <- list_models(openai_api_key = "sk-test")

  expect_true(all(c("provider", "base_url", "model_id", "availability", "cached_at", "source", "status") %in% names(out)))
  expect_setequal(out$provider, c("lmstudio", "ollama", "localai", "openai"))
  expect_identical(length(attr(out, "diagnostics")), 4L)
  expect_true(all(vapply(attr(out, "diagnostics"), function(x) identical(x$status, "ok_cache"), logical(1))))
})

test_that("list_models honors explicit base_url", {
  captured <- NULL
  testthat::local_mocked_bindings(
    .fetch_models_cached = function(provider = NULL, base_url = NULL, refresh = FALSE, openai_api_key = "", ssl_cert = NULL, ...) {
      captured <<- base_url
      make_models_df(provider = provider, base_url = base_url, ids = "custom-model")
    },
    .package = "gptr"
  )

  out <- list_models(provider = "lmstudio", base_url = "http://10.0.0.2:1234/v1/models")

  expect_identical(captured, "http://10.0.0.2:1234")
  expect_identical(out$base_url, "http://10.0.0.2:1234")
})

test_that("list_models forwards ssl_cert to cached probes", {
  cert <- withr::local_tempfile(fileext = ".crt")
  writeLines("dummy", cert)

  seen <- NULL
  testthat::local_mocked_bindings(
    .fetch_models_cached = function(provider = NULL, base_url = NULL, refresh = FALSE, openai_api_key = "", ssl_cert = NULL, ...) {
      seen <<- ssl_cert
      make_models_df(provider = provider, base_url = base_url, ids = "model")
    },
    .package = "gptr"
  )

  out <- list_models(provider = "openai", openai_api_key = "sk-test", ssl_cert = cert)

  expect_identical(seen, cert)
  expect_identical(out$model_id, "model")
})

test_that("refresh_models uses live probes and leaves cache untouched", {
  root <- seed_models_cache("lmstudio", "http://127.0.0.1:1234", ids = "cached-model", env = environment())
  cache <- getFromNamespace(".gptr_cache", "gptr")
  key_fun <- getFromNamespace(".cache_key", "gptr")

  testthat::local_mocked_bindings(
    .fetch_models_live = function(provider, base_url, refresh = FALSE, openai_api_key = "", ssl_cert = NULL, ...) {
      expect_true(isTRUE(refresh))
      list(df = data.frame(id = "live-model", created = 99, stringsAsFactors = FALSE), status = "ok")
    },
    .package = "gptr"
  )

  out <- refresh_models(provider = "lmstudio")
  cached <- cache$get(key_fun("lmstudio", root), missing = NULL)

  expect_identical(out$model_id, "live-model")
  expect_identical(out$source, "live")
  expect_identical(cached$models$id, "cached-model")
})

test_that("delete_models_cache clears all cached entries", {
  cache <- local_clean_models_cache(env = environment())
  key_fun <- getFromNamespace(".cache_key", "gptr")
  cache$set(key_fun("openai", "https://api.openai.com"), list(provider = "openai", base_url = "https://api.openai.com", models = data.frame(id = "gpt", created = 1), ts = 1))
  cache$set(key_fun("lmstudio", "http://127.0.0.1:1234"), list(provider = "lmstudio", base_url = "http://127.0.0.1:1234", models = data.frame(id = "m", created = 1), ts = 1))

  delete_models_cache()

  expect_identical(cache$keys(), character())
})

test_that("delete_models_cache removes entries by provider", {
  cache <- local_clean_models_cache(env = environment())
  key_fun <- getFromNamespace(".cache_key", "gptr")
  cache$set(key_fun("openai", "https://api.openai.com"), list(provider = "openai", base_url = "https://api.openai.com", models = data.frame(id = "gpt", created = 1), ts = 1))
  cache$set(key_fun("lmstudio", "http://127.0.0.1:1234"), list(provider = "lmstudio", base_url = "http://127.0.0.1:1234", models = data.frame(id = "m", created = 1), ts = 1))

  delete_models_cache(provider = "openai")

  expect_null(cache$get(key_fun("openai", "https://api.openai.com"), missing = NULL))
  expect_false(is.null(cache$get(key_fun("lmstudio", "http://127.0.0.1:1234"), missing = NULL)))
})

test_that("delete_models_cache removes entries by provider and base_url", {
  cache <- local_clean_models_cache(env = environment())
  key_fun <- getFromNamespace(".cache_key", "gptr")
  cache$set(key_fun("openai", "https://api.openai.com"), list(provider = "openai", base_url = "https://api.openai.com", models = data.frame(id = "gpt", created = 1), ts = 1))
  cache$set(key_fun("openai", "https://alt.openai.com"), list(provider = "openai", base_url = "https://alt.openai.com", models = data.frame(id = "gpt-alt", created = 2), ts = 1))

  delete_models_cache(provider = "openai", base_url = "https://api.openai.com/v1/models")

  expect_null(cache$get(key_fun("openai", "https://api.openai.com"), missing = NULL))
  expect_false(is.null(cache$get(key_fun("openai", "https://alt.openai.com"), missing = NULL)))
})

test_that("list_models refresh=TRUE reports unreachable diagnostics deterministically", {
  local_clean_models_cache(env = environment())
  testthat::local_mocked_bindings(
    .fetch_models_live = function(provider, base_url, refresh = FALSE, openai_api_key = "", ssl_cert = NULL, ...) {
      list(df = data.frame(id = character(), created = numeric(), stringsAsFactors = FALSE), status = "unreachable")
    },
    .package = "gptr"
  )

  out <- list_models(provider = "lmstudio", refresh = TRUE)

  expect_identical(nrow(out), 0L)
  expect_identical(attr(out, "diagnostics")[[1]]$status, "unreachable")
})

test_that("list_models refresh=TRUE reports empty results without inventing legacy statuses", {
  local_clean_models_cache(env = environment())
  testthat::local_mocked_bindings(
    .fetch_models_live = function(provider, base_url, refresh = FALSE, openai_api_key = "", ssl_cert = NULL, ...) {
      list(df = data.frame(id = character(), created = numeric(), stringsAsFactors = FALSE), status = "ok")
    },
    .package = "gptr"
  )

  out <- list_models(provider = "openai", refresh = TRUE, openai_api_key = "sk-test")

  expect_identical(nrow(out), 0L)
  expect_identical(attr(out, "diagnostics")[[1]]$status, "ok")
})
