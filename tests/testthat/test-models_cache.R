# shared store (keep yours)
.gptr_test_cache_store <- new.env(parent = emptyenv())

# normalize a base url to the cache "root"
.cache_root_for_test <- function(x) sub("/v1/.*$", "", x)

# Monotonic timestamp counter so snapshots differ predictably
.get_next_ts <- local({
  seq_name <- ".gptr_test_cache_seq"
  function() {
    cur <- get0(seq_name, envir = .gptr_test_cache_store, inherits = FALSE)
    if (is.null(cur)) cur <- 0L
    cur <- cur + 1L
    assign(seq_name, cur, envir = .gptr_test_cache_store)
    1755806518 + cur
  }
})


test_that("no direct httr2 calls outside wrappers", {
  # Files to scan
  files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
  files <- setdiff(files, file.path("R", "http_wrappers.R")) # allow wrappers

  offenders <- character()

  for (f in files) {
    lines <- readLines(f, warn = FALSE)
    lines <- lines[!grepl("^#'", lines)] # ignore roxygen comments

    hits_idx <- grep("\\bhttr2::", lines)
    if (length(hits_idx)) {
      # include file + line number + the offending line
      offenders <- c(
        offenders,
        paste0(basename(f), ":", hits_idx, ": ", trimws(lines[hits_idx]))
      )
    }
  }

  msg <- if (length(offenders)) {
    paste0(
      "Found direct httr2 calls (use .http_* wrappers):\n",
      paste(offenders, collapse = "\n")
    )
  } else {
    ""
  }

  expect(length(offenders) == 0L, msg) # edition-agnostic assertion
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




# Sanity: helper should be visible
testthat::with_reporter("silent", {
  testthat::test_that("helpers loaded", {
    testthat::expect_true(exists("mock_httr2_openai", inherits = TRUE))
    testthat::expect_true(is.environment(.gptr_test_cache_store))
  })
})




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
  store <- new.env(parent = emptyenv())
  list(
    get = function(provider, base_url) {
      key <- paste(provider, base_url, sep = "@")
      if (!exists(key, envir = store, inherits = FALSE)) {
        return(NULL)
      }
      get(key, envir = store, inherits = FALSE)
    },
    put = function(provider, base_url, models) {
      key <- paste(provider, base_url, sep = "@")
      assign(key, list(models = models, ts = fixed_ts), envir = store)
      invisible(TRUE)
    },
    env = store
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

# models cache snapshot immutability
test_that("cache snapshot - immutability", {
  snap <- getFromNamespace(".models_cache_snapshot", "gptr")
  put <- getFromNamespace(".cache_put", "gptr")
  get <- getFromNamespace(".cache_get", "gptr")

  put("openai", "https://api.openai.com", data.frame(id = "a", created = 1))
  s1 <- snap()
  put("openai", "https://api.openai.com", data.frame(id = "b", created = 2))
  s2 <- snap()
  expect_false(identical(s1, s2))
})

test_that("refresh writes cache and surfaces status", {
  fake_cache <- make_fake_cache()
  local_mocked_bindings(
    .cache_get = function(p, u) fake_cache$get(p, u),
    .cache_put = function(p, u, m) fake_cache$put(p, u, m),
    refresh_models_cache = function(provider, base_url) list(status = "refreshed"),
    .list_models_cached = function(...) character(0),
    .api_root = function(x) x
  )
  out <- list_models(provider = "lmstudio", refresh = TRUE)
  expect_true(any(out$status == "refreshed"))
})


test_that("openai non-JSON -> non_json", {
  live <- getFromNamespace(".list_openai_live", "gptr")
  mock_http_openai(status = 200L, json_throws = TRUE)
  o <- live("sk-test")
  expect_equal(o$status, "non_json")
})

test_that("openai network error -> unreachable", {
  live <- getFromNamespace(".list_openai_live", "gptr")
  mock_http_openai(perform_throws = TRUE)
  o <- live("sk-test")
  expect_equal(o$status, "unreachable")
})

test_that("openai 5xx -> http_503", {
  live <- getFromNamespace(".list_openai_live", "gptr")
  mock_http_openai(status = 503L)
  o <- live("sk-test")
  expect_equal(o$status, "http_503")
})

test_that("openai ok -> df parsed and status ok", {
  live <- getFromNamespace(".list_openai_live", "gptr")
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

test_that("openai fallback semantics via .list_openai_live", {
  live <- getFromNamespace(".list_openai_live", "gptr")

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


test_that(".list_models_cached - drops live probing", {
  f <- getFromNamespace(".list_models_cached", "gptr")
  put <- getFromNamespace(".cache_put", "gptr")
  put("openai", "https://api.openai.com", c("gpt-4o" = 1683))
  out <- f()
  expect_true(any(out$provider == "openai"))
  expect_true(all(out$source == "cache"))
})

test_that("invalidate clears cache for provider/base_url", {
  inv <- getFromNamespace("delete_models_cache", "gptr")
  put <- getFromNamespace(".cache_put", "gptr")
  get <- getFromNamespace(".cache_get", "gptr")

  base <- "https://api.openai.com" # normalized root!
  put("openai", base, data.frame(id = "x", created = 1))

  expect_false(is.null(get("openai", base)))
  inv(provider = "openai", base_url = base)
  expect_null(get("openai", base))
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
  expect_true(all(oi$status %in% c("ok_cache", NA))) # NA if locals; ok_cache for OpenAI
})



test_that("invalidate by provider+base_url clears exactly one", {
  inv <- getFromNamespace("delete_models_cache", "gptr")
  put <- getFromNamespace(".cache_put", "gptr")
  get <- getFromNamespace(".cache_get", "gptr")

  base <- "https://api.openai.com"
  put("openai", base, data.frame(id = "x", created = 1))
  expect_false(is.null(get("openai", base)))
  inv(provider = "openai", base_url = base)
  expect_null(get("openai", base))
})
