# Helpers for models_cache tests

cache_root_for_test <- function(x) sub("/v1/.*$", "", x)

.orig_test_bindings <- new.env(parent = emptyenv())

local_namespace_binding <- function(pkg, name, value, env = parent.frame()) {
  ns <- asNamespace(pkg)
  key <- paste(pkg, name, sep = "::")
  if (!exists(key, envir = .orig_test_bindings, inherits = FALSE)) {
    assign(key, get(name, envir = ns, inherits = FALSE), envir = .orig_test_bindings)
  }
  old <- get(key, envir = .orig_test_bindings, inherits = FALSE)

  unlockBinding(name, ns)
  assign(name, value, envir = ns)
  lockBinding(name, ns)

  withr::defer(
    {
      unlockBinding(name, ns)
      assign(name, old, envir = ns)
      lockBinding(name, ns)
    },
    envir = env
  )

  invisible(TRUE)
}

get_next_ts <- local({
  seq <- 0L
  function() {
    seq <<- seq + 1L
    1755806518 + seq
  }
})

json_resp <- function(status = 200L, body = list()) {
  httr2::response(
    status = status,
    body = charToRaw(jsonlite::toJSON(body, auto_unbox = TRUE)),
    headers = list("content-type" = "application/json")
  )
}

mock_http_openai <- function(status = 200L,
                             json = NULL,
                             json_throws = FALSE,
                             perform_throws = FALSE) {
  if (is.null(json)) json <- list()
  resp <- if (json_throws) {
    httr2::response(
      status = status,
      body = charToRaw("{"),
      headers = list("content-type" = "application/json")
    )
  } else {
    json_resp(status = status, body = json)
  }

  handler <- if (perform_throws) {
    function(req) stop("network fail")
  } else {
    function(req) resp
  }

  local_namespace_binding("gptr", ".httr_req_perform", handler, env = parent.frame())
}

local_cache_store <- function() {
  store <- cachem::cache_mem()
  cache_get_mock <- function(...) {
    a <- list(...)
    key_fun <- getFromNamespace('.cache_key', 'gptr')
    if (length(a) == 1L && is.character(a[[1L]])) {
      key <- a[[1L]]
    } else {
      provider <- as.character(a[[1L]])
      base_url <- cache_root_for_test(as.character(a[[2L]]))
      key <- key_fun(provider, base_url)
    }
    store$get(key, missing = NULL)
  }
  cache_put_mock <- function(...) {
    a <- list(...)
    key_fun <- getFromNamespace('.cache_key', 'gptr')
    provider <- as.character(a[[1L]])
    base_url <- cache_root_for_test(as.character(a[[2L]]))
    models <- a[[3L]]
    key <- key_fun(provider, base_url)
    store$set(
      key,
      list(provider = provider, base_url = base_url, models = models, ts = get_next_ts())
    )
    invisible(TRUE)
  }
  cache_del_mock <- function(...) {
    a <- list(...)
    key_fun <- getFromNamespace('.cache_key', 'gptr')
    provider <- as.character(a[[1L]])
    base_url <- cache_root_for_test(as.character(a[[2L]]))
    key <- key_fun(provider, base_url)
    store$remove(key)
    invisible(TRUE)
  }
  local_namespace_binding("gptr", ".cache_get", cache_get_mock, env = parent.frame())
  local_namespace_binding("gptr", ".cache_put", cache_put_mock, env = parent.frame())
  local_namespace_binding("gptr", ".cache_del", cache_del_mock, env = parent.frame())
  store
}

fixed_ts <- 1755806518
fix_time <- function(expr) {
  withr::local_envvar(c(TZ = "UTC"))
  withr::with_options(list(gpt.timeout = 2, gptr.request_timeout = 2), {
    withr::with_seed(1, {
      force(expr)
    })
  })
}

make_fake_cache <- function() {
  store <- cachem::cache_mem()
  key_fun <- getFromNamespace('.cache_key', 'gptr')
  list(
    get = function(provider, base_url) {
      key <- key_fun(provider, cache_root_for_test(base_url))
      store$get(key, missing = NULL)
    },
    put = function(provider, base_url, models) {
      root <- cache_root_for_test(base_url)
      key <- key_fun(provider, root)
      store$set(key, list(provider = provider, base_url = root, models = models, ts = fixed_ts))
      invisible(TRUE)
    },
    cache = store
  )
}

openai_models_payload <- function() {
  list(
    data = list(
      list(id = "gpt-4o", created = 1683758102),
      list(id = "gpt-4.1-mini", created = 1686558896)
    )
  )
}


