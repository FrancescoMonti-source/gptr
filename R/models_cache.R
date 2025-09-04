# Options you can set in .Rprofile:
# options(
#   gptr.check_model_once = TRUE,      # probe once per session (default)
#   gptr.model_cache_ttl  = 3600,      # seconds used when check_model_once = FALSE
#   gptr.lmstudio_base_url = "http://127.0.0.1:1234",
#   gptr.ollama_base_url   = "http://127.0.0.1:11434",
#   gptr.localai_base_url  = "http://127.0.0.1:8080"
# )

.gptr_cache <- {
  ttl <- if (isTRUE(getOption("gptr.check_model_once", TRUE))) NULL else getOption("gptr.model_cache_ttl", 3600)
  if (is.null(ttl)) {
    cachem::cache_mem()
  } else {
    cachem::cache_mem(max_age = ttl)
  }
}


# --- URL helpers -------------------------------------------------------------

# Normalize a base_url into a clean server root.
# Strips trailing slashes and removes accidental inclusion of /v1, /chat/completions, etc.
# Always returns just the root (e.g., "http://127.0.0.1:1234").
.api_root <- function(x) {
  x <- sub("(/v1/chat/completions)?$", "", x)
  x <- sub("(/v1/models)?$", "", x)
  x <- sub("/+$", "", x)
  x
}

# Given a root, build the canonical /v1/models endpoint URL for probing model lists.
.models_endpoint <- function(root) paste0(.api_root(root), "/v1/models")



# --- Candidate backends (local only) ----------------------------------------

# Collect the configured local backends (LM Studio, Ollama, LocalAI).
# Uses options() to allow user overrides (e.g., gptr.lmstudio_base_url).
# Returns a list of backends with provider key, name, and base_url.
.list_local_backends <- function() {
  # Users can override/extend via options(...)
  builtins <- list(
    lmstudio = list(
      provider = "lmstudio", name = "LM Studio",
      base_url = getOption("gptr.lmstudio_base_url", "http://127.0.0.1:1234")
    ),
    ollama = list(
      provider = "ollama", name = "Ollama",
      base_url = getOption("gptr.ollama_base_url", "http://127.0.0.1:11434")
    ),
    localai = list(
      provider = "localai", name = "LocalAI",
      base_url = getOption("gptr.localai_base_url", "http://127.0.0.1:8080")
    )
  )
  extras <- getOption("gptr.extra_local_backends", NULL)
  if (is.list(extras) && length(extras)) builtins <- utils::modifyList(builtins, extras)
  builtins
}

# --- Live probe --------------------------------------------------------------

#' @keywords internal
.parse_openai_models <- function(obj) {
  if (is.list(obj) && is.list(obj$data)) {
    return(.as_models_df(obj$data))
  }
  .as_models_df(NULL)
}

#' @keywords internal
.extract_model_ids <- function(obj) {
  if (is.list(obj$data)) {
    return(vapply(obj$data, function(m) m$id %||% "", ""))
  }
  if (is.list(obj$models)) {
    return(vapply(obj$models, function(m) m$id %||% "", ""))
  }
  if (is.list(obj)) {
    return(vapply(obj, function(m) m$id %||% "", ""))
  }
  if (is.character(obj)) {
    return(obj)
  }
  character(0)
}

#' @keywords internal
.parse_local_models <- function(obj) {
  ids <- .extract_model_ids(obj)
  ids <- unique(ids[nzchar(ids)])
  .as_models_df(ids)
}

#' Perform a live HTTP GET on /v1/models for a given provider and base_url.
#' Returns a list with a data frame of models (`df`) and a status string.
#' Branches on provider to apply OpenAI-specific headers and retry logic or
#' a generic flow for local backends.
#' @param provider Provider name (e.g., "openai", "ollama").
#' @param base_url Base URL of the backend.
#' @param refresh Logical flag indicating whether the caller wishes to bypass
#'   cached results.  This function itself always performs a live request and
#'   does not read from or write to the cache, but downstream callers use the
#'   flag to signal a cache bypass.
#' @param openai_api_key OpenAI API key used when `provider = "openai"`.
#' @param timeout Request timeout in seconds.
#' @keywords internal
.fetch_models_live <- function(provider,
                               base_url,
                               refresh = FALSE,
                               openai_api_key = Sys.getenv("OPENAI_API_KEY", ""),
                               timeout = getOption("gptr.request_timeout", 5)) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    return(list(df = .as_models_df(NULL), status = "httr2_missing"))
  }
  url <- .models_endpoint(base_url)
  req <- httr2::request(url) %>%
    httr2::req_timeout(timeout)

  if (identical(provider, "openai")) {
    if (!nzchar(openai_api_key)) {
      return(list(df = .as_models_df(NULL), status = "auth_missing"))
    }
    req <- req %>%
      httr2::req_headers(Authorization = paste("Bearer", openai_api_key)) %>%
      httr2::req_retry(
        max_tries = 3,
        backoff = function(i) 0.2 * i,
        is_transient = function(r) {
          sc <- try(httr2::resp_status(r), silent = TRUE)
          if (inherits(sc, "try-error")) {
            return(TRUE)
          }
          sc %in% c(408, 429, 500, 502, 503, 504)
        }
      )
  }

  resp <- try(req %>% httr2::req_perform(), silent = TRUE)
  if (inherits(resp, "try-error")) {
    return(list(df = .as_models_df(NULL), status = "unreachable"))
  }
  sc <- httr2::resp_status(resp)
  if (identical(provider, "openai") && sc == 401L) {
    return(list(df = .as_models_df(NULL), status = "auth_error"))
  }
  if (sc >= 400L) {
    return(list(df = .as_models_df(NULL), status = paste0("http_", sc)))
  }
  j <- try(httr2::resp_body_json(resp, simplifyVector = FALSE), silent = TRUE)
  if (inherits(j, "try-error")) {
    return(list(df = .as_models_df(NULL), status = "non_json"))
  }

  df <- if (identical(provider, "openai")) {
    .parse_openai_models(j)
  } else {
    .parse_local_models(j)
  }
  list(df = df, status = "ok")
}

#' @keywords internal
.ollama_tags_live <- function(base_url, timeout = getOption("gptr.request_timeout", 5)) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    return(character(0))
  }
  root <- .api_root(base_url)
  proto <- sub("^(https?://).*", "\\1", root)
  host <- sub("^https?://", "", root)
  host <- sub("/.*$", "", host)
  url <- paste0(ifelse(nzchar(proto), proto, "http://"), host, "/api/tags")

  resp <- try(
    httr2::request(url) %>%
      httr2::req_timeout(timeout) %>%
      httr2::req_retry(max_tries = 3, backoff = function(i) 0.2 * i) %>%
      httr2::req_perform(),
    silent = TRUE
  )
  if (inherits(resp, "try-error") || httr2::resp_status(resp) >= 400L) {
    return(character(0))
  }
  j <- try(httr2::resp_body_json(resp, simplifyVector = TRUE), silent = TRUE)
  if (inherits(j, "try-error") || !is.list(j) || !is.data.frame(j$models)) {
    return(character(0))
  }
  m <- j$models$name
  if (is.null(m)) m <- j$models$id
  unique(as.character(m[nzchar(m)]))
}

# --- Cache primitives --------------------------------------------------------

# Construct a unique cache key string from provider+base_url using
# only lowercase letters and numbers (cachem restriction).
.cache_key <- function(provider, base_url) {
  root <- .api_root(base_url)
  paste0(provider, digest::sha1(root))
}

# Look up a cached entry in .gptr_cache by provider+base_url.
# cachem's default `missing` value yields a `key_missing` sentinel; explicitly
# return `NULL` when an entry doesn't exist so callers can rely on `is.null()`.
.cache_get <- function(provider, base_url) {
  .gptr_cache$get(.cache_key(provider, base_url), missing = NULL)
}

#' Save a cache entry for provider+base_url with a vector of model IDs and timestamp.
#' @keywords internal
.cache_put <- function(provider, base_url, models) {
  entry <- list(
    provider = provider,
    base_url = .api_root(base_url),
    models   = models,
    ts       = as.numeric(Sys.time())
  )
  if (isTRUE(getOption("gptr.check_model_once", TRUE))) {
    .gptr_cache$set(.cache_key(provider, base_url), entry)
  } else {
    ttl <- getOption("gptr.model_cache_ttl", 3600)
    .gptr_cache$set(
      .cache_key(provider, base_url),
      entry,
      expires = Sys.time() + ttl
    )
  }
}


#' Remove a cache entry for provider+base_url from the cache.
#' @keywords internal
.cache_del <- function(provider, base_url) {
  .gptr_cache$remove(.cache_key(provider, base_url))
  invisible(TRUE)
}

## unified cached fetch -------------------------------------------------------
#' @keywords internal
.fetch_models_cached <- function(provider,
                                 base_url,
                                 openai_api_key = Sys.getenv("OPENAI_API_KEY", "")) {
  root <- .api_root(base_url)
  availability <- if (identical(provider, "openai")) "catalog" else "installed"
  ent <- .cache_get(provider, root)
  now <- as.numeric(Sys.time())
  if (!is.null(ent)) {
    use_cache <- FALSE
    if (isTRUE(getOption("gptr.check_model_once", TRUE))) {
      use_cache <- TRUE
    } else {
      ttl <- getOption("gptr.model_cache_ttl", 3600)
      if (!is.na(ent$ts) && (now - ent$ts) < ttl) use_cache <- TRUE
    }
    if (use_cache) {
      status <- if (identical(provider, "openai")) "ok_cache" else NA_character_
      return(.row_df(provider, root, ent$models, availability, "cache", ent$ts, status = status))
    }
  }
  live <- .fetch_models_live(provider, root, openai_api_key = openai_api_key)
  if (identical(live$status, "unreachable")) {
    Sys.sleep(0.2)
    live <- .fetch_models_live(provider, root, openai_api_key = openai_api_key)
  }
  mods <- live$df
  status <- live$status
  ts <- now
  if (provider == "ollama" && nrow(mods) == 0 && identical(status, "ok")) {
    mods <- .ollama_tags_live(root)
  }
  if (identical(status, "ok")) {
    if (provider == "openai") {
      if (nrow(mods) > 0) {
        .cache_put(provider, root, mods)
        ts <- .cache_get(provider, root)$ts
      } else {
        status <- "empty_cache"
      }
    } else {
      .cache_put(provider, root, mods)
      ts <- .cache_get(provider, root)$ts
    }
  }
  .row_df(provider, root, mods, availability, "live", ts, status = status)
}

#' Resolve a model's provider from cache (minimal lookup)
#' @keywords internal
.resolve_model_provider <- function(model,
                                    openai_api_key = Sys.getenv("OPENAI_API_KEY", "")) {
    providers <- c("lmstudio", "ollama", "localai", "openai")
    rows <- list()
    if (!nzchar(model)) {
        return(data.frame(provider = character(), base_url = character(),
                          model_id = character(), stringsAsFactors = FALSE))
    }
    for (p in providers) {
        if (p == "openai" && !nzchar(openai_api_key)) next
        bu <- switch(p,
            lmstudio = getOption("gptr.lmstudio_base_url", "http://127.0.0.1:1234"),
            ollama   = getOption("gptr.ollama_base_url",   "http://127.0.0.1:11434"),
            localai  = getOption("gptr.localai_base_url",  "http://127.0.0.1:8080"),
            openai   = "https://api.openai.com"
        )
        ent <- try(.fetch_models_cached(provider = p, base_url = bu,
                                            openai_api_key = openai_api_key), silent = TRUE)
        ids <- tryCatch({
            if (is.list(ent) && !is.null(ent$df)) as.character(ent$df$id) else character(0)
        }, error = function(e) character(0))
        if (length(ids) && any(tolower(ids) == tolower(model))) {
            rows[[length(rows) + 1L]] <- data.frame(
                provider = p,
                base_url = .api_root(bu),
                model_id = model,
                stringsAsFactors = FALSE
            )
        }
    }
    if (length(rows)) do.call(rbind, rows) else data.frame(
        provider = character(), base_url = character(), model_id = character(),
        stringsAsFactors = FALSE
    )
}
#' @param refresh Logical; if TRUE, the cache is skipped and the backend is
#'   queried directly without reading from or writing to the cache.
#' @keywords internal
.fetch_models_cached_local <- function(provider, base_url, refresh = FALSE) {
  if (isTRUE(refresh)) {
    live <- .fetch_models_live(provider, base_url, refresh = TRUE)
    ts <- as.numeric(Sys.time())
    return(.row_df(provider, base_url, live$df, "installed", "live", ts, status = live$status))
  }
  ent <- .cache_get(provider, base_url)
  if (is.null(ent)) {
    live <- .fetch_models_cached(provider, base_url)
    mods <- live$df
    ts <- .cache_get(provider, base_url)$ts %||% as.numeric(Sys.time())
    src <- "live"
    if (provider == "ollama" && nrow(mods) == 0) {
      mods <- .ollama_tags_live(base_url)
      .cache_put(provider, base_url, mods)
      ts <- .cache_get(provider, base_url)$ts
    }
    .row_df(provider, base_url, mods, "installed", src, ts, status = live$status)
  } else {
    .row_df(provider, base_url, ent$models, "installed", "cache", ent$ts)
  }
}

#' @param refresh Logical; if TRUE, bypasses cache and probes the OpenAI
#'   endpoint directly without touching the cache.
#' @keywords internal
.fetch_models_cached_openai <- function(openai_api_key,
                                        base_url = "https://api.openai.com",
                                        refresh = FALSE) {
  if (isTRUE(refresh)) {
    live <- .fetch_models_live("openai", base_url, refresh = TRUE, openai_api_key = openai_api_key)
    ts <- as.numeric(Sys.time())
    return(.row_df("openai", base_url, live$df, "catalog", "live", ts, status = live$status))
  }
  ent <- .cache_get("openai", base_url)
  if (is.null(ent)) {
    live <- .fetch_models_live("openai", base_url, openai_api_key = openai_api_key)
    if (identical(live$status, "ok") && nrow(live$df) > 0) {
      .cache_put("openai", base_url, live$df)
      ts <- .cache_get("openai", base_url)$ts
    } else if (identical(live$status, "ok") && nrow(live$df) == 0) {
      live$status <- "empty_cache"
      ts <- as.numeric(Sys.time())
    } else {
      ts <- as.numeric(Sys.time())
    }
  }
  if (length(rows)) do.call(rbind, rows) else data.frame(
    provider = character(), base_url = character(), model_id = character(),
    stringsAsFactors = FALSE
  )
}
# --- Public API --------------------------------------------------------------

#' List models across local backends (LM Studio, Ollama, LocalAI) and OpenAI.
#'
#' Behavior:
#' - Local providers read from the in-session cache by default (fast). Use `refresh=TRUE`
#'   to bypass the cache and probe `/v1/models` directly (Ollama falls back to `/api/tags`).
#'   When refreshing, cached entries are neither read nor updated.
#' - OpenAI is included if an API key is available (or explicitly provided).
#' - Output is normalized with an `availability` column:
#'     * "installed" for local backends
#'     * "catalog"   for OpenAI (account/catalog listing)
#' - Stable columns: provider, base_url, model_id, availability, cached_when, source.
#' - If a backend returns no models, zero rows are returned and the status is
#'   recorded in `attr(x, "diagnostics")`.
#' 
#' @param provider NULL (default, list all), or one of:
#'   "lmstudio","ollama","localai","openai".
#' @param base_url Optional root URL to target a specific server. If NULL,
#'   defaults from options are used for locals, and https://api.openai.com for OpenAI.
#' @param refresh Logical. If TRUE, bypasses cache and queries providers
#'   directly. Cached entries are left untouched.
#' @param openai_api_key Optional OpenAI API key. If missing, falls back to
#'   Sys.getenv("OPENAI_API_KEY"). If still empty, OpenAI rows will indicate
#'   an auth_missing status (no stop).
#' @return data.frame with columns:
#'   provider, base_url, model_id, availability, cached_when, source, and status.
#'   Status diagnostics for each backend are also attached via
#'   `attr(x, "diagnostics")`.
#' @export
list_models <- function(provider = NULL,
                        base_url = NULL,
                        refresh = FALSE,
                        openai_api_key = Sys.getenv("OPENAI_API_KEY", "")) {
  # ---- scope selection ---
  providers <- if (is.null(provider)) c("lmstudio", "ollama", "localai", "openai") else provider
  rows <- list()
  diagnostics <- list()

  for (p in providers) {
    if (p %in% c("lmstudio", "ollama", "localai")) {
      bu_default <- switch(p,
        lmstudio = getOption("gptr.lmstudio_base_url", "http://127.0.0.1:1234"),
        ollama   = getOption("gptr.ollama_base_url", "http://127.0.0.1:11434"),
        localai  = getOption("gptr.localai_base_url", "http://127.0.0.1:8080")
      )
      bu <- .api_root(base_url %||% bu_default)

      if (isTRUE(refresh)) {
        live <- .fetch_models_live(p, bu, refresh = TRUE)
        df <- .row_df(p, bu, live$df, "installed", "live", as.numeric(Sys.time()), status = live$status)
      } else {
        df <- .fetch_models_cached(p, bu, openai_api_key = openai_api_key)
      }

      diagnostics[[length(diagnostics) + 1L]] <- attr(df, "diagnostic")
      rows[[length(rows) + 1L]] <- df
    } else if (p == "openai") {
      bu <- .api_root(base_url %||% "https://api.openai.com")
      if (isTRUE(refresh)) {
        live <- .fetch_models_live("openai", bu, refresh = TRUE, openai_api_key = openai_api_key)
        df <- .row_df("openai", bu, live$df, "catalog", "live", as.numeric(Sys.time()), status = live$status)
      } else {
        df <- .fetch_models_cached("openai", bu, openai_api_key = openai_api_key)
      }

      diagnostics[[length(diagnostics) + 1L]] <- attr(df, "diagnostic")
      rows[[length(rows) + 1L]] <- df
    }
  }


  if (!length(rows)) {
    return(data.frame(
      provider = character(),
      base_url = character(),
      model_id = character(),
      created = numeric(),
      availability = character(),
      cached_at = as.POSIXct(numeric(), origin = "1970-01-01", tz = "UTC"),
      source = character(),
      status = character(),
      stringsAsFactors = FALSE
    ))
  }

  out <- do.call(rbind, rows)
  attr(out, "diagnostics") <- diagnostics

  # Add human-readable timestamp (UTC) where 'created' is present
  out$created <- suppressWarnings(as.numeric(out$created))
  out$created <- as.POSIXct(out$created, origin = "1970-01-01", tz = "UTC")

  # Stable ordering: locals first, then by recency (where available), then model_id
  ord <- order(
    match(out$provider, c("lmstudio", "ollama", "localai", "openai"), nomatch = 99L),
    out$availability,
    -ifelse(is.na(out$created), 0, out$created),
    tolower(ifelse(is.na(out$model_id), "", out$model_id))
  )
  out <- out[ord, , drop = FALSE]
  attr(out, "diagnostics") <- diagnostics
  out
}

# Convenience wrapper ------------------------------------------------------

#' Refresh model listings, bypassing cache
#'
#' Convenience wrapper around [list_models()] with `refresh = TRUE`.
#' @inheritParams list_models
#' @return See [list_models()]
#' @export
refresh_models <- function(...) {
  list_models(refresh = TRUE, ...)
}

# ---------------------------------------------------------------------------

  #' Clear cache entries so that the next run will re-probe the server.
#'
#' With no arguments, clears all cached model listings. You can target a
#' subset by specifying a `provider`, a `base_url`, or both.
#' @param provider Optional provider name to invalidate.
#' @param base_url Optional base URL of the backend to invalidate.
#' @return invisible TRUE
#' @export
delete_models_cache <- function(provider = NULL, base_url = NULL) {
    keys <- .gptr_cache$keys()
    if (!length(keys)) return(invisible(TRUE))

    # no args: wipe everything
    if (is.null(provider) && is.null(base_url)) {
        .gptr_cache$reset()
        return(invisible(TRUE))
    }

    # provider only
    if (!is.null(provider) && is.null(base_url)) {
        for (k in keys) {
            ent <- .gptr_cache$get(k, missing = NULL)
            if (!is.null(ent) && identical(ent$provider, provider)) {
                .cache_del(ent$provider, ent$base_url)
            }
        }
        return(invisible(TRUE))
    }

    # base_url only
    if (is.null(provider) && !is.null(base_url)) {
        root <- .api_root(base_url)
        for (k in keys) {
            ent <- .gptr_cache$get(k, missing = NULL)
            if (!is.null(ent) && identical(ent$base_url, root)) {
                .cache_del(ent$provider, ent$base_url)
            }
        }
        return(invisible(TRUE))
    }

    # both provider and base_url
    .cache_del(provider, base_url)
    invisible(TRUE)
}


