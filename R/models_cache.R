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
#' Perform a live HTTP GET on /v1/models for a given provider+base_url.
#' Parses the JSON response and extracts model IDs, handling several schema variants.
#' Returns a character vector of model IDs (may be empty if unreachable or malformed).
#' @keywords internal
.fetch_models_live <- function(provider, base_url, timeout = getOption("gptr.request_timeout", 5)) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    warning("Package 'httr2' not available; cannot probe /v1/models.", call. = FALSE)
    return(character(0))
  }
  url <- .models_endpoint(base_url)
  resp <- try(httr2::request(url) %>% httr2::req_timeout(timeout) %>% httr2::req_perform(),
    silent = TRUE
  )
  if (inherits(resp, "try-error")) {
    return(character(0))
  }
  if (httr2::resp_status(resp) >= 400) {
    return(character(0))
  }

  j <- try(httr2::resp_body_json(resp, simplifyVector = FALSE), silent = TRUE)
  if (inherits(j, "try-error")) {
    return(character(0))
  }

  pick_ids <- function(obj) {
    if (is.list(obj$data)) {
      return(vapply(obj$data, \(m) m$id %||% "", ""))
    }
    if (is.list(obj$models)) {
      return(vapply(obj$models, \(m) m$id %||% "", ""))
    }
    if (is.list(obj)) {
      return(vapply(obj, \(m) m$id %||% "", ""))
    }
    if (is.character(obj)) {
      return(obj)
    }
    character(0)
  }
  ids <- pick_ids(j)
  unique(ids[nzchar(ids)])
}

#' @keywords internal
.list_openai_live <- function(openai_api_key, timeout = getOption("gptr.timeout", 5)) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    return(list(df = data.frame(id = character(0), created = numeric(0)), status = "httr2_missing"))
  }
  if (!nzchar(openai_api_key)) {
    return(list(df = data.frame(id = character(0), created = numeric(0)), status = "auth_missing"))
  }
  url <- "https://api.openai.com/v1/models"
  resp <- try(
    .http_request(url) %>%
      .http_req_headers(Authorization = paste("Bearer", openai_api_key)) %>%
      .http_req_timeout(timeout) %>%
      .http_req_retry(
        max_tries = 3,
        backoff = function(i) 0.2 * i,
        is_transient = function(r) {
          sc <- try(.http_resp_status(r), silent = TRUE)
          if (inherits(sc, "try-error")) {
            return(TRUE)
          }
          sc %in% c(408, 429, 500, 502, 503, 504)
        }
      ) %>%
      .http_req_perform(),
    silent = TRUE
  )
  if (inherits(resp, "try-error")) {
    return(list(df = data.frame(id = character(0), created = numeric(0)), status = "unreachable"))
  }

  sc <- .http_resp_status(resp)
  if (sc == 401L) {
    return(list(df = data.frame(id = character(0), created = numeric(0)), status = "auth_error"))
  }
  if (sc >= 400L) {
    return(list(df = data.frame(id = character(0), created = numeric(0)), status = paste0("http_", sc)))
  }

  # <U+2B07><U+FE0F> wrap JSON body read
  j <- try(.http_resp_body_json(resp, simplifyVector = FALSE), silent = TRUE)
  if (inherits(j, "try-error")) {
    return(list(df = data.frame(id = character(0), created = numeric(0)), status = "non_json"))
  }

  if (is.list(j) && is.list(j$data)) {
    rows <- lapply(j$data, function(m) {
      id <- tryCatch(m$id, error = function(e) NULL)
      cr <- tryCatch(m$created, error = function(e) NA_real_)
      if (!is.character(id) || length(id) != 1L || !nzchar(id)) {
        return(NULL)
      }
      data.frame(id = id, created = as.numeric(cr), stringsAsFactors = FALSE)
    })
    rows <- Filter(Negate(is.null), rows)
    df <- if (length(rows)) do.call(rbind, rows) else data.frame(id = character(0), created = numeric(0))
    return(list(df = df, status = "ok"))
  }

  # fallback when schema unexpected
  return(list(df = data.frame(id = character(0), created = numeric(0)), status = "ok"))
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

# Construct a unique cache key string from provider+base_url.
.cache_key <- function(provider, base_url) {
  paste0(provider, "::", .api_root(base_url))
}

# Look up a cached entry in .gptr_cache by provider+base_url.
# Returns NULL if not cached.
.cache_get <- function(provider, base_url) {
  .gptr_cache$get(.cache_key(provider, base_url))
}

#' Save a cache entry for provider+base_url with a vector of model IDs and timestamp.
#' @keywords internal
.cache_put <- function(provider, base_url, models) {
  entry <- list(
    models = models,
    ts     = as.POSIXct(as.numeric(Sys.time()), origin = "1970-01-01", tz = "Europe/Paris")
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

#' @keywords internal
.parse_cache_key <- function(key) {
  # key format: "<provider>::<root>"
  parts <- strsplit(key, "::", fixed = TRUE)[[1]]
  list(provider = parts[[1]], base_url = parts[[2]])
}

#' @noRd
#' @keywords internal
.list_models_cached <- function(provider = NULL, base_url = NULL) {
    # Case A: both missing -> enumerate everything currently cached (summary view)
    if (is.null(provider) && is.null(base_url)) {
        keys <- .gptr_cache$keys()
        if (!length(keys)) {
            return(data.frame(
                provider   = character(),
                base_url   = character(),
                n_models   = integer(),
                cached_at  = as.POSIXct(character(), tz = "Europe/Paris"),
                stringsAsFactors = FALSE
            ))
        }
        rows <- lapply(keys, function(k) {
            ent  <- .gptr_cache$get(k)
            meta <- .parse_cache_key(k)  # "<provider>::<base_url_root>"
            data.frame(
                provider  = meta$provider,
                base_url  = meta$base_url,
                n_models  = length(ent$models %||% character(0)),
                cached_at = if (!is.null(ent$ts)) {
                    as.POSIXct(ent$ts, origin = "1970-01-01", tz = "Europe/Paris")
                } else {
                    as.POSIXct(NA, tz = "Europe/Paris")
                },
                stringsAsFactors = FALSE
            )
        })
        return(do.call(rbind, rows))
    }

    # Case B: only provider
    if (!is.null(provider) && is.null(base_url)) {
        cands <- .list_local_backends()
        cand <- cands[[provider]]
        if (is.null(cand)) stop("Unknown provider '", provider, "'.")
        base_url <- cand$base_url
    }

    # Case C: only base_url
    if (is.null(provider) && !is.null(base_url)) {
        root <- .api_root(base_url)
        keys <- .gptr_cache$keys()
        hits <- vapply(keys, function(k) .parse_cache_key(k)$base_url == root, logical(1))
        if (!any(hits)) return(character(0))
        models <- unique(unlist(lapply(keys[hits], function(k) .gptr_cache$get(k)$models), use.names = FALSE))
        return(models %||% character(0))
    }

    # Case D: provider + base_url with TTL behavior
    ent <- .cache_get(provider, base_url)
    if (!is.null(ent)) {
        if (isTRUE(getOption("gptr.check_model_once", TRUE))) return(ent$models)
        ttl <- getOption("gptr.model_cache_ttl", 3600)
        if (!is.na(ent$ts) && (as.numeric(Sys.time()) - ent$ts) < ttl) return(ent$models)
    }
    models <- .fetch_models_live(provider, base_url)
    .cache_put(provider, base_url, models)
    models
}





# --- Public API --------------------------------------------------------------

#' List models across local backends (LM Studio, Ollama, LocalAI) and OpenAI.
#'
#' Behavior:
#' - Local providers read from the in-session cache by default (fast). Use `refresh=TRUE`
#'   to force a live probe of `/v1/models` (Ollama falls back to `/api/tags`).
#' - OpenAI is included if an API key is available (or explicitly provided).
#' - Output is normalized with an `availability` column:
#'     * "installed" for local backends
#'     * "catalog"   for OpenAI (account/catalog listing)
#' - Stable columns: provider, base_url, model_id, availability, cached_when, source.
#'
#' @param provider NULL (default, list all), or one of:
#'   "lmstudio","ollama","localai","openai".
#' @param base_url Optional root URL to target a specific server. If NULL,
#'   defaults from options are used for locals, and https://api.openai.com for OpenAI.
#' @param refresh Logical. If TRUE, forces a live probe and updates cache
#'   (for locals) or bypasses cache (for OpenAI).
#' @param openai_api_key Optional OpenAI API key. If missing, falls back to
#'   Sys.getenv("OPENAI_API_KEY"). If still empty, OpenAI rows will indicate
#'   an auth_missing status (no stop).
#' @return data.frame with columns:
#'   provider, base_url, model_id, availability, cached_when, source, and optional status.
#' @export
list_models <- function(provider = NULL,
                        base_url = NULL,
                        refresh = FALSE,
                        openai_api_key = Sys.getenv("OPENAI_API_KEY", "")) {
  # ---- scope selection ---
  providers <- if (is.null(provider)) c("lmstudio", "ollama", "localai", "openai") else provider
  rows <- list()

  for (p in providers) {
    if (p %in% c("lmstudio", "ollama", "localai")) {
      bu_default <- switch(p,
        lmstudio = getOption("gptr.lmstudio_base_url", "http://127.0.0.1:1234"),
        ollama   = getOption("gptr.ollama_base_url", "http://127.0.0.1:11434"),
        localai  = getOption("gptr.localai_base_url", "http://127.0.0.1:8080")
      )
      bu <- .api_root(base_url %||% bu_default)

      if (isTRUE(refresh)) {
        refreshed <- refresh_models_cache(p, bu)
        mods_vec <- .list_models_cached(p, bu) # character vector (locals)
        ts <- .cache_get(p, bu)$ts %||% as.numeric(Sys.time())
        src <- "live"
        if (p == "ollama" && length(mods_vec) == 0) {
          mods_vec <- .ollama_tags_live(bu)
          .cache_put(p, bu, mods_vec)
          ts <- .cache_get(p, bu)$ts
        }
        rows[[length(rows) + 1L]] <- .row_df(p, bu, .as_models_df(mods_vec), "installed", src, ts,
          status = tryCatch(refreshed$status, error = function(e) NA_character_)
        )
      } else {
        ent <- .cache_get(p, bu)
        if (is.null(ent)) {
          mods_vec <- .list_models_cached(p, bu)
          ts <- .cache_get(p, bu)$ts %||% as.numeric(Sys.time())
          src <- "live"
          if (p == "ollama" && length(mods_vec) == 0) {
            mods_vec <- .ollama_tags_live(bu)
            .cache_put(p, bu, mods_vec)
            ts <- .cache_get(p, bu)$ts
          }
          rows[[length(rows) + 1L]] <- .row_df(p, bu, .as_models_df(mods_vec), "installed", src, ts)
        } else {
          rows[[length(rows) + 1L]] <- .row_df(p, bu, .as_models_df(ent$models), "installed", "cache", ent$ts)
        }
      }
    } else if (p == "openai") {
      bu <- "https://api.openai.com"

      if (isTRUE(refresh)) {
        live <- .list_openai_live(openai_api_key)
        ts <- as.numeric(Sys.time())
        # Cache ONLY if ok and non-empty
        if (identical(live$status, "ok") && nrow(live$df) > 0) {
          .cache_put("openai", bu, live$df)
        }
        rows[[length(rows) + 1L]] <- .row_df("openai", bu, live$df, "catalog", "live", ts, status = live$status)
      } else {
        ent <- .cache_get("openai", bu)
        if (is.null(ent)) {
          live <- .list_openai_live(openai_api_key)
          if (identical(live$status, "ok") && nrow(live$df) > 0) {
            .cache_put("openai", bu, live$df)
            ts <- .cache_get("openai", bu)$ts
            rows[[length(rows) + 1L]] <- .row_df("openai", bu, live$df, "catalog", "live", ts, status = live$status)
          } else {
            ts <- as.numeric(Sys.time())
            rows[[length(rows) + 1L]] <- .row_df("openai", bu, live$df, "catalog", "live", ts, status = live$status)
          }
        } else {
          # read from cache; also surface a status so the user sees it's cached
          rows[[length(rows) + 1L]] <- .row_df("openai", bu, ent$models, "catalog", "cache", ent$ts, status = "ok_cache")
        }
      }
    }
  }


  if (!length(rows)) {
    return(data.frame(
      provider = character(), base_url = character(), model_id = character(),
      created = numeric(), availability = character(), cached_when = numeric(),
      source = character(), status = character(), stringsAsFactors = FALSE
    ))
  }

  out <- do.call(rbind, rows)


  # Add human-readable timestamp (UTC) where 'created' is present
  out$created <- suppressWarnings(as.numeric(out$created))
  out$created <- as.POSIXct(out$created, origin = "1970-01-01", tz = "Europe/Paris")

  # Stable ordering: locals first, then by recency (where available), then model_id
  ord <- order(
    match(out$provider, c("lmstudio", "ollama", "localai", "openai"), nomatch = 99L),
    out$availability,
    -ifelse(is.na(out$created), 0, out$created),
    tolower(ifelse(is.na(out$model_id), "", out$model_id))
  )
  out[ord, , drop = FALSE]
}

#' Force a live probe of /v1/models and update the cache immediately.
#' Useful after adding/pulling a new model in LM Studio/Ollama.
#' Returns a data.frame summarizing provider, base_url, count of models, refreshed_at, and status.
#' @return data.frame: provider, base_url, models_count, refreshed_at, status
#' @export
refresh_models_cache <- function(provider = NULL, base_url = NULL) {
    cands <- .list_local_backends()
    keys  <- if (is.null(provider)) names(cands) else provider
    out   <- list()

    for (p in keys) {
        cand <- cands[[p]]
        if (is.null(cand)) next

        bu     <- base_url %||% cand$base_url
        models <- .fetch_models_live(p, bu)
        .cache_put(p, bu, models)

        out[[length(out) + 1L]] <- data.frame(
            provider     = p,
            base_url     = .api_root(bu),
            models_count = length(models),
            # keep it POSIXct from the start
            refreshed_at = as.POSIXct(Sys.time(), tz = "Europe/Paris"),
            status       = if (length(models)) "ok" else "unreachable_or_empty",
            stringsAsFactors = FALSE
        )
    }

    if (!length(out)) {
        return(data.frame(
            provider     = character(),
            base_url     = character(),
            models_count = integer(),
            # zero-length POSIXct, not numeric
            refreshed_at = as.POSIXct(numeric(), origin = "1970-01-01", tz = "Europe/Paris"),
            status       = character(),
            stringsAsFactors = FALSE
        ))
    }

    out <- do.call(rbind, out)

    # safety: if anything snuck in as numeric, reclass it here
    if (!inherits(out$refreshed_at, "POSIXct")) {
        out$refreshed_at <- as.POSIXct(out$refreshed_at, origin = "1970-01-01", tz = "Europe/Paris")
    }

    out
}


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
            parts <- .parse_cache_key(k)
            if (identical(parts$provider, provider)) {
                .gptr_cache$remove(k)
            }
        }
        return(invisible(TRUE))
    }

    # base_url only
    if (is.null(provider) && !is.null(base_url)) {
        root <- .api_root(base_url)
        for (k in keys) {
            parts <- .parse_cache_key(k)
            if (identical(parts$base_url, root)) {
                .gptr_cache$remove(k)
            }
        }
        return(invisible(TRUE))
    }

    # both provider and base_url
    .gptr_cache$remove(.cache_key(provider, base_url))
    invisible(TRUE)
}


