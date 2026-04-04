# Link to openai platform documentation
browse_openai_documentation <- function(url = "https://platform.openai.com/docs/") {
  browseURL(url)
}


#' Interpret a key spec (type string or allowed set)
#' @param spec Either a type string ("integer","numeric","character","logical")
#'   or a vector of allowed values.
#' @return list(type = <chr|NULL>, allowed = <vector|NULL>)
#' @keywords internal
.parse_key_spec <- function(spec) {
  normalized_type <- if (is.character(spec) && length(spec) == 1L) .normalize_type_name(spec) else NULL
  if (!is.null(normalized_type)) {
    list(type = normalized_type, allowed = NULL, allowed_type = NULL)
  } else {
    list(type = NULL, allowed = spec, allowed_type = .infer_allowed_type(spec))
  }
}


#' normalize a scalar token for comparisons
#' @keywords internal
.normalize_token <- function(x) {
  if (is.null(x)) {
    return(NA_character_)
  }
  if (is.logical(x) && length(x) == 1L) {
    return(tolower(as.character(x)))
  }
  if (is.numeric(x) && length(x) == 1L) {
    return(as.character(x))
  }
  if (is.character(x) && length(x) == 1L) {
    return(trimws(x))
  }
  if (length(x) == 1L) {
    return(as.character(x))
  }
  NA_character_
}

#' check if a scalar belongs to an allowed set (case-insensitive for character)
#' @keywords internal
.in_allowed <- function(value, allowed) {
  if (is.null(allowed)) {
    return(TRUE)
  }
  if (length(allowed) == 0L) {
    return(TRUE)
  }
  v <- .normalize_token(value)
  if (is.na(v)) {
    return(FALSE)
  }

  if (is.character(allowed)) {
    any(tolower(v) == tolower(trimws(allowed)))
  } else {
    # numeric/integer/logical sets: compare as characterized scalars
    any(v == vapply(allowed, .normalize_token, character(1)))
  }
}


#' Determine if a scalar should be treated as NA (case-insensitive for characters)
#' @keywords internal
.is_na_like <- function(x, na_vals = c("NA", "null", "", "[]", "{}", "None")) {
  if (is.null(x)) {
    return(TRUE)
  }
  if (length(x) != 1L) {
    return(FALSE)
  }
  if (is.character(x)) {
    xv <- trimws(tolower(x))
    return(xv %in% trimws(tolower(na_vals)))
  }
  FALSE
}

.url_host <- function(x) {
  if (is.null(x) || !length(x) || is.na(x[[1]]) || !nzchar(x[[1]])) {
    return(NA_character_)
  }

  parsed <- tryCatch(httr2::url_parse(x[[1]]), error = function(e) NULL)
  host <- parsed$hostname %||% NA_character_
  if (!is.character(host) || !length(host) || is.na(host[[1]]) || !nzchar(host[[1]])) {
    return(NA_character_)
  }

  tolower(trimws(host[[1]]))
}

.is_loopback_host <- function(host) {
  if (!is.character(host) || !length(host) || is.na(host[[1]]) || !nzchar(host[[1]])) {
    return(FALSE)
  }

  host <- tolower(trimws(host[[1]]))
  host %in% c("localhost", "127.0.0.1", "::1", "[::1]")
}

.is_loopback_base_url <- function(x) {
  .is_loopback_host(.url_host(x))
}

.assert_remote_allowed <- function(base_url,
                                   allow_remote = getOption("gptr.allow_remote", FALSE),
                                   context = "This gpt() call") {
  if (isTRUE(allow_remote) || .is_loopback_base_url(base_url)) {
    return(invisible(TRUE))
  }

  target <- .url_host(base_url)
  if (is.na(target)) {
    target <- as.character(base_url[[1]] %||% "<unknown>")
  }

  stop(
    sprintf(
      "%s would send data to a non-local endpoint (%s). Set `allow_remote = TRUE` or `options(gptr.allow_remote = TRUE)` to permit remote transmission.",
      context,
      target
    ),
    call. = FALSE
  )
}

# This helper answers a deliberately narrow question:
# "Given the user's provider/backend/base_url options, where will this request go?"
#
# The important design rule is route-first resolution. We choose the route once,
# using explicit inputs and configured defaults, and only then decide anything
# else (model validation, backend-schema support, etc.).
#
# In particular, this helper does *not* search across providers by model name.
# If a caller wants discovery, that remains an explicit action via `list_models()`.
# Request execution should be boring and predictable: same inputs, same route.
.resolve_request_route <- function(provider = c("auto", "local", "openai", "lmstudio", "ollama", "localai"),
                                   backend = NULL,
                                   base_url = NULL,
                                   openai_api_key = Sys.getenv("OPENAI_API_KEY", unset = ""),
                                   allow_remote = getOption("gptr.allow_remote", FALSE)) {
  provider <- match.arg(provider)
  effective_openai_api_key <- if (isTRUE(allow_remote)) openai_api_key else ""
  request_base_url <- if (!is.null(base_url) && nzchar(base_url)) base_url else NULL

  if (provider %in% c("lmstudio", "ollama", "localai")) {
    backend <- provider
    provider <- "local"
  }

  configured_local_root <- getOption("gptr.local_base_url", NULL)
  roots <- list(
    lmstudio = getOption("gptr.lmstudio_base_url", "http://127.0.0.1:1234"),
    ollama   = getOption("gptr.ollama_base_url",   "http://127.0.0.1:11434"),
    localai  = getOption("gptr.localai_base_url",  "http://127.0.0.1:8080")
  )
  prefer <- getOption("gptr.local_prefer", c("lmstudio", "ollama", "localai"))
  valid_roots <- names(roots)[vapply(roots, function(x) !is.null(x) && nzchar(x), logical(1L))]
  prefer <- prefer[prefer %in% valid_roots]
  if (!length(prefer)) prefer <- valid_roots

  if ((provider %in% c("local", "auto")) && !is.null(backend) && nzchar(backend)) {
    backend <- tolower(as.character(backend))
    if (!(backend %in% c(names(roots), "custom-local"))) {
      rlang::abort(sprintf(
        "Unknown local backend '%s'. Use one of: %s.",
        backend,
        paste(c(names(roots), "custom-local"), collapse = ", ")
      ))
    }
  }

  base_root <- NULL
  if (provider %in% c("local", "auto")) {
    if (!is.null(request_base_url)) {
      base_root <- .api_root(request_base_url)
    } else if (!is.null(backend) && nzchar(backend) && backend %in% names(roots)) {
      base_root <- .api_root(roots[[backend]])
    } else if (!is.null(configured_local_root) && nzchar(configured_local_root)) {
      base_root <- .api_root(configured_local_root)
    } else if (length(prefer)) {
      backend <- prefer[[1L]]
      base_root <- .api_root(roots[[backend]])
    }
  }

  if (provider == "auto") {
    if (!is.null(base_root) && nzchar(base_root)) {
      provider <- "local"
    } else if (nzchar(effective_openai_api_key)) {
      provider <- "openai"
    } else {
      rlang::abort(
        "No route is configured for `provider = \"auto\"`; set `backend` or `base_url`, set `options(gptr.local_base_url = ...)`, or use `provider = \"openai\"` with `allow_remote = TRUE`."
      )
    }
  }

  if (provider == "local") {
    if (is.null(base_root) || !nzchar(base_root)) {
      rlang::abort(
        "No local route is configured; set `backend` or `base_url`, or set `options(gptr.local_base_url = ...)`."
      )
    }

    if (is.null(backend) || !nzchar(backend)) {
      normalized_roots <- vapply(roots, function(x) {
        if (is.null(x) || !nzchar(x)) return(NA_character_)
        .api_root(x)
      }, character(1L))
      hit <- names(roots)[normalized_roots == base_root]
      backend <- if (length(hit)) hit[[1L]] else "custom-local"
    }

    request_base_url <- base_root
  } else {
    backend <- NULL
  }

  list(
    provider = provider,
    backend = backend,
    base_root = if (identical(provider, "local")) base_root else NULL,
    request_base_url = request_base_url,
    effective_openai_api_key = effective_openai_api_key
  )
}

#' Show current gptr package options
#'
#' Prints all options that start with "gptr." along with their current values.
#' This is useful to inspect defaults set in `zzz.R` and any overrides applied
#' in your `.Rprofile` or current session.
#'
#' In R, to change an option, you call options() with a name = value pair:
#' options(gptr.lmstudio_base_url = "http://localhost:5678/v1/chat/completions")

#' @return Invisibly returns a named list of package options.
#' @examples
#' show_gptr_options()
#' @export
show_gptr_options <- function() {
    opts <- options()
    gpt_opts <- opts[grep("^gptr\\.", names(opts))]
    if (length(gpt_opts) == 0) {
        cli::cli_warn("No gptr.* options are currently set.")
        return(invisible(list()))
    }
    cli::cli_inform("Current gptr package options:")
    print(gpt_opts)
    invisible(gpt_opts)
}

#' Play a gptr_audio
#' @export
play <- function(x) {
  path <- if (inherits(x, "gptr_audio")) x$file else as.character(x)
  stopifnot(file.exists(path))
  # relies on OS defaults
  utils::browseURL(path)
  invisible(path)
}


#' Coalesce operator (re-export from rlang)
#' See [rlang::%||%].
#' @name %||%
#' @rdname coalesce-operator
#' @export
#' @importFrom rlang %||%
NULL

#' @importFrom stats na.omit runif setNames
NULL

#' @importFrom utils adist browseURL head tail
NULL

#' @importFrom magrittr %>%
NULL
