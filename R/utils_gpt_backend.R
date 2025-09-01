#' #' Authoritative list of default chat endpoints for each backend.
#' #' The resolver uses it to look up a base URL when the caller hasn’t provided one.
#' #' @keywords Internal
#' .gptr_registry <- local({
#'   list(
#'     lmstudio = list(default_base = "http://127.0.0.1:1234"),
#'     ollama   = list(default_base = "http://127.0.0.1:11434"),
#'     localai  = list(default_base = "http://127.0.0.1:8080"),
#'     openai   = list(default_base = "https://api.openai.com/v1/chat/completions")
#'   )
#' })
#'
#'
#' #' Resolve provider, backend and base URL for a GPT call.
#' #'
#' #' Normalises the `provider`, `backend` and `base_url` arguments based on
#' #' explicit user values, model hints, the presence of a running local backend,
#' #' and whether an OpenAI API key is available.
#' #'
#' #' @keywords internal
#' .resolve_provider_backend <- function(provider,
#'                                       backend = NULL,
#'                                       base_url = NULL,
#'                                       model = NULL,
#'                                       openai_api_key = Sys.getenv("OPENAI_API_KEY", unset = "")) {
#'   provider <- match.arg(provider, c("auto", "local", "openai", "lmstudio", "ollama", "localai"))
#'
#'   # Shorthand local providers
#'   if (provider %in% c("lmstudio", "ollama", "localai")) {
#'     backend <- provider
#'     provider <- "local"
#'   }
#'
#'   # Non-auto providers: honour as-is
#'   if (provider != "auto") {
#'     if (provider == "local" && (is.null(base_url) || !nzchar(base_url))) {
#'       # pick default base URL based on backend or options
#'       if (!is.null(backend) && backend %in% names(.gptr_registry)) {
#'         opt_name <- paste0("gptr.", backend, "_base_url")
#'         base_url <- getOption(opt_name, .gptr_registry[[backend]]$default_base)
#'       } else {
#'         base_url <- .resolve_base_url("auto", NULL)
#'       }
#'     }
#'     return(list(provider = provider, backend = backend, base_url = base_url))
#'   }
#'
#'   # provider == "auto"
#'   # explicit base_url <U+2192> local or openai
#'   if (!is.null(base_url) && nzchar(base_url)) {
#'     bu <- tolower(base_url)
#'     if (grepl("api\\.openai\\.com|\\.openai\\.azure\\.com", bu)) {
#'       return(list(provider = "openai", backend = NULL, base_url = base_url))
#'     }
#'     # otherwise local; detect backend from URL
#'     backend_detected <- backend
#'     if (is.null(backend_detected) || !nzchar(backend_detected)) {
#'       backend_detected <- .which_backend_from_base_url(base_url)
#'       if (!backend_detected %in% c("lmstudio", "ollama", "localai", "local", "openai")) {
#'         backend_detected <- "local"
#'       }
#'     }
#'     return(list(provider = "local", backend = backend_detected, base_url = base_url))
#'   }
#'
#'   # hint from model <U+2192> OpenAI if API key present
#'   looks_openai <- is.character(model) && nzchar(model) &&
#'     grepl("^(gpt-|o[0-9])", model, ignore.case = TRUE)
#'   if (looks_openai && nzchar(openai_api_key)) {
#'     return(list(provider = "openai", backend = NULL, base_url = NULL))
#'   }
#'
#'   # explicit backend hint
#'   if (!is.null(backend) && nzchar(backend) && backend != "auto") {
#'     opt_name <- paste0("gptr.", backend, "_base_url")
#'     base <- getOption(opt_name, .gptr_registry[[backend]]$default_base)
#'     return(list(provider = "local", backend = backend, base_url = base))
#'   }
#'
#'   # detect running local backends; choose one if available
#'   avail <- .detect_local_backends()
#'   if (is.data.frame(avail) && nrow(avail) > 0) {
#'     chosen <- .pick_local_backend(avail, require_model = model)
#'     if (!is.null(chosen) && nrow(chosen) >= 1) {
#'       return(list(
#'         provider = "local",
#'         backend = as.character(chosen$backend[[1]]),
#'         base_url = as.character(chosen$base_url[[1]])
#'       ))
#'     }
#'   }
#'
#'   # fallback: if API key exists, use OpenAI; else local
#'   if (nzchar(openai_api_key)) {
#'     return(list(provider = "openai", backend = NULL, base_url = NULL))
#'   }
#'
#'   list(provider = "local", backend = backend, base_url = base_url)
#' }
#'
#'
#'
#' #'  Encapsulates “given a provider and no explicit URL, what endpoint should we call?” logic.
#' #'  It also honours a global gptr.local_base_url override. Even with a central resolver,
#' #'   you still need a helper like this to construct a canonical URL for local calls.
#' #'  In fact, in the resolver we suggested calling .resolve_base_url("auto", NULL)
#' #'   to pick a generic local default when no backend is specified.
#' #'  @keywords Internal
#'
#' # .resolve_base_url <- function(provider, base_url) {
#' #   if (identical(provider, "local")) "auto" else provider
#' #   if (!is.null(base_url) && nzchar(base_url)) {
#' #     return(base_url)
#' #   }
#' #   if (provider %in% c("auto", "lmstudio", "ollama", "localai")) {
#' #     pinned <- getOption("gptr.local_base_url", "")
#' #     if (nzchar(pinned)) {
#' #       return(pinned)
#' #     }
#' #   }
#' #   if (provider %in% names(.gptr_registry)) {
#' #     return(.gptr_registry[[provider]]$default_base)
#' #   }
#' #   if (provider == "auto") {
#' #     return(getOption("gptr.lmstudio_base_url", .gptr_registry$lmstudio$default_base))
#' #   }
#' #   stop(sprintf("Unknown provider '%s'.", provider))
#' # }
#'
#'
#' #' pings the selected local server’s /models endpoint and caches the result GitHub.
#' #' The resolver doesn’t deal with server reachability at all;
#' #' gpt() still calls .ensure_backend_up() to fail fast if a local backend isn’t running or reachable.
#' #' @keywords Internal
#' .ensure_backend_up <- function(base_url, provider) {
#'   if (identical(provider, "local")) "auto" else provider
#'   if (provider == "openai") {
#'     return(invisible(TRUE))
#'   }
#'   root <- sub("/chat/completions$", "", base_url)
#'   if (exists(root, envir = .gptr_state$ping_cache, inherits = FALSE)) {
#'     ok <- get(root, envir = .gptr_state$ping_cache)
#'     if (isTRUE(ok)) {
#'       return(invisible(TRUE))
#'     }
#'     stop(attr(ok, "msg"), call. = FALSE)
#'   }
#'   vmsg <- isTRUE(getOption("gptr.verbose_preflight", FALSE))
#'   ok <- tryCatch(
#'     {
#'       httr2::request(paste0(root, "/models")) %>%
#'         httr2::req_timeout(1) %>%
#'         httr2::req_perform()
#'       TRUE
#'     },
#'     error = function(e) e
#'   )
#'   if (isTRUE(ok)) {
#'     if (vmsg) message(sprintf("[gptr] %s reachable at %s", provider, base_url))
#'     assign(root, TRUE, envir = .gptr_state$ping_cache)
#'     return(invisible(TRUE))
#'   } else {
#'     msg <- sprintf(
#'       "Could not reach '%s' at %s. Pass `base_url=` or set options(gptr.%s_base_url=...).",
#'       provider, base_url,
#'       if (provider %in% c("lmstudio", "ollama", "localai")) provider else "local"
#'     )
#'     bad <- FALSE
#'     attr(bad, "msg") <- msg
#'     assign(root, bad, envir = .gptr_state$ping_cache)
#'     stop(msg, call. = FALSE)
#'   }
#' }
