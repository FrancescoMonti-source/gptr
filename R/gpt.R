#' Chat once with the selected provider
#'
#' Minimal front-door that delegates to provider-specific helpers.
#' Returns plain text (character scalar). Usage is attached as an attribute.
#'
#' @param prompt Character scalar user message. Ignored if `messages` is supplied upstream.
#' @param model Optional model id. If NULL, resolved per provider defaults.
#' @param temperature Numeric scalar (default 0.2).
#' @param provider One of "auto", "local", "openai", "lmstudio", "ollama", "localai".
#' @param base_url “Optional. Pin a specific local endpoint (…/v1 or …/v1/chat/completions).”
#' @param backend “Optional. When provider is local, choose a running backend ('lmstudio', 'ollama', 'localai').”
#' @param openai_api_key Optional API key for OpenAI; defaults to env var.
#' @param image_path Optional path or vector of paths to images to include.
#' @param system Optional system prompt.
#' @param seed Optional integer for determinism (when supported).
#' @param response_format NULL, "json_object", or a full list (OpenAI API shape).
#' @param print_raw Logical. If TRUE, pretty-print a compact response skeleton and return it immediately (skips any post-processing). Default FALSE.
#' @param ... Extra fields passed through to the provider payload (e.g. `max_tokens`, `stop`).
#'
#' @return Character scalar (assistant message). `attr(value, "usage")` may contain token usage.
#' @export
#'
gpt <- function(prompt,
                model = NULL,
                temperature = 0.2,
                provider = c("auto", "lmstudio", "openai", "ollama", "local", "localai"),
                base_url = NULL,
                openai_api_key = Sys.getenv("OPENAI_API_KEY", unset = ""),
                image_path = NULL,
                system = NULL,
                seed = NULL,
                response_format = NULL,
                backend = NULL,
                strict_model = getOption("gptr.strict_model", TRUE),
                allow_backend_autoswitch = getOption("gptr.local_autoswitch", TRUE),
                print_raw = FALSE,
                ...) {

    provider <- match.arg(provider)
    provider_input <- provider
    base_root <- NULL

    # --- Early auto+model resolution (use cache, no heuristics) ---
    if (identical(provider, "auto") && is.character(model) && nzchar(model)) {
        lm <- try(.resolve_model_provider(model, openai_api_key = openai_api_key), silent = TRUE)
        if (!inherits(lm, "try-error") && NROW(lm)) {
            hits <- lm
            prefer_locals <- getOption("gptr.local_prefer", c("lmstudio","ollama","localai"))
            rank_fn <- function(p) {
                m <- match(p, c(prefer_locals, "openai"))
                ifelse(is.na(m), 999L, m)
            }
            ord <- order(rank_fn(tolower(as.character(hits$provider))))
            hit <- hits[ord[1L], , drop = FALSE]
            hit_provider <- tolower(as.character(hit$provider))
            if (identical(hit_provider, "openai")) {
                provider <- "openai"
            } else {
                provider <- "local"
                backend  <- hit_provider
                base_root <- .api_root(as.character(hit$base_url[1L]))
            }
        } else {
            rlang::abort(sprintf("Model '%s' is not available; specify a provider.", model))
        }
    }

    # --- Normalize local provider aliases ---
    if (provider %in% c("lmstudio","ollama","localai")) {
        backend  <- provider
        provider <- "local"
    }

    # --- Guarded local root resolution (only if still unset) ---
    if ((provider %in% c("local","auto")) && is.null(base_root)) {
        roots <- list(
            lmstudio = getOption("gptr.lmstudio_base_url", "http://127.0.0.1:1234"),
            ollama   = getOption("gptr.ollama_base_url",   "http://127.0.0.1:11434"),
            localai  = getOption("gptr.localai_base_url",  "http://127.0.0.1:8080")
        )
        prefer <- getOption("gptr.local_prefer", c("lmstudio","ollama","localai"))

        if (!is.null(base_url) && nzchar(base_url)) {
            base_root <- .api_root(base_url)
            provider  <- "local"
        } else if (!is.null(backend) && nzchar(backend) && backend %in% names(roots)) {
            base_root <- .api_root(roots[[backend]])
            provider  <- "local"
        } else {
            picked <- FALSE
            for (bk in prefer) {
                lm <- try(.fetch_models_cached(provider = bk, base_url = roots[[bk]],
                                                   openai_api_key = openai_api_key), silent = TRUE)
                if (!inherits(lm, "try-error") && is.list(lm) && NROW(lm$df)) {
                    base_root <- .api_root(roots[[bk]])
                    backend   <- bk
                    picked    <- TRUE
                    break
                }
            }
            if (!picked || is.null(base_root)) {
                if (nzchar(openai_api_key)) {
                    provider <- "openai"
                    backend <- NULL
                    base_root <- NULL
                } else {
                    backend   <- prefer[[1L]]
                    base_root <- .api_root(roots[[backend]])
                    provider  <- "local"
                }
            } else {
                provider <- "local"
            }
        }
    }

    # --- helpers ---
    .compact <- function(lst) lst[!vapply(lst, is.null, logical(1L))]
    .to_skeleton <- function(body) {
        x <- if (is.character(body)) jsonlite::fromJSON(body, simplifyVector = FALSE) else body
        usage <- x$usage %||% list()
        usage <- .compact(list(
            prompt_tokens     = usage$prompt_tokens,
            completion_tokens = usage$completion_tokens,
            total_tokens      = usage$total_tokens
        ))
        choices <- x$choices %||% list()
        choices_skel <- lapply(choices, function(ch) {
            msg <- ch$message %||% ch$delta %||% list()
            content <- msg$content
            if (is.list(content)) {
                parts <- unlist(lapply(content, function(p) p$text %||% p$content %||% NULL), use.names = FALSE)
                content <- if (length(parts)) paste(parts, collapse = "") else NULL
            }
            content <- content %||% msg$text %||% ch$text %||% x$output_text %||% NULL
            .compact(list(
                index         = ch$index %||% NULL,
                finish_reason = ch$finish_reason %||% NULL,
                role          = msg$role %||% NULL,
                content       = content
            ))
        })
        model_id <- x$model %||% (x$meta$model %||% NULL)
        .compact(list(
            id      = x$id %||% NULL,
            created = x$created %||% NULL,
            model   = model_id,
            usage   = if (length(usage)) usage else NULL,
            choices = choices_skel
        ))
    }
    .handle_return <- function(res, backend_name, model_name) {
        if (isTRUE(print_raw)) {
            sk <- .to_skeleton(res$body)
            cat(jsonlite::toJSON(sk, auto_unbox = TRUE, pretty = TRUE), "\n")
            return(sk)
        }
        parsed <- openai_parse_text(res$body)
        out <- parsed$text
        attr(out, "usage") <- parsed$usage
        attr(out, "backend") <- backend_name
        attr(out, "model") <- model_name
        out
    }

    image_paths <- if (is.null(image_path)) NULL else as.character(image_path)

    # ---------------- provider == "openai" ----------------
    if (provider == "openai") {
        msgs <- openai_make_messages(system = system, user = prompt, image_paths = image_paths)
        defs <- .resolve_openai_defaults(model = model, base_url = base_url, api_key = openai_api_key)
        bu_root <- .api_root(defs$base_url)
        if (is.null(.cache_get("openai", bu_root))) {
            invisible(try(.fetch_models_cached(provider = "openai", base_url = bu_root,
                                                  openai_api_key = defs$api_key), silent = TRUE))
        }
        ids <- tryCatch(
            .fetch_models_cached(provider = "openai", base_url = bu_root,
                                     openai_api_key = defs$api_key)$df$id,
            error = function(e) character(0)
        )
        if (length(ids) && !tolower(defs$model) %in% tolower(ids)) {
            if (identical(provider_input, "auto")) {
                stop(sprintf("Model '%s' not found for OpenAI. Please specify a provider.", defs$model), call. = FALSE)
            } else {
                defs$model <- .resolve_openai_defaults(base_url = defs$base_url, api_key = defs$api_key)$model
            }
        }
        payload <- openai_compose_payload(
            messages        = msgs,
            model           = defs$model,
            temperature     = temperature,
            seed            = seed,
            response_format = response_format,
            extra           = list(...)
        )
        res <- request_openai(payload, base_url = defs$base_url, api_key = defs$api_key)
        return(.handle_return(res, backend_name = "openai", model_name = defs$model))
    }

    # ---------------- provider == "local" ----------------
    if (provider == "local") {
        stopifnot(!is.null(base_root), nzchar(base_root))

        ent <- try(.fetch_models_cached(provider = backend, base_url = base_root,
                                           openai_api_key = openai_api_key), silent = TRUE)
        ids <- if (!inherits(ent, "try-error") && is.list(ent) && !is.null(ent$df)) {
            unique(na.omit(as.character(ent$df$id)))
        } else character(0)

        default_model <- getOption("gptr.local_model", if (length(ids)) ids[[1]] else "mistralai/mistral-7b-instruct-v0.3")
        requested_model <- model %||% default_model

        if (nzchar(requested_model) && length(ids) &&
            !tolower(requested_model) %in% tolower(ids)) {
            if (identical(provider_input, "auto")) {
                stop(sprintf("Model '%s' not found. Please specify a provider.", requested_model), call. = FALSE)
            } else {
                msg <- sprintf("Model '%s' not found on %s.", requested_model, base_root)
                if (isTRUE(strict_model)) {
                    stop(msg, call. = FALSE)
                } else {
                    warning(msg, call. = FALSE)
                    requested_model <- default_model
                }
            }
        }

        msgs <- openai_make_messages(system = system, user = prompt, image_paths = image_paths)
        payload <- openai_compose_payload(
            messages        = msgs,
            model           = requested_model,
            temperature     = temperature,
            seed            = seed,
            response_format = response_format,
            extra           = list(...)
        )
        res <- request_local(payload, base_url = base_root)

        used_model <- tryCatch({
            b <- if (is.character(res$body)) jsonlite::fromJSON(res$body, simplifyVector = FALSE) else res$body
            b$model %||% (b$meta$model %||% NULL)
        }, error = function(e) NULL)
        if (!is.null(requested_model) && nzchar(requested_model) && !is.null(used_model) && nzchar(used_model)) {
            if (!identical(tolower(requested_model), tolower(used_model))) {
                msg <- sprintf("Server used model '%s' instead of requested '%s'.", used_model, requested_model)
                if (isTRUE(strict_model)) stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
            }
        }

        return(.handle_return(res, backend_name = backend %||% "custom-local", model_name = requested_model))
    }

    stop("Unsupported provider mode after normalization. This is a bug.", call. = FALSE)
}
