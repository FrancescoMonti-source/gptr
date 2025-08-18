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
gpt <- function(
        prompt,
        model = NULL,
        temperature = 0.2,
        provider = c("auto","lmstudio","openai","ollama","local","localai"),  # + localai
        base_url = NULL,
        openai_api_key = Sys.getenv("OPENAI_API_KEY", unset = ""),
        image_path = NULL,
        system = NULL,
        seed = NULL,
        response_format = NULL,
        backend = NULL,
        strict_model = getOption("gpt.strict_model", TRUE),
        allow_backend_autoswitch = getOption("gpt.local_autoswitch", TRUE),
        print_raw = FALSE,
        ...
) {
    provider <- match.arg(provider)

    # normalize to API root (…/v1) when calling request_local()
    normalize_base <- function(u) {
        u <- sub("/chat/completions/?$", "", u)
        sub("/v1/?$", "/v1", u)
    }

    image_paths <- if (is.null(image_path)) NULL else as.character(image_path)

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
            tc <- msg$tool_calls %||% ch$tool_calls %||% NULL
            tool_calls <- NULL
            if (!is.null(tc) && length(tc)) {
                tool_calls <- lapply(tc, function(one) {
                    fn <- one[["function"]] %||% list()
                    args <- fn$arguments %||% NULL
                    if (is.list(args)) args <- jsonlite::toJSON(args, auto_unbox = TRUE, null = "null")
                    .compact(list(
                        id         = one$id %||% NULL,
                        type       = one$type %||% "function",
                        `function` = .compact(list(
                            name      = fn$name %||% NULL,
                            arguments = args
                        ))
                    ))
                })
            }
            .compact(list(
                index         = ch$index %||% NULL,
                finish_reason = ch$finish_reason %||% NULL,
                role          = msg$role %||% NULL,
                content       = content,
                tool_calls    = tool_calls
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
        attr(out, "usage")   <- parsed$usage
        attr(out, "backend") <- backend_name
        attr(out, "model")   <- model_name
        out
    }

    # ---------------- provider == "openai" ----------------
    if (provider == "openai") {
        msgs <- openai_make_messages(system = system, user = prompt, image_paths = image_paths)
        defs <- .resolve_openai_defaults(model = model, base_url = base_url, api_key = openai_api_key)

        if (!is.null(model) && nzchar(model)) {
            ids <- tryCatch(list_models(provider = "openai", openai_api_key = defs$api_key)$id,
                            error = function(e) character(0))
            if (length(ids) && !tolower(model) %in% tolower(ids)) {
                stop(sprintf(
                    "Model '%s' not found for OpenAI.\nTo list OpenAI models:\n  list_models(provider = 'openai')",
                    model
                ), call. = FALSE)
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

    # ---------------- provider == "local" with explicit base_url ----------------
    if (!is.null(base_url) && nzchar(base_url)) {
        base_norm <- normalize_base(base_url)
        requested_model <- model

        ids <- character(0)
        ms  <- try(list_models(base_url = base_norm), silent = TRUE)
        if (!inherits(ms, "try-error") && !is.null(ms) && NROW(ms) && "id" %in% names(ms)) {
            ids <- unname(ms$id)
        }

        if (!is.null(requested_model) && nzchar(requested_model) && length(ids)) {
            if (!tolower(requested_model) %in% tolower(ids)) {
                stop(sprintf(
                    "Model '%s' not found on this server.\nTo list models on this server:\n  list_models(base_url = %s)",
                    requested_model, shQuote(base_norm)
                ), call. = FALSE)
            }
        }

        if (is.null(requested_model) || !nzchar(requested_model)) {
            requested_model <- getOption("gpt.local_model", NULL)
            if (is.null(requested_model) || !nzchar(requested_model)) {
                requested_model <- if (length(ids)) ids[[1]] else "mistralai/mistral-7b-instruct-v0.3"
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
        res <- request_local(payload, base_url = base_norm)
        return(.handle_return(res, backend_name = backend %||% "custom-local", model_name = requested_model))
    }

    # ---------------- local-like providers: auto / lmstudio / ollama / localai ----------------
    if (provider %in% c("auto","local","lmstudio","ollama","localai")) {
        local_base <- .resolve_base_url(provider, base_url)   # returns …/v1/chat/completions
        .ensure_backend_up(local_base, provider)              # cached /models ping
        base_norm <- normalize_base(local_base)

        requested_model <- model %||% getOption("gpt.local_model", "mistralai/mistral-7b-instruct-v0.3")

        if (isTRUE(getOption("gpt.check_model_once", FALSE)) && nzchar(requested_model)) {
            ids <- try(.get_model_ids(local_base), silent = TRUE)
            if (!inherits(ids, "try-error") && length(ids)) {
                if (!tolower(requested_model) %in% tolower(ids)) {
                    if (isTRUE(strict_model)) {
                        stop(sprintf(
                            "Model '%s' not found on %s.\nEither load it there, pass base_url=, or set options(gpt.local_base_url=...).",
                            requested_model, sub("/chat/completions$", "", local_base)
                        ), call. = FALSE)
                    } else {
                        warning(sprintf(
                            "Model '%s' not found on %s; continuing (strict_model=FALSE).",
                            requested_model, sub("/chat/completions$", "", local_base)
                        ), call. = FALSE)
                    }
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

        looks_openai <- function(m) is.character(m) && nzchar(m) && grepl("^(gpt-|o[0-9])", m)

        # Try local, optionally fall back to OpenAI if provider == "auto"
        res <- tryCatch(
            request_local(payload, base_url = base_norm),
            error = function(e) {
                if (provider != "auto" || !nzchar(openai_api_key) || (!is.null(model) && !looks_openai(model)))
                    stop(e)
                # fallback to OpenAI defaults (model can be NULL or an OpenAI id)
                defs2 <- .resolve_openai_defaults(model = model, base_url = NULL, api_key = openai_api_key)
                request_openai(payload = openai_compose_payload(
                    messages        = msgs,
                    model           = defs2$model,
                    temperature     = temperature,
                    seed            = seed,
                    response_format = response_format,
                    extra           = list(...)
                ), base_url = defs2$base_url, api_key = defs2$api_key)
            }
        )

        used_model <- tryCatch({
            b <- if (is.character(res$body)) jsonlite::fromJSON(res$body, simplifyVector = FALSE) else res$body
            b$model %||% (b$meta$model %||% NULL)
        }, error = function(e) NULL)

        if (!is.null(requested_model) && nzchar(requested_model) && !is.null(used_model) && nzchar(used_model)) {
            if (!identical(tolower(requested_model), tolower(used_model))) {
                msg <- sprintf(
                    "Server used model '%s' instead of requested '%s'. Pin the correct server with base_url= or set options(gpt.local_base_url=...).",
                    used_model, requested_model
                )
                if (isTRUE(strict_model)) stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
            }
        }

        return(.handle_return(res, backend_name = backend %||% provider, model_name = requested_model))
    }
}
