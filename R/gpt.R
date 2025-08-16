#' Chat once with the selected provider
#'
#' Minimal front-door that delegates to provider-specific helpers.
#' Returns plain text (character scalar). Usage is attached as an attribute.
#'
#' @param prompt Character scalar user message. Ignored if `messages` is supplied upstream.
#' @param model Optional model id. If NULL, resolved per provider defaults.
#' @param temperature Numeric scalar (default 0.2).
#' @param provider One of "local", "openai".
#' @param base_url Optional override of the chat completions endpoint.
#' @param openai_api_key Optional API key for OpenAI; defaults to env var.
#' @param image_path Optional path or vector of paths to images to include.
#' @param system Optional system prompt.
#' @param seed Optional integer for determinism (when supported).
#' @param response_format NULL, "json_object", or a full list (OpenAI API shape).
#' @param ... Extra fields passed through to the provider payload (e.g. `max_tokens`, `stop`).
#'
#' @return Character scalar (assistant message). `attr(value, "usage")` may contain token usage.
#' @export
gpt <- function(
        prompt,
        model = NULL,
        temperature = 0.2,
        provider = c("local", "openai"),
        base_url = NULL,
        openai_api_key = Sys.getenv("OPENAI_API_KEY", unset = ""),
        image_path = NULL,
        system = NULL,
        seed = NULL,
        response_format = NULL,
        backend = NULL,   # already added earlier
        strict_model = getOption("gpt.strict_model", TRUE),              # NEW
        allow_backend_autoswitch = getOption("gpt.local_autoswitch", TRUE), # NEW
        ...
) {
    provider <- match.arg(provider)
    image_paths <- if (is.null(image_path)) NULL else as.character(image_path)

    # ------------ provider == "openai" ------------
    if (provider == "openai") {
        # (unchanged OpenAI path)
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
            messages = msgs,
            model = defs$model,
            temperature = temperature,
            seed = seed,
            response_format = response_format,
            extra = list(...)
        )
        res <- request_openai(payload, base_url = defs$base_url, api_key = defs$api_key)
        parsed <- openai_parse_text(res$body)
        out <- parsed$text
        attr(out, "usage") <- parsed$usage
        attr(out, "backend") <- "openai"
        attr(out, "model") <- defs$model
        return(out)
    }

    # ------------ provider == "local" ------------
    # Case A: user forces a base_url  -> validate model, if possible
    if (!is.null(base_url) && nzchar(base_url)) {
        # optional: validate model by probing /models
        requested_model <- model
        if (!is.null(requested_model) && nzchar(requested_model)) {
            ids <- .models_from_base_url(base_url)
            if (length(ids) && !tolower(requested_model) %in% tolower(ids)) {
                stop(sprintf(
                    "Model '%s' not found on this server.\nTo list models on this server:\n  list_models(base_url = %s)",
                    requested_model, shQuote(base_url)
                ), call. = FALSE)
            }
        }

        model <- requested_model %||% getOption("gpt.local_model", "mistralai/mistral-7b-instruct-v0.3")

        msgs <- openai_make_messages(system = system, user = prompt, image_paths = image_paths)
        payload <- openai_compose_payload(
            messages = msgs,
            model = model,
            temperature = temperature,
            seed = seed,
            response_format = response_format,
            extra = list(...)
        )
        res <- request_local(payload, base_url = base_url)
        parsed <- openai_parse_text(res$body)
        out <- parsed$text
        attr(out, "usage") <- parsed$usage
        attr(out, "backend") <- backend %||% "custom-local"
        attr(out, "model") <- model
        return(out)
    }

    # Case B: auto-detect backends and pick one (optionally by model)
    avail <- .detect_local_backends()
    if (!nrow(avail)) {
        stop("No local OpenAI-compatible backend detected. Set a base_url or start LM Studio/Ollama/LocalAI.", call. = FALSE)
    }

    # respect explicit backend filter
    if (!is.null(backend) && nzchar(backend)) {
        avail <- avail[avail$backend == backend, , drop = FALSE]
        if (!nrow(avail)) stop(sprintf(
            "Requested backend '%s' is not running.\nTo see running backends and models:\n  list_models(provider = c(\"any\",\"lmstudio\",\"ollama\",\"localai\",\"openai\"))",
            backend
        ), call. = FALSE)
    }

    chosen <- NULL
    requested_model <- model

    # If user asked for a model, try to find a backend that has it
    if (!is.null(requested_model) && nzchar(requested_model)) {
        has <- vapply(avail$models, function(vec) is.character(vec) && any(tolower(vec) == tolower(requested_model)), FALSE)
        if (any(has)) {
            # choose the first backend (in preference order) that has the model
            chosen <- avail[which(has)[1L], , drop = FALSE]
        } else {
            if (!any(has)) {
                if (isTRUE(strict_model)) {
                    stop(sprintf(
                        "Model '%s' not found on any running local backend.\nTo list available models:\n  list_models(provider = c(\"any\",\"lmstudio\",\"ollama\",\"localai\",\"openai\"))",
                        requested_model
                    ), call. = FALSE)
                } else {
                    warning(sprintf(
                        "Model '%s' not found; falling back to a default model.",
                        requested_model
                    ), call. = FALSE)
                    requested_model <- NULL
                }
            }

        }
    }

    # If we still haven't chosen, pick by preference (and optionally auto-switch for model)
    if (is.null(chosen)) {
        chosen <- .pick_local_backend(avail, require_model = NULL)  # already ordered by preference
    }

    base_url <- chosen$base_url
    if (is.null(requested_model) || !nzchar(requested_model)) {
        # use configured default or first model on that backend
        requested_model <- getOption("gpt.local_model", NULL)
        if (is.null(requested_model) || !nzchar(requested_model)) {
            mods <- chosen$models[[1L]]
            if (is.character(mods) && length(mods)) {
                requested_model <- mods[[1L]]
            } else {
                requested_model <- "mistralai/mistral-7b-instruct-v0.3"
            }
        }
    }

    msgs <- openai_make_messages(system = system, user = prompt, image_paths = image_paths)
    payload <- openai_compose_payload(
        messages = msgs,
        model = requested_model,
        temperature = temperature,
        seed = seed,
        response_format = response_format,
        extra = list(...)
    )
    res <- request_local(payload, base_url = base_url)
    parsed <- openai_parse_text(res$body)
    out <- parsed$text
    attr(out, "usage") <- parsed$usage
    attr(out, "backend") <- chosen$backend
    attr(out, "model") <- requested_model
    out
}
