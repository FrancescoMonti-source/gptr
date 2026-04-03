#' Low-level single request to the selected provider
#'
#' Low-level front-door used by `gpt_column()` and advanced users who want a
#' single provider call. Returns plain text (character scalar). Usage is
#' attached as an attribute.
#'
#' @param prompt Character scalar user message. Ignored if `messages` is supplied upstream.
#' @param model Optional model id. If NULL, resolved per provider defaults.
#' @param temperature Numeric scalar (default 0.2).
#' @param provider One of "auto", "local", "openai", "lmstudio", "ollama", "localai".
#'   With `provider = "auto"`, `gpt()` uses the configured local route
#'   (`base_url`, `backend`, `gptr.local_base_url`, or `gptr.local_prefer`) and
#'   only falls back to OpenAI when no local route is configured and remote
#'   access is allowed.
#' @param base_url "Optional. Pin a specific local endpoint (…/v1 or …/v1/chat/completions)."
#' @param backend Optional. When provider is local, choose a running backend ('lmstudio', 'ollama', 'localai').
#' @param openai_api_key Optional API key for OpenAI; defaults to env var.
#' @param file_path Optional path or vector of paths to files to include.
#' @param image_path Optional path or vector of paths to images to include.
#' @param system Optional system prompt.
#' @param seed Optional integer for determinism (when supported).
#' @param response_format NULL, "json_object", or a full list (OpenAI API shape).
#' @param strict_model Logical. If TRUE (default), error if the requested model is
#'   unavailable on the chosen backend.
#' @param allow_backend_autoswitch Retained for backward compatibility.
#'   `gpt()` no longer probes alternative local backends while choosing a route.
#' @param print_raw Logical. If TRUE, pretty-print the compact response
#'   skeleton as JSON in the console and return that skeleton list instead of
#'   the assistant text (skipping any downstream post-processing). Default
#'   FALSE.
#' @param ssl_cert Optional path to a certificate authority (CA) bundle passed to
#'   provider HTTP clients and model probes as `cainfo`.
#' @param allow_remote Logical. If FALSE (default), block transmission to
#'   non-loopback endpoints, including OpenAI cloud and explicit non-local
#'   OpenAI-compatible base URLs. Set to TRUE to opt in to remote transmission.
#' @param ... Extra fields passed through to the provider payload (e.g. `max_tokens`, `stop`).
#'
#' @return Character scalar (assistant message). `attr(value, "usage")` may
#'   contain token usage. When `print_raw = TRUE`, prints a JSON skeleton to the
#'   console and returns that skeleton list instead of the assistant text.
#' @export
#'
gpt <- function(prompt,
                model = NULL,
                temperature = 0.2,
                provider = c("auto", "lmstudio", "openai", "ollama", "local", "localai"),
                base_url = NULL,
                openai_api_key = Sys.getenv("OPENAI_API_KEY", unset = ""),
                file_path = NULL,
                image_path = NULL,
                system = NULL,
                seed = NULL,
                response_format = NULL,
                backend = NULL,
                strict_model = getOption("gptr.strict_model", TRUE),
                allow_backend_autoswitch = getOption("gptr.local_autoswitch", TRUE),
                print_raw = FALSE,
                ssl_cert = getOption("gptr.ssl_cert", NULL),
                allow_remote = getOption("gptr.allow_remote", FALSE),
                ...) {

    route <- .resolve_request_route(
        provider = provider,
        backend = backend,
        base_url = base_url,
        openai_api_key = openai_api_key,
        allow_remote = allow_remote
    )
    provider <- route$provider
    backend <- route$backend
    base_root <- route$base_root
    effective_openai_api_key <- route$effective_openai_api_key

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
            return(invisible(sk))
        }
        parsed <- openai_parse_text(res$body)
        out <- parsed$text
        attr(out, "usage") <- parsed$usage
        attr(out, "backend") <- backend_name
        attr(out, "model") <- model_name
        return(out)
    }

    is_url <- function(x) grepl("^(https?://|data:)", x, ignore.case = TRUE)
    file_paths <- if (is.null(file_path)) NULL else as.character(file_path)
    image_paths <- if (is.null(image_path)) NULL else as.character(image_path)

    if (length(file_paths)) {
        local_files <- file_paths[!is_url(file_paths)]
        missing_files <- local_files[!file.exists(local_files)]
        if (length(missing_files)) {
            stop(sprintf("File not found: %s", missing_files[[1L]]), call. = FALSE)
        }
        if (length(local_files) && !requireNamespace("base64enc", quietly = TRUE)) {
            stop("Package 'base64enc' is required when `file_path` points to local files.", call. = FALSE)
        }
    }

    # ---------------- provider == "openai" ----------------
    if (provider == "openai") {
        msgs <- openai_build_messages(
            system = system,
            user = prompt,
            image_paths = image_paths,
            file_paths = file_paths
        )
        defs <- .resolve_openai_defaults(model = model, base_url = base_url, api_key = openai_api_key)
        .assert_remote_allowed(
            defs$base_url,
            allow_remote = allow_remote,
            context = "This gpt() call"
        )
        if (isTRUE(strict_model)) {
            bu_root <- .api_root(defs$base_url)
            ids <- tryCatch(
                .fetch_models_cached(
                    provider = "openai",
                    base_url = bu_root,
                    openai_api_key = defs$api_key,
                    ssl_cert = ssl_cert
                )$model_id,
                error = function(e) character(0)
            )
            if (length(ids) && !tolower(defs$model) %in% tolower(ids)) {
                stop(sprintf("Model '%s' not found for OpenAI.", defs$model), call. = FALSE)
            }
        }
        payload <- openai_build_payload(
            messages        = msgs,
            model           = defs$model,
            temperature     = temperature,
            seed            = seed,
            response_format = response_format,
            extra           = list(...)
        )
        res <- openai_send_request(
            payload,
            base_url = defs$base_url,
            api_key = defs$api_key,
            ssl_cert = ssl_cert
        )
        return(.handle_return(res, backend_name = "openai", model_name = defs$model))
    }

    # ---------------- provider == "local" ----------------
    if (provider == "local") {
        stopifnot(!is.null(base_root), nzchar(base_root))
        .assert_remote_allowed(
            base_root,
            allow_remote = allow_remote,
            context = "This gpt() call"
        )

        default_model <- getOption(
            paste0("gptr.", backend, "_model"),
            getOption("gptr.local_model", "mistralai/mistral-7b-instruct-v0.3")
        )
        requested_model <- model %||% default_model

        if (isTRUE(strict_model) && nzchar(requested_model)) {
            ent <- try(
                .fetch_models_cached(
                    provider = backend,
                    base_url = base_root,
                    openai_api_key = effective_openai_api_key,
                    ssl_cert = ssl_cert
                ),
                silent = TRUE
            )
            ids <- if (!inherits(ent, "try-error") && is.data.frame(ent)) {
                unique(na.omit(as.character(ent$model_id)))
            } else character(0)

            if (length(ids) && !tolower(requested_model) %in% tolower(ids)) {
                stop(sprintf("Model '%s' not found on %s.", requested_model, base_root), call. = FALSE)
            }
        }

        msgs <- openai_build_messages(
            system = system,
            user = prompt,
            image_paths = image_paths,
            file_paths = file_paths
        )
        payload <- openai_build_payload(
            messages        = msgs,
            model           = requested_model,
            temperature     = temperature,
            seed            = seed,
            response_format = response_format,
            extra           = list(...)
        )
        res <- .request_local(payload, base_url = base_root, ssl_cert = ssl_cert)

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
