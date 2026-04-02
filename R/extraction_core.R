.infer_allowed_type <- function(allowed) {
    if (is.null(allowed) || !length(allowed)) {
        return(NULL)
    }

    if (is.logical(allowed)) {
        return("logical")
    }

    if (is.integer(allowed)) {
        return("integer")
    }

    if (is.numeric(allowed)) {
        if (all(is.na(allowed) | abs(allowed - round(allowed)) < .Machine$double.eps^0.5)) {
            return("integer")
        }
        return("numeric")
    }

    if (is.character(allowed)) {
        vals <- trimws(tolower(allowed))
        if (length(vals) && all(vals %in% c("true", "false", "t", "f", "yes", "y", "no", "n", "0", "1", "oui", "non"))) {
            return("logical")
        }
        num_vals <- suppressWarnings(as.numeric(vals))
        if (all(!is.na(num_vals))) {
            if (all(abs(num_vals - round(num_vals)) < .Machine$double.eps^0.5)) {
                return("integer")
            }
            return("numeric")
        }
    }

    "character"
}

.normalize_key_specs <- function(keys) {
    if (is.null(keys)) {
        return(NULL)
    }

    if (!is.list(keys) || is.null(names(keys))) {
        stop("`keys` must be a named list.", call. = FALSE)
    }

    specs <- purrr::map(keys, .parse_key_spec)
    names(specs) <- names(keys)
    specs
}

.effective_key_type <- function(spec) {
    spec$type %||% spec$allowed_type %||% "character"
}

.format_schema_value <- function(value) {
    if (is.null(value) || (length(value) == 1L && is.na(value))) {
        return("NA")
    }

    if (is.logical(value)) {
        return(tolower(as.character(value)))
    }

    if (is.numeric(value)) {
        return(as.character(value))
    }

    sprintf("\"%s\"", as.character(value))
}

.schema_prompt_hint <- function(keys) {
    key_specs <- .normalize_key_specs(keys)
    if (is.null(key_specs) || !length(key_specs)) {
        return("")
    }

    describe_spec <- function(spec) {
        if (!is.null(spec$allowed)) {
            vals <- unique(c(vapply(spec$allowed, .format_schema_value, character(1)), "\"NA\""))
            return(paste(vals, collapse = "|"))
        }

        switch(
            .effective_key_type(spec),
            integer = "\"0\"|\"1\"|\"NA\"",
            numeric = "a number (ex: 3.14)",
            character = "\"value1\"|\"value2\"|\"NA\"",
            logical = "true|false|NA",
            "\"value\"|\"NA\""
        )
    }

    json_lines <- paste(
        names(key_specs),
        vapply(key_specs, describe_spec, character(1)),
        sep = ": ",
        collapse = ", "
    )

    glue::glue("{{ {json_lines} }}")
}

.json_schema_primitive <- function(spec) {
    json_type <- switch(
        .effective_key_type(spec),
        integer = "integer",
        numeric = "number",
        logical = "boolean",
        character = "string",
        "string"
    )

    core <- list(type = json_type)
    if (!is.null(spec$allowed)) {
        core$enum <- unname(spec$allowed)
    }

    list(anyOf = list(core, list(type = "null")))
}

.schema_to_response_format <- function(key_specs, allow_additional = FALSE, schema_name = "gptr_extraction") {
    if (is.null(key_specs) || !length(key_specs)) {
        return(NULL)
    }

    properties <- lapply(key_specs, .json_schema_primitive)

    list(
        type = "json_schema",
        json_schema = list(
            name = schema_name,
            strict = !isTRUE(allow_additional),
            schema = list(
                type = "object",
                properties = properties,
                required = names(key_specs),
                additionalProperties = isTRUE(allow_additional)
            )
        )
    )
}

.as_scalar_list <- function(value) {
    if (is.null(value) || length(value) == 0L) {
        return(list(NULL))
    }

    if (is.list(value) && !is.data.frame(value)) {
        return(unname(value))
    }

    unname(as.list(value))
}

.can_coerce_scalar <- function(value, type) {
    type <- tolower(type %||% "")
    if (!nzchar(type) || is.null(value) || (length(value) == 1L && is.na(value))) {
        return(TRUE)
    }

    if (length(value) != 1L || is.list(value)) {
        return(FALSE)
    }

    if (type == "character") {
        return(TRUE)
    }

    if (type == "integer") {
        if (is.integer(value)) {
            return(TRUE)
        }
        if (is.numeric(value)) {
            return(is.finite(value) && abs(value - round(value)) < .Machine$double.eps^0.5)
        }
        if (is.character(value)) {
            num <- suppressWarnings(as.numeric(trimws(value)))
            return(!is.na(num) && abs(num - round(num)) < .Machine$double.eps^0.5)
        }
        return(FALSE)
    }

    if (type == "numeric") {
        if (is.numeric(value)) {
            return(is.finite(value))
        }
        if (is.character(value)) {
            num <- suppressWarnings(as.numeric(trimws(value)))
            return(!is.na(num))
        }
        return(FALSE)
    }

    if (type == "logical") {
        if (is.logical(value)) {
            return(TRUE)
        }
        if (is.numeric(value)) {
            return(!is.na(value) && value %in% c(0, 1))
        }
        if (is.character(value)) {
            val <- trimws(tolower(value))
            return(val %in% c("true", "false", "t", "f", "yes", "y", "no", "n", "0", "1", "oui", "non"))
        }
        return(FALSE)
    }

    TRUE
}

.validate_parsed_against_specs <- function(value,
                                           key_specs = NULL,
                                           na_values = c("NA", "N/A", "null", "None", ""),
                                           .coerce_types = FALSE,
                                           coerce_when = NULL,
                                           i = NULL,
                                           verbose = FALSE) {
    if (is.null(key_specs) || !is.list(value) || is.null(names(value))) {
        return(list(value = value, meta = NULL))
    }

    out <- value
    meta_rows <- vector("list", 0L)

    for (key in intersect(names(key_specs), names(out))) {
        spec <- key_specs[[key]]
        expected_type <- spec$type %||% spec$allowed_type
        items <- .as_scalar_list(out[[key]])

        processed <- vector("list", length(items))
        key_type_ok <- TRUE
        key_allowed_ok <- TRUE

        for (idx in seq_along(items)) {
            raw_item <- items[[idx]]
            if (.is_na_like(raw_item, na_values) || (length(raw_item) == 1L && is.na(raw_item))) {
                processed[[idx]] <- NA
                next
            }

            coerced_item <- raw_item
            type_ok <- TRUE
            allowed_ok <- TRUE

            if (!is.null(expected_type)) {
                type_ok <- .can_coerce_scalar(raw_item, expected_type)
                if (isTRUE(type_ok) && (isTRUE(.coerce_types) || (is.function(coerce_when) && isTRUE(coerce_when(key, raw_item, spec, i))))) {
                    coerced_item <- .coerce_type(raw_item, expected_type)
                }
            }

            if (!is.null(spec$allowed)) {
                allowed_ok <- .in_allowed(coerced_item, spec$allowed)
            }

            if (!isTRUE(type_ok) || !isTRUE(allowed_ok)) {
                processed[[idx]] <- NA
                key_type_ok <- key_type_ok && isTRUE(type_ok)
                key_allowed_ok <- key_allowed_ok && isTRUE(allowed_ok)
            } else {
                processed[[idx]] <- coerced_item
            }
        }

        if (length(processed) == 1L) {
            out[[key]] <- processed[[1L]]
        } else if (is.atomic(out[[key]]) && !is.list(out[[key]])) {
            out[[key]] <- unlist(processed, recursive = FALSE, use.names = FALSE)
        } else {
            out[[key]] <- processed
        }

        if (!is.null(expected_type) || !is.null(spec$allowed)) {
            meta_rows[[length(meta_rows) + 1L]] <- data.frame(
                stage = "validate",
                key = key,
                value = jsonlite::toJSON(value[[key]], auto_unbox = TRUE, null = "null"),
                type_expected = expected_type %||% NA_character_,
                type_ok = if (!is.null(expected_type)) key_type_ok else NA,
                allowed_values = if (!is.null(spec$allowed)) paste(vapply(spec$allowed, as.character, character(1)), collapse = "|") else NA_character_,
                allowed = if (!is.null(spec$allowed)) key_allowed_ok else NA,
                action = if (isTRUE(key_type_ok) && isTRUE(key_allowed_ok)) "accepted" else "coerced_to_na",
                stringsAsFactors = FALSE
            )
        }
    }

    meta <- if (length(meta_rows)) do.call(rbind, meta_rows) else NULL
    if (isTRUE(verbose) && is.data.frame(meta) && nrow(meta)) {
        bad <- meta[(meta$type_ok %in% FALSE) | (meta$allowed %in% FALSE), , drop = FALSE]
        if (nrow(bad)) {
            message("Row ", if (is.null(i)) "?" else i, ": schema validation flagged ", nrow(bad), " field(s).")
        }
    }

    list(value = out, meta = meta)
}

.normalize_structured_provider <- function(provider, backend = NULL) {
    provider <- tolower(provider %||% "")
    if (provider %in% c("lmstudio", "ollama", "localai")) {
        return(provider)
    }
    if (provider == "local" && !is.null(backend) && nzchar(backend)) {
        return(tolower(backend))
    }
    provider
}

.supports_native_structured <- function(provider,
                                        backend = NULL,
                                        model = NULL,
                                        openai_api_key = Sys.getenv("OPENAI_API_KEY", ""),
                                        ssl_cert = NULL,
                                        allow_remote = getOption("gptr.allow_remote", FALSE)) {
    provider <- match.arg(provider, c("auto", "local", "openai", "lmstudio", "ollama", "localai"))
    configured <- tolower(getOption("gptr.native_structured_backends", character()))
    normalized <- .normalize_structured_provider(provider, backend)

    if (identical(normalized, "openai") || normalized %in% configured) {
        return(TRUE)
    }

    if (!identical(provider, "auto") || !is.character(model) || !nzchar(model)) {
        return(FALSE)
    }

    hits <- try(
        .resolve_model_provider(
            model,
            openai_api_key = if (isTRUE(allow_remote)) openai_api_key else "",
            ssl_cert = ssl_cert
        ),
        silent = TRUE
    )
    if (inherits(hits, "try-error") || !is.data.frame(hits) || nrow(hits) < 1L) {
        return(FALSE)
    }

    prefer_locals <- getOption("gptr.local_prefer", c("lmstudio", "ollama", "localai"))
    rank_fn <- function(p) {
        m <- match(p, c(prefer_locals, "openai"))
        ifelse(is.na(m), 999L, m)
    }
    ord <- order(rank_fn(tolower(as.character(hits$provider))))
    chosen <- tolower(as.character(hits$provider[ord[1L]]))
    identical(chosen, "openai") || chosen %in% configured
}

.prepare_extraction_request <- function(prompt,
                                        key_specs = NULL,
                                        structured = c("auto", "native", "repair"),
                                        provider = "auto",
                                        backend = NULL,
                                        model = NULL,
                                        base_url = NULL,
                                        openai_api_key = Sys.getenv("OPENAI_API_KEY", ""),
                                        ssl_cert = getOption("gptr.ssl_cert", NULL),
                                        keep_unexpected_keys = FALSE,
                                        base_args = list(),
                                        dots = list()) {
    structured <- match.arg(structured)
    allow_remote <- dots$allow_remote %||% getOption("gptr.allow_remote", FALSE)
    call_args <- c(base_args, list(prompt = prompt))
    call_args <- c(call_args, Filter(Negate(is.null), list(
        provider = provider,
        backend = backend,
        base_url = base_url,
        model = model
    )))

    if (!is.null(dots$response_format) && structured != "repair" && !is.null(key_specs)) {
        rlang::warn(
            "`gpt_column()` controls `response_format` when `structured != \"repair\"`; ignoring the user-supplied value.",
            .frequency = "once",
            .frequency_id = "gptr_gpt_column_response_format_ignored"
        )
        dots$response_format <- NULL
    }

    if (!is.null(key_specs) && structured != "repair") {
        can_native <- .supports_native_structured(
            provider = provider,
            backend = backend,
            model = model,
            openai_api_key = openai_api_key,
            ssl_cert = ssl_cert,
            allow_remote = allow_remote
        )

        if (isTRUE(can_native)) {
            dots$response_format <- .schema_to_response_format(
                key_specs,
                allow_additional = isTRUE(keep_unexpected_keys)
            )
            return(list(args = c(call_args, dots), mode = "native"))
        }

        if (identical(structured, "native")) {
            stop("Native structured extraction is not available for this provider/backend.", call. = FALSE)
        }
    }

    list(args = c(call_args, dots), mode = "repair")
}

.req_apply_ssl_cert <- function(req, ssl_cert = NULL) {
    if (!is.null(ssl_cert) && length(ssl_cert) && !is.na(ssl_cert[[1]]) && nzchar(ssl_cert[[1]])) {
        return(httr2::req_options(req, cainfo = ssl_cert[[1]]))
    }
    req
}

.httr_request <- function(url) {
    httr2::request(url)
}

.httr_req_timeout <- function(req, timeout) {
    httr2::req_timeout(req, timeout)
}

.httr_req_headers <- function(req, ...) {
    httr2::req_headers(req, ...)
}

.httr_req_retry <- function(req, ...) {
    httr2::req_retry(req, ...)
}

.httr_req_perform <- function(req) {
    httr2::req_perform(req)
}

.httr_resp_status <- function(resp) {
    httr2::resp_status(resp)
}

.httr_resp_body_json <- function(resp, simplifyVector = FALSE) {
    httr2::resp_body_json(resp, simplifyVector = simplifyVector)
}

.httr_resp_body_string <- function(resp) {
    httr2::resp_body_string(resp)
}

.httr_resp_body_raw <- function(resp) {
    httr2::resp_body_raw(resp)
}

.httr_resp_header <- function(resp, name) {
    httr2::resp_header(resp, name)
}
