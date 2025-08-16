# R/providers_openai.R
# OpenAI provider (httr2) --------------------------------------------------

# Imports (DESCRIPTION):
# Imports: httr2, jsonlite
# Suggests: base64enc (only if you want to support data: URLs for images)


# -- Defaults ---------------------------------------------------------------

#' @keywords internal
.resolve_openai_defaults <- function(base_url = NULL, model = NULL, api_key = NULL) {
    list(
        base_url = base_url %||% getOption("gpt.openai_base_url", "https://api.openai.com/v1/chat/completions"),
        model    = model    %||% getOption("gpt.openai_model",    "gpt-4o-mini"),
        api_key  = api_key  %||% Sys.getenv("OPENAI_API_KEY", unset = "")
    )
}

# -- Message helpers --------------------------------------------------------

#' Build a Chat Completions message list (text, images, files)
#' @keywords internal
#' @param system Optional system prompt (character scalar).
#' @param user   User prompt (character scalar).
#' @param image_paths Optional character vector of image file paths. Each will be
#'   embedded as "type": "input_image" with a data URL (base64).
#' @param file_paths Optional character vector of file paths. Each will be embedded
#'   as "type": "input_file" with a data URL (base64). Requires `base64enc`.
#' @return A list suitable for OpenAI `messages`.
openai_make_messages <- function(system = NULL, user = NULL,
                                 image_paths = NULL, file_paths = NULL,
                                 on_missing = c("warn","skip","error")) {
    on_missing <- match.arg(on_missing)
    have_b64 <- requireNamespace("base64enc", quietly = TRUE)

    mime_guess <- function(p, kind = c("image","file")) {
        kind <- match.arg(kind)
        ext <- tolower(tools::file_ext(p))
        if (kind == "image") {
            switch(ext,
                   "png"="image/png","jpg"="image/jpeg","jpeg"="image/jpeg","webp"="image/webp",
                   "gif"="image/gif","bmp"="image/bmp","tif"="image/tiff","tiff"="image/tiff","image/*")
        } else {
            switch(ext,
                   "pdf"="application/pdf","txt"="text/plain","csv"="text/csv",
                   "docx"="application/vnd.openxmlformats-officedocument.wordprocessingml.document",
                   "xlsx"="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                   "json"="application/json","application/octet-stream")
        }
    }

    as_data_url <- function(path, mime) {
        b64 <- base64enc::base64encode(path)
        sprintf("data:%s;base64,%s", mime, b64)
    }

    add_or_handle_missing <- function(ok, what, path) {
        if (ok) return(invisible(TRUE))
        msg <- sprintf("%s not found: %s", what, path)
        switch(on_missing,
               warn  = { warning(msg, call. = FALSE); FALSE },
               skip  = FALSE,
               error = stop(msg, call. = FALSE)
        )
    }

    msgs <- list()
    if (!is.null(system) && nzchar(system)) {
        msgs[[length(msgs)+1]] <- list(role = "system", content = system)
    }

    content <- list()
    if (!is.null(user) && nzchar(user)) {
        content[[length(content)+1]] <- list(type = "text", text = user)
    }

    # Accept URLs (http, https, data) directly
    is_url <- function(x) grepl("^(https?://|data:)", x, ignore.case = TRUE)

    # Images
    if (length(image_paths)) {
        for (p in image_paths) {
            if (is_url(p)) {
                content[[length(content)+1]] <- list(type="input_image", image_url=list(url=p))
                next
            }
            if (!file.exists(p)) { add_or_handle_missing(FALSE, "Image file", p); next }
            if (!have_b64)       { add_or_handle_missing(FALSE, "base64enc missing for image", p); next }
            mime <- mime_guess(p, "image")
            url  <- as_data_url(p, mime)
            content[[length(content)+1]] <- list(type="input_image", image_url=list(url=url))
        }
    }

    # Files
    if (length(file_paths)) {
        for (p in file_paths) {
            if (is_url(p)) {
                content[[length(content)+1]] <- list(type="input_file", file_url=list(url=p))
                next
            }
            if (!file.exists(p)) { add_or_handle_missing(FALSE, "File", p); next }
            if (!have_b64)       { add_or_handle_missing(FALSE, "base64enc missing for file", p); next }
            mime <- mime_guess(p, "file")
            url  <- as_data_url(p, mime)
            content[[length(content)+1]] <- list(type="input_file", file_url=list(url=url))
        }
    }

    if (length(content)) {
        msgs[[length(msgs)+1]] <- list(role = "user", content = content)
    }
    msgs
}


# -- Payload composition ----------------------------------------------------

#' Compose OpenAI Chat Completions payload
#' @keywords internal
#' @param messages list created by `openai_make_messages()` or equivalent
#' @param model character model id
#' @param temperature numeric scalar
#' @param seed optional integer for determinism (when supported)
#' @param response_format one of NULL, "json_object", or a named list matching the
#'   OpenAI API (e.g., list(type = "json_schema", json_schema = ...))
#' @param tools tool spec list (optional, pass-through)
#' @param top_p,max_tokens,frequency_penalty,presence_penalty optional tuning params
#' @param extra named list of extra params passed through as-is
openai_compose_payload <- function(
        messages,
        model,
        temperature = 0.2,
        seed = NULL,
        response_format = NULL,
        tools = NULL,
        top_p = NULL,
        max_tokens = NULL,
        frequency_penalty = NULL,
        presence_penalty = NULL,
        extra = NULL
) {
    payload <- list(
        model = model,
        messages = messages,
        temperature = temperature
    )
    if (!is.null(seed))              payload$seed               <- as.integer(seed)
    if (!is.null(top_p))             payload$top_p              <- top_p
    if (!is.null(max_tokens))        payload$max_tokens         <- as.integer(max_tokens)
    if (!is.null(frequency_penalty)) payload$frequency_penalty  <- frequency_penalty
    if (!is.null(presence_penalty))  payload$presence_penalty   <- presence_penalty
    if (!is.null(tools))             payload$tools              <- tools

    # JSON mode shorthand
    if (is.character(response_format) && identical(response_format, "json_object")) {
        payload$response_format <- list(type = "json_object")
    } else if (is.list(response_format)) {
        payload$response_format <- response_format
    }

    # Pass-through any additional fields (e.g., logprobs, stop, n, metadata)
    if (is.list(extra) && length(extra)) {
        for (nm in names(extra)) payload[[nm]] <- extra[[nm]]
    }

    payload
}

# -- Request/Response -------------------------------------------------------

#' Perform a Chat Completions request to OpenAI (httr2)
#' @keywords internal
#' @param payload list as created by `openai_compose_payload()`
#' @param base_url chat completions endpoint (default resolved by options)
#' @param api_key OpenAI API key (default: Sys.getenv("OPENAI_API_KEY"))
#' @param timeout seconds (numeric)
#' @return list with `body`, `resp` (httr2 response). Use `openai_parse_text()` to get text.
request_openai <- function(payload,
                           base_url = NULL,
                           api_key = NULL,
                           timeout = getOption("gpt.timeout", 30)) {
    defs <- .resolve_openai_defaults(base_url = base_url, api_key = api_key)
    if (!nzchar(defs$api_key)) {
        stop("OpenAI API key is missing. Set OPENAI_API_KEY or pass `api_key=`.", call. = FALSE)
    }

    ua <- sprintf("gptr/%s (+openai)", tryCatch(as.character(utils::packageVersion("gptr")), error = function(e) "0.0.0"))

    req <- httr2::request(defs$base_url) |>
        httr2::req_user_agent(ua) |>
        httr2::req_timeout(seconds = timeout) |>
        httr2::req_headers(Authorization = paste("Bearer", defs$api_key)) |>
        httr2::req_body_json(payload, auto_unbox = TRUE) |>
        httr2::req_retry(
            max_tries = 4,
            backoff = function(attempt) runif(1, 0.3, 1.2),
            is_transient = function(resp) {
                sc <- try(httr2::resp_status(resp), silent = TRUE)
                if (inherits(sc, "try-error")) return(TRUE)
                sc %in% c(408, 409, 429, 500, 502, 503, 504)
            }
        )


    resp <- httr2::req_perform(req)
    try(httr2::resp_check_status(resp), silent = TRUE)

    body_txt <- httr2::resp_body_string(resp)
    body <- tryCatch(jsonlite::fromJSON(body_txt, simplifyVector = FALSE), error = function(e) NULL)

    if (httr2::resp_status(resp) >= 400) {
        msg <- "OpenAI request failed"
        if (is.list(body) && !is.null(body$error)) {
            em <- body$error$message; et <- body$error$type; ec <- body$error$code
            msg <- paste(na.omit(c(em, if (!is.null(et)) paste0("type=", et), if (!is.null(ec)) paste0("code=", ec))), collapse = " | ")
        }
        stop(msg, call. = FALSE)
    }

    list(body = body, resp = resp)
}

#' Extract assistant text and metadata from OpenAI Chat response
#' @keywords internal
#' @param body response body (list) from `request_openai()$body`
#' @return list(text, finish_reason, usage, content_parts, raw)
openai_parse_text <- function(body) {
    if (is.null(body) || is.null(body$choices) || !length(body$choices)) {
        return(list(text = "", finish_reason = NA_character_, usage = NULL, content_parts = NULL, raw = body))
    }

    choice <- body$choices[[1]]
    finish_reason <- choice$finish_reason %||% NA_character_

    # Newer API: message$content is a list of content parts
    parts <- choice$message$content %||% list()
    text_chunks <- character()
    if (length(parts)) {
        for (p in parts) {
            if (is.list(p) && identical(p$type, "text")) {
                text_chunks <- c(text_chunks, p$text %||% "")
            }
        }
    }
    # Fallback to legacy message$content as single string
    if (!length(text_chunks)) {
        text_chunks <- choice$message$content %||% ""
        if (is.list(text_chunks)) text_chunks <- "" # unexpected shape; keep safe
    }

    usage <- body$usage %||% NULL

    list(
        text = paste(text_chunks, collapse = ""),
        finish_reason = finish_reason,
        usage = usage,
        content_parts = parts,
        raw = body
    )
}

# -- High-level convenience (optional) -------------------------------------

#' One-shot OpenAI chat call (convenience wrapper)
#' @keywords internal
#' @param prompt user text
#' @param system optional system prompt
#' @param image_paths optional character vector of image file paths
#' @param model,temperature,seed,response_format,... forwarded to compose/payload
#' @return list(text, usage, finish_reason, raw)
openai_chat_once <- function(
        prompt,
        system = NULL,
        image_paths = NULL,
        model = NULL,
        temperature = 0.2,
        seed = NULL,
        response_format = NULL,
        ...
) {
    defs <- .resolve_openai_defaults(model = model)
    msgs <- openai_make_messages(system = system, user = prompt, image_paths = image_paths)
    payload <- openai_compose_payload(
        messages = msgs,
        model = defs$model,
        temperature = temperature,
        seed = seed,
        response_format = response_format,
        extra = list(...)
    )
    res <- request_openai(payload)
    openai_parse_text(res$body)
}
