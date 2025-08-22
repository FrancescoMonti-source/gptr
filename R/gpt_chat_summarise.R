#' Summarise the current chat history and replace it with a single summary message
#'
#' Uses the chat completions endpoint of the selected backend.
#'
#' @param model Character. Model id (e.g. "mistralai/mistral-7b-instruct-v0.3").
#' @param base_url Character. Either the root (e.g. "http://127.0.0.1:1234")
#'   or the full endpoint URL "…/v1/chat/completions". Both are accepted.
#' @param temperature Numeric scalar. Sampling temperature.
#' @param timeout Numeric seconds for the HTTP request (default from option
#'   \code{gptr.request_timeout}, falling back to 10).
#' @return Invisibly \code{NULL}. On success, replaces the chat history env
#'   with a single user message containing the summary.
#' @noRd
gpt_chat_summarise <- function(
        model = "mistralai/mistral-7b-instruct-v0.3",
        base_url = "http://127.0.0.1:1234",
        temperature = 0.3,
        timeout = getOption("gptr.request_timeout", 10)
) {
    if (!exists("gpt_chat", mode = "function", inherits = TRUE)) {
        stop("gpt_chat() must be initialized first.")
    }

    # Access the chat environment created by gpt_chat()
    env <- environment(gpt_chat)
    if (!exists("history", envir = env, inherits = FALSE)) {
        stop("No chat history to summarize.")
    }
    history <- get("history", envir = env)

    if (length(history) < 2L) {
        message("Not enough history to summarize.")
        return(invisible(NULL))
    }

    # Format history to plain text (base R, no purrr)
    history_text <- paste(
        vapply(history, function(x) {
            role <- toupper(x$role)
            if (is.character(x$content)) {
                paste0(role, ": ", x$content)
            } else {
                texts <- vapply(x$content, function(b) {
                    if (isTRUE(b$type == "text")) b$text else "[image]"
                }, character(1))
                paste0(role, ": ", paste(texts, collapse = "\n"))
            }
        }, character(1)),
        collapse = "\n\n"
    )

    messages <- list(
        list(
            role = "user",
            content = paste0(
                "Here's a chat history :\n\n", history_text,
                "\n\nPlease summarise it concisely and accurately. ",
                "Stick to the original language whatever it is."
            )
        )
    )

    # Accept either root or full endpoint
    url <-
        if (grepl("/v1/", base_url, fixed = TRUE)) {
            sub("/+$", "", base_url)
        } else {
            paste0(sub("/+$", "", base_url), "/v1/chat/completions")
        }

    # Build and perform request via wrappers (no direct httr2 calls here)
    req <- .http_request(url) |>
        .http_req_headers(`Content-Type` = "application/json") |>
        .http_req_timeout(timeout) |>
        .http_req_retry(max_tries = 3, backoff = function(i) 0.2 * i) |>
        .http_req_body_json(list(
            model = model,
            messages = messages,
            temperature = temperature
        ))

    resp <- try(.http_req_perform(req), silent = TRUE)
    if (inherits(resp, "try-error")) {
        warning("Summarization failed: network unreachable.")
        return(invisible(NULL))
    }

    sc <- .http_resp_status(resp)
    if (!identical(sc, 200L)) {
        # Safe plain-text body fetch
        body <- try(.http_resp_body_json(resp, simplifyVector = FALSE), silent = TRUE)
        msg <- if (!inherits(body, "try-error")) {
            # best-effort extraction
            tryCatch(
                as.character(body$error$message %||% body$message %||% "HTTP error"),
                error = function(e) "HTTP error"
            )
        } else {
            "HTTP error"
        }
        warning("Summarization failed (http_", sc, "): ", msg)
        return(invisible(NULL))
    }

    j <- try(.http_resp_body_json(resp, simplifyVector = FALSE), silent = TRUE)
    if (inherits(j, "try-error")) {
        warning("Summarization failed: non-JSON response.")
        return(invisible(NULL))
    }

    summary_text <- tryCatch(
        j$choices[[1]]$message$content,
        error = function(e) NULL
    )
    if (!nzchar(summary_text %||% "")) {
        warning("Summarization produced no text.")
        return(invisible(NULL))
    }

    # Replace history with a single user summary message
    summary_message <- list(role = "user", content = paste("Chat summary :", summary_text))
    assign("history", list(summary_message), envir = env)
    message("Summary injected into chat history.")
    invisible(NULL)
}
