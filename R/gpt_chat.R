#' Stateful chat with OpenAI-compatible servers (LM Studio, OpenAI, etc.)
#'
#' `gpt_chat()` provides a persistent, multi-turn chat interface that keeps
#' conversation history in memory and (optionally) on disk. It speaks the
#' OpenAI Chat Completions API, so it works with local servers like **LM Studio**
#' as well as hosted providers such as **OpenAI** (set `api_key`).
#'
#' @section What it does:
#' - Maintains **conversation memory** across turns (system/user/assistant roles).
#' - Accepts **file** (`.txt/.md/.csv/.log`, `.pdf`, `.docx`) and **image**
#'   (base64 data URL) inputs alongside text prompts.
#' - Can **trim context** to a character budget for small-context models.
#' - Can **save**/ **load** history to/from JSON for persistence.
#'
#' @param prompt Character scalar, the new user input. Required unless showing
#'   or resetting history.
#' @param model Character scalar. Model identifier to use (provider-specific).
#' @param temperature Numeric. Sampling temperature (default 0.2).
#' @param provider One of `"auto"`, `"lmstudio"`, `"openai"`, `"ollama"`,
#'   `"local"`, `"localai"`. Default `"auto"`.
#' @param base_url Override base URL for the provider (optional).
#' @param openai_api_key API key for OpenAI (default reads from env var).
#' @param save_path File path. If given, conversation history is saved after
#'   each turn as pretty-printed JSON.
#' @param load_path File path. If given, conversation history is loaded from
#'   JSON on disk before proceeding.
#' @param trim_max_chars Integer. If given, trims history to fit within this
#'   approximate character budget.
#' @param ... Passed to [gpt()].
#'
#' @return Invisibly returns the model reply (character scalar) for normal turns.
#' @details
#' **Available methods** (call with `gpt_chat$method(...)`):
#' - `show_history(as_text = TRUE)` – Print (or return tibble when `FALSE`).
#' - `reset()` – Clear the in-memory history.
#' - `get_history()` – Return the raw list of messages.
#' - `save(path)` / `load(path)` – Persist/restore history as JSON.
#' - `replace_history(x)` – Overwrite history with `x` (list of messages).
#' - `summarise(model=..., provider=..., base_url=..., temperature=..., replace=TRUE, prefix=\"Chat summary : \")`
#'    – Summarise the current history via `gpt()`; by default replaces history with the summary message.
#' @export
#'
#' @examples
#' \dontrun{
#' # Normal usage (callable):
#' gpt_chat("Hello, who are you?")
#'
#' # Methods:
#' gpt_chat$show_history()
#' gpt_chat$reset()
#'
#' # Save/load:
#' gpt_chat$save("chat.json")
#' gpt_chat$load("chat.json")
#' gpt_chat$show_history()
#' }
gpt_chat <- local({
    history <- list()

    # --- helpers bound in a private env; exposed via `$` S3 method ---
    methods_env <- new.env(parent = emptyenv())

    # helper: flatten current history to plain text
    .flatten_history <- function(hist) {
        paste(
            vapply(hist, function(x) {
                role <- toupper(x$role)
                if (is.character(x$content)) {
                    paste0(role, ": ", x$content)
                } else {
                    blocks <- vapply(
                        x$content,
                        function(b) if (isTRUE(b$type == "text")) b$text else "[non-text content]",
                        FUN.VALUE = ""
                    )
                    paste0(role, ": ", paste(blocks, collapse = "\n"))
                }
            }, ""),
            collapse = "\n\n"
        )
    }

    methods_env$summarise <- function(
        model = "mistralai/mistral-7b-instruct-v0.3",
        provider = c("auto","lmstudio","openai","ollama","local","localai"),
        base_url = NULL,
        temperature = 0.3,
        openai_api_key = Sys.getenv("OPENAI_API_KEY", ""),
        replace = TRUE,
        prefix = "Chat summary : ",
        ...
    ) {
        provider <- match.arg(provider)
        if (length(history) < 2L) {
            message("Not enough history to summarize.")
            return(invisible(NULL))
        }

        history_text <- .flatten_history(history)

        prompt <- paste0(
            "Here's a chat history :\n\n", history_text,
            "\n\nPlease summarise it concisely and accurately. ",
            "Stick to the original language whatever it is."
        )

        # use gpt() so all your provider resolution, fallbacks, etc. apply
        reply <- tryCatch(
            gpt(
                prompt        = prompt,
                model         = model,
                temperature   = temperature,
                provider      = provider,
                base_url      = base_url,
                openai_api_key = openai_api_key,
                ...
            ),
            error = function(e) {
                warning("Summarization failed: ", conditionMessage(e))
                return(NULL)
            }
        )

        if (is.null(reply) || !nzchar(reply)) {
            warning("Summarization produced no text.")
            return(invisible(NULL))
        }

        if (isTRUE(replace)) {
            history <<- list(list(role = "user", content = paste0(prefix, reply)))
            message("Summary injected into chat history.")
        } else {
            history <<- append(history, list(list(role = "assistant", content = paste0(prefix, reply))))
        }

        invisible(reply)
    }

    # add inside the local({...}) where methods_env is defined:
    methods_env$replace_history <- function(x) {
        stopifnot(is.list(x))
        history <<- x
        invisible(NULL)
    }

    methods_env$show_history <- function(as_text = TRUE) {
        if (length(history) == 0) {
            message("No conversation history yet.")
            return(invisible(NULL))
        }
        if (as_text) {
            cat(paste(vapply(history, function(msg) {
                paste0(toupper(msg$role), ": ",
                       if (is.character(msg$content)) msg$content else
                           paste(vapply(msg$content, function(b) {
                               if (isTRUE(b$type == "text")) b$text else "[non-text content]"
                           }, ""), collapse = "\n"))
            }, ""), collapse = "\n\n"), "\n")
            invisible(NULL)
        } else {
            tibble::tibble(
                role    = vapply(history, `[[`, "", "role"),
                message = vapply(history, function(x) {
                    if (is.character(x$content)) x$content else ""
                }, "")
            )
        }
    }

    methods_env$reset <- function() {
        history <<- list()
        invisible(NULL)
    }

    methods_env$get_history <- function() {
        history
    }

    methods_env$save <- function(path) {
        writeLines(jsonlite::toJSON(history, auto_unbox = TRUE, pretty = TRUE), path)
        invisible(path)
    }

    methods_env$load <- function(path) {
        history <<- jsonlite::fromJSON(readLines(path), simplifyVector = FALSE)
        invisible(path)
    }

    # --- the callable function ---
    fun <- function(prompt = NULL,
                    model = NULL,
                    temperature = 0.2,
                    provider = c("auto","lmstudio","openai","ollama","local","localai"),
                    base_url = NULL,
                    openai_api_key = Sys.getenv("OPENAI_API_KEY", ""),
                    save_path = NULL,
                    load_path = NULL,
                    trim_max_chars = NULL,
                    ...) {

        # load from disk (if requested this turn)
        if (!is.null(load_path)) {
            history <<- jsonlite::fromJSON(readLines(load_path), simplifyVector = FALSE)
            message("History loaded from ", load_path)
        }

        if (is.null(prompt)) stop("prompt must be provided (or use gpt_chat$show_history / gpt_chat$reset)")

        # append user message
        history <<- append(history, list(list(role = "user", content = prompt)))

        # optionally trim by a char budget (rough)
        if (!is.null(trim_max_chars)) {
            chars <- 0
            i <- length(history)
            while (i >= 1 && chars < trim_max_chars) {
                msg <- history[[i]]$content
                chars <- chars + nchar(if (is.character(msg)) msg else "")
                i <- i - 1
            }
            if (i >= 1) history <<- history[(i+1):length(history)]
        }

        # build combined prompt
        combined <- paste(vapply(history, function(m) {
            paste0(toupper(m$role), ": ",
                   if (is.character(m$content)) m$content else "")
        }, ""), collapse = "\n\n")

        # call gpt() and handle errors
        reply <- tryCatch(
            gpt(prompt       = combined,
                model        = model,
                temperature  = temperature,
                provider     = provider,
                base_url     = base_url,
                openai_api_key = openai_api_key,
                ...),
            error = function(e) {
                warning("Chat failed: ", conditionMessage(e))
                return(NULL)
            }
        )

        if (!is.null(reply)) {
            history <<- append(history, list(list(role = "assistant", content = reply)))
            if (!is.null(save_path)) {
                writeLines(jsonlite::toJSON(history, auto_unbox = TRUE, pretty = TRUE), save_path)
            }
            cat(reply, "\n")
            return(invisible(reply))
        } else {
            return(NULL)
        }
    }

    # attach methods env + class so `$` can be overridden
    attr(fun, "gpt_chat_methods") <- methods_env
    class(fun) <- c("gpt_chat_callable", class(fun))
    fun
})

#' @export
#' @rdname gpt_chat
#' @usage gpt_chat$<method>
#' @details
#' **Available methods** (call with `gpt_chat$method(...)`):
#' - `show_history(as_text = TRUE)`
#'   Print the conversation (or return a tibble when `as_text = FALSE`).
#' - `reset()`
#'   Clear the in-memory history.
#' - `get_history()`
#'   Return the raw list of message objects.
#' - `save(path)` / `load(path)`
#'   Persist or restore the history as JSON.
`$.gpt_chat_callable` <- function(x, name) {
    env <- attr(x, "gpt_chat_methods", exact = TRUE)
    if (!is.environment(env)) stop("No methods environment on this gpt_chat object.")
    get(name, envir = env, inherits = FALSE)
}
