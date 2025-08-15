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
#' @param prompt Character. Your user message for this turn. If `NULL`, the
#'   function only performs actions like `reset`, `show_history`, `load_path`, etc.
#' @param image_path Optional path to an image file to include (sent as a
#'   base64 data URL; MIME type auto-detected).
#' @param file_path Optional path to a text/PDF/Word file whose extracted text
#'   will be appended to the prompt. Very large files are truncated (~100k chars).
#' @param reset Logical. If `TRUE`, clears the in-memory history and returns.
#' @param show_history Logical. If `TRUE`, shows the current conversation history
#'   and returns (no API call).
#' @param show_history_as_text Logical. When showing history, print a readable
#'   transcript (`TRUE`) or return a tibble with `role`/`message` (`FALSE`).
#' @param model Character. Model id (e.g., `"mistralai/mistral-7b-instruct-v0.3"`,
#'   `"gpt-4o-mini"`).
#' @param base_url Character. Base URL to the API, default `"http://127.0.0.1:1234/v1"`.
#'   For OpenAI use `"https://api.openai.com/v1"`.
#' @param api_key Optional character. Bearer token for providers that require it
#'   (e.g., `Sys.getenv("OPENAI_API_KEY")`). Not needed for LM Studio.
#' @param temperature Numeric. Sampling temperature.
#' @param system Optional character. A system message to prime behavior. If
#'   provided and the history is empty, it is inserted once at the top.
#' @param trim_max_chars Optional integer. If set, oldest turns are dropped
#'   before sending so that the serialized message contents stay under this
#'   character budget (keeps the first system message).
#' @param save_path Optional file path. If provided, the updated history is
#'   written as pretty JSON after this call.
#' @param load_path Optional file path. If provided, history is loaded from this
#'   JSON file before processing this call.
#'
#' @return
#' - On a normal chat turn: **invisibly** returns the assistant's text reply
#'   (character scalar), and also prints it to the console.
#' - If `show_history = TRUE` and `show_history_as_text = TRUE`: prints a
#'   transcript and returns `NULL` (invisibly).
#' - If `show_history = TRUE` and `show_history_as_text = FALSE`: returns a
#'   tibble with columns `role` and `message`.
#' - On error (HTTP failure): returns `NULL` and emits a warning containing the
#'   server's response text.
#'
#' @details
#' The function keeps conversation state in a private closure variable (`history`).
#' Each call can add a new user message, send the full history to the Chat
#' Completions endpoint (`{base_url}/chat/completions`), and append the assistant
#' reply back into `history`. File inputs are converted to text (`pdftools`,
#' `officer`) and concatenated. Images are sent as `image_url` content blocks
#' using a base64 data URL. When `trim_max_chars` is set, the oldest messages
#' (except the first system message if present) are dropped until the budget is met.
#'
#' @section Dependencies:
#' `httr`, `jsonlite`, `purrr`, `tibble`, `base64enc`, `pdftools`, `officer`, `mime`, `stringi`.
#' The helper attempts to install missing packages interactively; in non-interactive
#' contexts it errors with a clear message.
#'
#' @section Compatibility:
#' - **LM Studio**: Works out of the box with the default `base_url` and no `api_key`.
#' - **OpenAI**: Set `base_url = "https://api.openai.com/v1"` and pass `api_key`.
#'   Make sure `model` is one available to your account.
#'
#' @examples
#' \dontrun{
#' # --- LM Studio, local chat ---
#' gpt_chat(reset = TRUE)
#' gpt_chat(system = "You are a concise medical data assistant.")
#' gpt_chat("Ciao! Facciamo un test?")
#' gpt_chat("Ricordati che lavoro in sanità pubblica.")
#' gpt_chat(show_history = TRUE)                   # print transcript
#' gpt_chat(show_history = TRUE, show_history_as_text = FALSE)  # tibble
#'
#' # With a PDF attached (text extracted and appended):
#' gpt_chat("Riassumi il documento.", file_path = "note.pdf")
#'
#' # Save conversation to disk:
#' gpt_chat("Ok, salva lo stato.", save_path = "chat_history.json")
#'
#' # Reload later and continue:
#' gpt_chat(load_path = "chat_history.json")
#' gpt_chat("Riprendiamo da dove eravamo rimasti.")
#'
#' # --- OpenAI (requires API key) ---
#' Sys.setenv(OPENAI_API_KEY = "sk-...")  # or pass api_key explicitly
#' gpt_chat(
#'   base_url = "https://api.openai.com/v1",
#'   api_key  = Sys.getenv("OPENAI_API_KEY"),
#'   model    = "gpt-4o-mini",
#'   system   = "You are terse and precise."
#' )
#' gpt_chat("Summarize mixed-effects models in 3 bullets.")
#' }
#'
#' @seealso
#' - OpenAI Chat Completions API.
#' - LM Studio API (OpenAI-compatible).
#'
#' @export


gpt_chat <- local({
    history <- list()

    .require_package <- function(pkg) {
        if (!requireNamespace(pkg, quietly = TRUE)) {
            if (!interactive()) stop("Missing required package: ", pkg, call. = FALSE)
            ans <- utils::askYesNo(sprintf("Package '%s' is required. Install now?", pkg), default = FALSE)
            if (isTRUE(ans)) utils::install.packages(pkg) else stop("Missing required package: ", pkg, call. = FALSE)
        }
    }

    purrr::walk(c("base64enc","pdftools","tibble","officer","purrr","mime","httr","jsonlite","stringi"), .require_package)

    # small helper
    .trim_history_chars <- function(hist, max_chars = 12000) {
        # keep the first system message if present; then drop oldest until under budget
        total <- sum(nchar(purrr::map_chr(hist, ~{
            x <- .x$content
            if (is.character(x)) x else paste(purrr::map_chr(x, ~ifelse(isTRUE(.x$type == "text"), .x$text, "")), collapse="\n")
        })), na.rm = TRUE)
        if (total <= max_chars) return(hist)

        keep_sys <- length(hist) > 0 && identical(hist[[1]]$role, "system")
        drop_from <- if (keep_sys) 2L else 1L

        i <- drop_from
        while (length(hist) >= drop_from && total > max_chars) {
            txt <- hist[[i]]$content
            chars <- if (is.character(txt)) nchar(txt) else nchar(paste(purrr::map_chr(txt, ~ifelse(isTRUE(.x$type=="text"), .x$text, "")), collapse="\n"))
            total <- total - chars
            hist <- hist[-i]
            if (i > length(hist)) break
        }
        hist
    }

    function(
        prompt = NULL,
        image_path = NULL,
        file_path = NULL,
        reset = FALSE,
        show_history = FALSE,
        show_history_as_text = TRUE,
        model = "mistralai/mistral-7b-instruct-v0.3",
        base_url = "http://127.0.0.1:1234/v1",
        api_key = NULL,                # for OpenAI-compatible remote servers, else NULL
        temperature = 0.2,
        system = NULL,                 # optional system prompt (set once when starting)
        trim_max_chars = NULL,         # e.g., 12000 to auto-trim before sending
        save_path = NULL,              # e.g., "chat_history.json" to save after this turn
        load_path = NULL               # e.g., "chat_history.json" to load before this turn
    ){
        if (reset) {
            history <<- list()
            cat("History reset.\n")
            return(invisible(NULL))
        }

        # Lazy-load existing history
        if (!is.null(load_path)) {
            if (!file.exists(load_path)) stop("History file not found: ", load_path)
            txt <- readLines(load_path, warn = FALSE)
            history <<- jsonlite::fromJSON(paste(txt, collapse = "\n"), simplifyVector = FALSE)
            cat("History loaded from ", load_path, ".\n", sep = "")
            if (show_history && is.null(prompt)) show_history <- TRUE  # allow immediate print
        }

        # Print history and bail if requested
        if (show_history) {
            if (length(history) == 0) {
                cat("No conversation history yet.\n")
                return(invisible(NULL))
            }
            if (show_history_as_text) {
                lines <- purrr::map_chr(history, function(x) {
                    role <- x$role
                    if (is.character(x$content)) {
                        msg <- x$content
                    } else {
                        texts <- purrr::map_chr(x$content, function(block) {
                            if (!is.null(block$type) && block$type == "text") block$text else "[non-text content]"
                        })
                        msg <- paste(texts, collapse = "\n")
                    }
                    paste0(toupper(role), ": ", msg)
                })
                cat(paste(lines, collapse = "\n\n"), "\n")
                return(invisible(NULL))
            } else {
                return(
                    tibble::tibble(
                        role = purrr::map_chr(history, "role"),
                        message = purrr::map_chr(history, function(x) {
                            if (is.character(x$content)) return(x$content)
                            texts <- purrr::map_chr(x$content, function(block) {
                                if (!is.null(block$type) && block$type == "text") block$text else "[non-text content]"
                            })
                            paste(texts, collapse = "\n")
                        })
                    )
                )
            }
        }

        if (is.null(prompt)) {
            warning("You must provide a prompt unless you're resetting, loading, or showing history.")
            return(invisible(NULL))
        }

        # Ensure system message exists once, at start
        if (!is.null(system) && (length(history) == 0 || history[[1]]$role != "system")) {
            history <<- append(history, list(list(role = "system", content = list(list(type="text", text = system)))))
        }

        # Handle file input (cap size to avoid blowing context)
        file_text <- NULL
        if (!is.null(file_path)) {
            if (!file.exists(file_path)) stop("File not found: ", file_path)
            ext <- tolower(tools::file_ext(file_path))
            if (ext %in% c("txt","md","csv","log")) {
                file_text <- paste(readLines(file_path, warn = FALSE), collapse = "\n")
            } else if (ext == "pdf") {
                file_text <- paste(pdftools::pdf_text(file_path), collapse = "\n")
            } else if (ext == "docx") {
                doc <- officer::read_docx(file_path)
                file_text <- officer::docx_summary(doc)$text |> na.omit() |> paste(collapse = "\n")
            } else {
                stop("Unsupported file type: ", ext)
            }
            # soft cap ~100k chars
            if (nchar(file_text) > 100000) {
                file_text <- substr(file_text, 1, 100000)
                file_text <- paste0(file_text, "\n\n[Truncated to 100k chars]")
            }
        }

        # Handle image input (detect MIME)
        image_block <- NULL
        if (!is.null(image_path)) {
            if (!file.exists(image_path)) stop("Image file not found: ", image_path)
            mime <- mime::guess_type(image_path)
            if (is.na(mime)) mime <- "image/png"
            image_b64 <- base64enc::base64encode(image_path)
            image_uri <- paste0("data:", mime, ";base64,", image_b64)
            image_block <- list(type = "image_url", image_url = list(url = image_uri))
        }

        # Build message content
        content_blocks <- list(list(type = "text", text = prompt))
        if (!is.null(file_text)) content_blocks <- append(content_blocks, list(list(type = "text", text = paste0("\n\n[File content]\n", file_text))))
        if (!is.null(image_block)) content_blocks <- append(content_blocks, list(image_block))

        # Update history with user message
        history <<- append(history, list(list(role = "user", content = content_blocks)))

        # Optional trimming before send
        if (!is.null(trim_max_chars) && is.finite(trim_max_chars)) {
            history <<- .trim_history_chars(history, max_chars = as.integer(trim_max_chars))
        }

        # Prepare request
        endpoint <- paste0(sub("/+$","", base_url), "/chat/completions")
        headers <- c("Content-Type" = "application/json")
        if (!is.null(api_key)) headers <- c(headers, Authorization = paste("Bearer", api_key))

        payload <- list(
            model = model,
            messages = history,
            temperature = temperature
        )

        response <- httr::POST(
            url = endpoint,
            httr::add_headers(.headers = headers),
            body = jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null"),
            encode = "json"
        )

        if (httr::status_code(response) != 200) {
            warning("Request failed: ", httr::content(response, "text", encoding = "UTF-8"))
            return(NULL)
        }

        parsed <- httr::content(response, "parsed", encoding = "UTF-8")
        reply_content <- parsed$choices[[1]]$message$content

        assistant_content <- if (is.character(reply_content)) {
            list(list(type = "text", text = reply_content))
        } else {
            reply_content
        }
        history <<- append(history, list(list(role = "assistant", content = assistant_content)))

        # Optional persistence after this turn
        if (!is.null(save_path)) {
            writeLines(jsonlite::toJSON(history, auto_unbox = TRUE, pretty = TRUE), save_path)
            cat("History saved to ", save_path, "\n", sep = "")
        }

        cat(if (is.character(reply_content)) reply_content else "[non-text response]", "\n")
        invisible(reply_content)
    }
})
