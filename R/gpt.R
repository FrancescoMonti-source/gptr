#' Chat with an LLM (Local or OpenAI), with optional file/image ingestion
#'
#' `gpt()` sends a Chat Completions–style request to either a local
#' OpenAI-compatible API (e.g., LM Studio, Ollama, LocalAI) or the OpenAI API.
#' You can pass a simple character prompt or a full `messages` list.
#' See Details for message formats and options. The function
#' returns the assistant's text; if the API replies with multiple content parts,
#' their `text` fields are concatenated.
#'
#' @section What you can pass as `prompt`:
#' You may provide either:
#'
#' - A character prompt (most common). If `file_path` and/or `image_path` are
#'   supplied, they are appended to the same user message as extra content parts.
#' - A prebuilt `messages` list (advanced). Each item is a list with `role`
#'   and `content`. `content` may be a single string or a list of parts, e.g.:
#'
#'   ```r
#'   list(
#'     list(role = "system", content = "You are concise."),
#'     list(role = "user", content = list(
#'       list(type = "text", text = "Caption this image"),
#'       list(type = "image_url", image_url = list(url = "data:image/png;base64,<...>"))
#'     ))
#'   )
#'   ```
#'
#'   Supported content parts include:
#'
#'   - `list(type = "text", text = "<string>")`
#'   - `list(type = "image_url", image_url = list(url = "<https://... or data:...>"))`
#'
#' @section File and image ingestion (helpers):
#' When `prompt` is a character string:
#'
#' - `file_path` (optional): reads `.txt`, `.md`, `.csv`, `.log`, `.pdf`, `.docx`
#'   and appends the text as a `type = "text"` part to the same user message.
#' - `image_path` (optional): loads a local image and appends a
#'   `type = "image_url"` part using a base64 `data:` URI.
#'
#' These are ignored when you pass a prebuilt `messages` list.
#'
#' @section Extra API parameters via `...`:
#' Any valid Chat Completions fields can be forwarded. Common ones:
#'
#' - `max_tokens` (integer): cap completion length.
#' - `stop` (character vector): stop sequences.
#' - `top_p` (numeric), `presence_penalty` / `frequency_penalty` (numeric).
#' - `logit_bias` (named numeric vector).
#' - `response_format` (list) to force JSON / JSON Schema, for example:
#'
#'   ```r
#'   response_format = list(
#'     type = "json_schema",
#'     json_schema = list(
#'       name = "result",
#'       schema = list(
#'         type = "object",
#'         properties = list(ok = list(type = "boolean")),
#'         required = list("ok"),
#'         additionalProperties = FALSE
#'       )
#'     )
#'   )
#'   ```
#'
#' - Tool calling: supply `tools = list(...)` and optionally `tool_choice`.
#' - Streaming: `stream = TRUE` (note: this wrapper does not yet expose a stream handler).
#'
#' @section Providers:
#' - OpenAI: requires `openai_api_key` (or env `OPENAI_API_KEY`). Uses
#'   `https://api.openai.com/v1/chat/completions`. Supports system messages, JSON
#'   formats, tools, and (if the chosen model supports it) images.
#' - LM Studio: local server (default `http://127.0.0.1:1234/v1/chat/completions`),
#'   no key. Mirrors the OpenAI schema; feature support depends on the local model/runtime.
#'
#' @param prompt Character (single string) or a prebuilt `messages` list (see above).
#' @param model Character. When `NULL`, a default is chosen per provider:
#'   LM Studio: `"mistralai/mistral-7b-instruct-v0.3"`;
#'   OpenAI: option `gptcolumnr.openai_model` (default `"gpt-4o-mini"`).
#' @param temperature Numeric sampling temperature.
#' @param provider One of `"lmstudio"` or `"openai"`.
#' @param base_url Endpoint URL. If `NULL`, provider-specific defaults are used.
#' @param openai_api_key Character. If omitted, falls back to env `OPENAI_API_KEY`
#'   (or legacy `OPENAI_openai_api_key`).
#' @param image_path,file_path Optional local paths; see "File and image ingestion".
#' @param system Optional system prompt (prepended when `prompt` is a character;
#'   ignored if you pass a full `messages` list).
#' @param seed Optional deterministic seed (if supported by the backend).
#' @param ... Extra fields forwarded to the API body (see "Extra API parameters").
#'
#' @return Character scalar containing the assistant's text. If the API returns
#' a list of `content` parts, their `text` fields are concatenated. Returns
#' `NA_character_` if no content is available.
#'
#' @examples
#' \dontrun{
#' # Basic
#' gpt("Say hello", provider = "openai", model = "gpt-4o-mini")
#'
#' # With a system message and token cap
#' gpt(
#'   "Summarize this in one sentence:\n<text>",
#'   provider = "openai",
#'   system = "You are a precise summarizer.",
#'   max_tokens = 100
#' )
#'
#' # Prebuilt multimodal messages
#' msgs <- list(
#'   list(role = "system", content = "You are terse."),
#''  list(role = "user", content = list(
#'     list(type = "text", text = "Caption this image"),
#'     list(type = "image_url", image_url = list(url = "data:image/png;base64,<...>"))
#'   ))
#' )
#' gpt(msgs, provider = "openai", model = "gpt-4o-mini")
#'
#' # Forcing JSON with a schema
#' schema <- list(
#'   type = "object",
#'   properties = list(ok = list(type = "boolean")),
#'   required = list("ok"),
#'   additionalProperties = FALSE
#' )
#' gpt(
#'   "Return {\"ok\": true|false} after checking: fever >= 38C?",
#'   provider = "openai",
#'   response_format = list(
#'     type = "json_schema",
#'     json_schema = list(name = "result", schema = schema)
#'   )
#' )
#'
#' # LM Studio (local): ensure the server is running and the port matches.
#' options(gptcolumnr.provider = "lmstudio")
#' gpt("hi")
#' }
#'
#' @seealso \code{\link{gpt_messages}} for a focused help topic on message formats.
#' @export



gpt <- function(
        prompt,
        model = NULL,
        temperature = 0.2,
        provider = c("local", "openai"),
        base_url = NULL,
        openai_api_key = Sys.getenv("OPENAI_API_KEY", Sys.getenv("OPENAI_openai_api_key", "")),
        image_path = NULL,
        file_path = NULL,
        system = NULL,
        seed = NULL,
        ...
) {
    provider <- match.arg(provider)

    ## ---------- choose sensible model/base_url if not supplied ----------
    if (provider == "local") {
        # If user gave a base_url, use it; else autodetect
        if (is.null(base_url) || base_url == "") {
            hit <- .detect_local_backend()
            if (!is.null(hit)) {
                base_url <- hit$base_url
                # Optional: message about which backend we picked
                if (isTRUE(getOption("gpt.local_verbose", FALSE))) {
                    cli::cli_inform("Using local backend: {hit$name} ({hit$base_url})")
                }
            } else {
                # Fallback to LM Studio defaults if nothing detected
                base_url <- getOption("gpt.local_base_url",
                                      "http://127.0.0.1:1234/v1/chat/completions")
            }
        }
        if (is.null(model) || model == "") {
            # Generic local default; user can set these to point to their favorite model
            model <- getOption("gpt.local_model",
                               getOption("gpt.lmstudio_model",
                                         "mistralai/mistral-7b-instruct-v0.3"))
        }
    } else if (provider == "openai") {
        if (is.null(base_url) || base_url == "") {
            base_url <- "https://api.openai.com/v1/chat/completions"
        }
        if (is.null(model) || model == "") {
            model <- getOption("gpt.openai_model", "gpt-4o-mini")
        }
    }

    ## ---------- helper for required packages ----------
    .require_package <- function(pkg) {
        if (!requireNamespace(pkg, quietly = TRUE))
            stop("Package ‘", pkg, "’ is required but not installed.")
    }
    purrr::walk(c("base64enc", "pdftools", "officer", "httr", "jsonlite", "stringr"), .require_package)

    # Ensure a valid temp directory (Windows can purge %TEMP% while R is open)
    td <- tempdir()
    if (!dir.exists(td)) {
        dir.create(td, recursive = TRUE, showWarnings = FALSE)
    }

    ## ---------- optional file ingestion ----------
    file_text <- NULL
    if (!is.null(file_path)) {
        if (!file.exists(file_path)) stop("File not found: ", file_path)
        ext <- tolower(tools::file_ext(file_path))
        file_text <- switch(
            ext,
            txt  = paste(readLines(file_path, warn = FALSE), collapse = "\n"),
            md   = paste(readLines(file_path, warn = FALSE), collapse = "\n"),
            csv  = paste(readLines(file_path, warn = FALSE), collapse = "\n"),
            log  = paste(readLines(file_path, warn = FALSE), collapse = "\n"),
            pdf  = paste(pdftools::pdf_text(file_path), collapse = "\n"),
            docx = {
                doc <- officer::read_docx(file_path)
                paste(stats::na.omit(officer::docx_summary(doc)$text), collapse = "\n")
            },
            stop("Unsupported file type: ", ext)
        )
    }

    ## ---------- optional image ingestion ----------
    image_block <- NULL
    if (!is.null(image_path)) {
        if (!file.exists(image_path)) stop("Image not found: ", image_path)
        image_b64  <- base64enc::base64encode(image_path)
        image_uri  <- paste0("data:image/png;base64,", image_b64)
        image_block <- list(type = "image_url", image_url = list(url = image_uri))
    }

    ## ---------- construct messages ----------
    if (is.character(prompt)) {
        content_blocks <- list(list(type = "text", text = prompt))
        if (!is.null(file_text))
            content_blocks <- append(content_blocks, list(list(type = "text",
                                                               text = paste0("\n\nContenu du fichier :\n", file_text))))
        if (!is.null(image_block))
            content_blocks <- append(content_blocks, list(image_block))

        messages <- list()
        if (!is.null(system)) {
            messages <- append(messages, list(list(role = "system", content = system)))
        }
        messages <- append(messages, list(list(role = "user", content = content_blocks)))

    } else {
        messages <- prompt # assume already a proper messages list
    }

    ## ---------- choose headers ----------
    if (provider == "local") {
        headers <- httr::add_headers(`Content-Type` = "application/json")
    } else { # provider == "openai"
        if (openai_api_key == "")
            stop("OPENAI_API_KEY missing; set Sys.setenv(OPENAI_API_KEY = 'sk-…') or pass openai_api_key.")
        headers <- httr::add_headers(
            Authorization  = paste("Bearer", openai_api_key),
            `Content-Type` = "application/json"
        )
    }

    ## ---------- POST request ----------
    response <- httr::POST(
        url   = base_url,
        headers,
        body  = jsonlite::toJSON(
            c(list(model = model, messages = messages, temperature = temperature),
              if (!is.null(seed)) list(seed = seed),
              list(...)),
            auto_unbox = TRUE
        ),
        encode = "json"
    )

    ## ---------- handle response ----------
    if (httr::status_code(response) != 200) {
        warning("Request failed: ", httr::content(response, "text", encoding = "UTF-8"))
        return(NA_character_)
    }

    parsed <- httr::content(response, "parsed", encoding = "UTF-8")
    # Concatenate all text parts if multimodal
    content_parts <- parsed$choices[[1]]$message$content
    if (is.character(content_parts)) {
        return(stringr::str_trim(paste(content_parts, collapse = "\n")))
    } else if (is.list(content_parts)) {
        texts <- vapply(content_parts, function(p) if (!is.null(p$text)) as.character(p$text) else "", character(1))
        return(stringr::str_trim(paste(texts, collapse = "\n")))
    } else {
        return(NA_character_)
    }
}
