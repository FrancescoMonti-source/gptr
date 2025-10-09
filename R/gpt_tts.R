#' Text-to-Speech with OpenAI (Audio Formats)
#'
#' Synthesize spoken audio from plain text by calling the OpenAI Text-to-Speech
#' endpoint. The helper wraps `openai_request_non_chat_services()` and stores the
#' binary response as an audio file on disk.
#'
#' @param text Single character string containing the text to synthesize.
#' @param instructions Provide extra instructions to the model.
#' @param model OpenAI text-to-speech model identifier to use.
#' @param voice Voice preset offered by the selected model.
#' @param format Audio container to request from the API; one of `"mp3"`,
#'   `"wav"`, `"opus"`, `"aac"`, `"flac"`, or `"pcm"`.
#' @param speed Optional playback speed multiplier accepted by the API.
#' @param output_file Optional path to write the audio to. When `NULL`, a unique
#'   temporary file is created using the requested format.
#' @param base_url Base URL for the OpenAI-compatible API endpoint.
#' @param api_key API key used for authentication.
#' @param timeout Request timeout (in seconds) passed to the underlying HTTP
#'   helper.
#' @param retry_max Maximum number of retry attempts before giving up.
#' @param retry_backoff Function returning the backoff delay (in seconds) between
#'   retries.
#' @param ca_bundle Optional path to a custom certificate bundle.
#'
#' @details If `output_file` points to a directory that does not yet exist it is
#' created recursively. Any content-type returned by the service that does not
#' begin with `audio/` (or is not a generic `octet-stream`) triggers a warning
#' but the payload is still written to disk to aid debugging.
#'
#' @return A `gptr_audio` object containing the file path, size (in bytes), and
#' the request metadata used to generate the audio.
#'
#' @examples
#' \dontrun{
#' audio <- gpt_tts("Hello from R!")
#' print(audio)
#' }
#'
#' @export
gpt_tts <- function(
  text,
  instructions = "",
  model = "gpt-4o-mini-tts",
  voice = "alloy",
  format = c("mp3", "wav", "opus", "aac", "flac", "pcm"),
  speed = NULL,
  output_file = NULL,
  base_url = getOption("gptr.base_url", "https://api.openai.com"),
  api_key = Sys.getenv("OPENAI_API_KEY"),
  timeout = getOption("gptr.request_timeout", 30),
  retry_max = 3L,
  retry_backoff = function(i) 0.2 * i,
  ca_bundle = getOption("gptr.ca_bundle", NULL)
) {
  stopifnot(is.character(text), length(text) == 1L, nchar(text) > 0)
  format <- match.arg(format)
  if (is.null(output_file)) output_file <- tempfile(fileext = paste0(".", format))

  body <- list(
    model = model,
    voice = voice,
    input = text,
    instructions = instructions,
    response_format = format
  )
  if (!is.null(speed)) body$speed <- speed

  res <- openai_request_non_chat_services(
    task = "tts",
    body = body,
    base_url = base_url,
    api_key = api_key,
    timeout = timeout,
    retry_max = retry_max,
    retry_backoff = retry_backoff,
    ca_bundle = ca_bundle,
    expect = "binary"
  )

  # Minimal sanity check
  if (!grepl("^audio/|octet-stream", tolower(res$ctype))) {
    warning("Unexpected content-type: ", res$ctype, " - saving anyway.")
  }

  dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
  writeBin(res$raw, output_file)

  structure(list(
    file   = normalizePath(output_file, winslash = "/", mustWork = FALSE),
    bytes  = length(res$raw),
    model  = model,
    voice  = voice,
    format = format
  ), class = "gptr_audio")
}

#' Print method for gptr audio objects
#'
#' @param x A `gptr_audio` object returned by [gpt_tts()].
#' @param ... Unused; present for method compatibility.
#'
#' @export
print.gptr_audio <- function(x, ...) {
  cat("<gptr_audio>\n",
      "  file:   ", x$file, "\n",
      "  size:   ", format(utils::object.size(raw(0)) + x$bytes, units = "auto"), "\n",
      "  model:  ", x$model, "\n",
      "  voice:  ", x$voice, "\n",
      "  format: ", x$format, "\n", sep = "")
  invisible(x)
}
