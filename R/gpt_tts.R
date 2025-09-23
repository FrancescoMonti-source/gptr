#' Text-to-Speech with OpenAI (MP3/WAV/OGG) — gptr wrapper
#' @export
gpt_tts <- function(
  text,
  model = "gpt-4o-mini-tts",
  voice = "alloy",
  format = c("mp3", "wav", "ogg"),
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
    format = format
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
    warning("Unexpected content-type: ", res$ctype, " — saving anyway.")
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
