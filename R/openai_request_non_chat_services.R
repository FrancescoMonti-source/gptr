#' Perform OpenAI Non-Chat Request
#'
#' Construct and send a JSON request to an OpenAI-compatible endpoint for
#' non-chat services such as image generation, embeddings, text-to-speech, or
#' speech-to-text. The helper normalises base URLs, inserts authentication
#' headers, and handles retry logic.
#'
#' @param task Type of service being requested. Determines the default API path
#'   when `path` is not supplied. Must be one of `"images"`, `"embeddings"`,
#'   `"tts"`, or `"stt"`.
#' @param body Named list converted to JSON and sent as the request payload.
#' @param base_url Base URL of the OpenAI-compatible service.
#' @param path Optional override for the request path. Use when targeting
#'   alternative or beta endpoints.
#' @param api_key API key (Bearer token) used for authentication.
#' @param timeout Request timeout in seconds passed to `httr2::req_timeout()`.
#' @param retry_max Maximum number of attempts performed by
#'   `httr2::req_retry()`.
#' @param retry_backoff Function receiving the retry attempt index and returning
#'   the delay (in seconds) before the next retry.
#' @param ca_bundle Optional path to a custom certificate bundle supplied to
#'   `curl`.
#' @param expect Expected response body type. Use `"json"` for structured data
#'   or `"binary"` when downloading media assets.
#' @param ... Currently unused; reserved for future extension.
#'
#' @details When `path` is `NULL`, a task-specific default endpoint is selected.
#'   The request body is serialized as JSON and sent using `httr2`. Responses
#'   with status codes `>= 400` raise an error with any message returned by the
#'   API.
#'
#' @return If `expect` is `"binary"`, a list containing the raw payload,
#'   response content type, and full `httr2` response object. Otherwise, the
#'   parsed JSON content as an R list.
#'
#' @keywords internal
openai_request_non_chat_services <- function(
  task = c("images", "embeddings", "tts", "stt"),
  body,
  base_url = "https://api.openai.com",
  path    = NULL,
  api_key = Sys.getenv("OPENAI_API_KEY"),
  timeout = getOption("gptr.request_timeout", 30),
  retry_max = 3L,
  retry_backoff = function(i) 0.2 * i,
  ca_bundle = getOption("gptr.ca_bundle", NULL),
  expect = c("json", "binary"),
  ...
) {
  task <- match.arg(task)
  if (!nzchar(api_key)) stop("OPENAI_API_KEY is missing.", call. = FALSE)

  # default paths by task (override with `path` if you want)
  if (is.null(path)) {
    path <- switch(task,
      images     = "/v1/images/generations",
      embeddings = "/v1/embeddings",
      tts        = "/v1/audio/speech",
      stt        = "/v1/audio/transcriptions",
      stop("Unknown task: ", task)
    )
  }
  expect <- match.arg(expect, c("json", "binary"))

  url <- paste0(sub("/+$", "", base_url), path)

  req <- httr2::request(url) |>
    httr2::req_headers(
      "Authorization" = paste("Bearer", api_key),
      "Content-Type"  = "application/json"
    ) |>
    httr2::req_body_json(body) |>
    httr2::req_timeout(timeout) |>
    httr2::req_retry(max_tries = retry_max, backoff = retry_backoff)

  if (!is.null(ca_bundle) && nzchar(ca_bundle)) {
    req <- httr2::req_options(cainfo = ca_bundle)
  }

  resp <- httr2::req_perform(req)

  status <- httr2::resp_status(resp)
  if (status >= 400) {
    err <- try(httr2::resp_body_json(resp, simplifyVector = TRUE), silent = TRUE)
    msg <- if (!inherits(err, "try-error") && !is.null(err$error$message)) {
      err$error$message
    } else {
      paste0("HTTP ", status)
    }
    stop("Request failed: ", msg, call. = FALSE)
  }

  if (identical(expect, "binary")) {
    return(list(
      raw      = httr2::resp_body_raw(resp),
      ctype    = httr2::resp_header(resp, "content-type"),
      response = resp
    ))
  } else {
    return(httr2::resp_body_json(resp, simplifyVector = TRUE))
  }
}

