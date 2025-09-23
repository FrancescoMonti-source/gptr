#' Internal unified requester
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
