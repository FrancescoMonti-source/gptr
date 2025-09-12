# R/providers_local.R

#' Call a local OpenAI-compatible backend using httr2
#' @noRd
#' @param payload List already shaped like OpenAI chat payload, or a JSON string
#' @param base_url Base URL of the server. Accepts ".../v1" or ".../v1/chat/completions"
#' @param timeout_sec Numeric timeout in seconds
#' @param max_tries Retries (total tries = max_tries)
#' @return list(status = <int>, body = <parsed json list>)
#' @importFrom httr2 request req_url_path_append req_headers req_body_json req_body_raw
#' @importFrom httr2 req_user_agent req_timeout req_retry req_perform resp_status resp_body_json
.request_local <- function(payload,
                          base_url,
                          timeout_sec = getOption("gptr.timeout_sec", 180),
                          max_tries = getOption("gptr.max_tries", 2),
                          user_agent = paste0("gptr/", as.character(utils::packageVersion("gptr"))),
                          debug_http = getOption("gptr.debug_http", FALSE)) {
  # Build a clean root: strip any /v1 or /chat/completions; we'll append explicitly
  root <- sub("/chat/completions/?$", "", base_url)
  root <- sub("/v1/?$", "", root)

  req <- httr2::request(root) %>%
    httr2::req_url_path_append("v1", "chat", "completions") %>%
    httr2::req_headers(
      "Content-Type" = "application/json",
      "Accept" = "application/json"
    ) %>%
    httr2::req_user_agent(user_agent) %>%
    httr2::req_timeout(timeout_sec) %>%
    httr2::req_retry(
      max_tries = max_tries,
      backoff = function(attempt) 0.2 * (2^(attempt - 1))
    ) %>%
    httr2::req_error(is_error = function(resp) FALSE) # patched

  # Payload: list -> JSON; character -> raw JSON
  if (is.list(payload)) {
    req <- httr2::req_body_json(req, payload, auto_unbox = TRUE)
  } else if (is.character(payload) && length(payload) == 1L) {
    req <- httr2::req_body_raw(req, charToRaw(payload), type = "application/json")
  } else {
    stop("`payload` must be a named list or a single JSON string.", call. = FALSE)
  }

  if (isTRUE(debug_http)) httr2::req_dry_run(req)

  resp <- httr2::req_perform(req)
  status <- httr2::resp_status(resp)

  # Try parsing JSON; if it fails, fall back to raw string
  j <- try(httr2::resp_body_json(resp, simplifyVector = FALSE), silent = TRUE)
  if (inherits(j, "try-error")) {
    body_txt <- try(httr2::resp_body_string(resp), silent = TRUE)
    # safer url: fall back to base_url if req_url() fails
    url_str <- try(httr2::req_url(req), silent = TRUE)
    if (inherits(url_str, "try-error") || is.null(url_str)) url_str <- base_url
    stop(sprintf(
      "Local backend HTTP %s at %s\nBody: %s",
      status, url_str, as.character(body_txt)
    ), call. = FALSE)
  }

  if (!is.null(j$error)) {
    msg <- if (is.character(j$error)) j$error else (j$error$message %||% "Unknown error")
    url_str <- try(httr2::req_url(req), silent = TRUE)
    if (inherits(url_str, "try-error") || is.null(url_str)) url_str <- base_url
    stop("Local backend error: ", msg, " (", url_str, ")", call. = FALSE)
  }

  # Sanity: ensure text content exists
  txt <- NULL
  if (!is.null(j$choices) && length(j$choices) >= 1) {
    txt <- j$choices[[1]]$message$content
    if (is.null(txt)) txt <- j$choices[[1]]$text
  }
  if (is.null(txt) || !nzchar(as.character(txt))) {
    body_txt <- try(httr2::resp_body_string(resp), silent = TRUE)
    url_str <- try(httr2::req_url(req), silent = TRUE)
    if (inherits(url_str, "try-error") || is.null(url_str)) url_str <- base_url
    stop("Local backend returned empty content. Check `model` and backend response shape.\n",
      "URL: ", url_str, "\nBody: ", as.character(body_txt),
      call. = FALSE
    )
  }

  list(status = status, body = j)
}
