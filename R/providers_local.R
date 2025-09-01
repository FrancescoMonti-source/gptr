# R/providers_local.R
# Discovery helpers ---------------------------------------------------------

#' @keywords internal
# .local_candidates <- function() {
#   lmstudio_base <- getOption(
#     "gptr.lmstudio_base_url",
#     "http://127.0.0.1:1234/v1/chat/completions"
#   )
#   ollama_base <- getOption(
#     "gptr.ollama_base_url",
#     "http://127.0.0.1:11434/v1/chat/completions"
#   )
#   localai_base <- getOption(
#     "gptr.localai_base_url",
#     "http://127.0.0.1:8080/v1/chat/completions"
#   )
#
#   mk_probes <- function(base_url) {
#     bu <- sub("/+$", "", base_url)
#     unique(c(
#       sub("/v1/chat/completions$", "/v1/models", bu, perl = TRUE),
#       sub("/chat/completions$", "/models", bu, perl = TRUE),
#       paste0(bu, "/v1/models"),
#       paste0(bu, "/models")
#     ))
#   }
#
#   builtins <- list(
#     lmstudio = list(
#       key = "lmstudio", name = "LM Studio",
#       base_url = lmstudio_base, probes = mk_probes(lmstudio_base)
#     ),
#     ollama = list(
#       key = "ollama", name = "Ollama",
#       base_url = ollama_base, probes = mk_probes(ollama_base)
#     ),
#     localai = list(
#       key = "localai", name = "LocalAI",
#       base_url = localai_base, probes = mk_probes(localai_base)
#     )
#   )
#
#   # User-extendable overrides
#   extras <- getOption("gptr.extra_local_backends", NULL)
#   if (is.list(extras) && length(extras)) {
#     for (nm in names(extras)) builtins[[nm]] <- extras[[nm]]
#   }
#   builtins
# }

# Return all running local backends with model lists (data.frame + list-col)
#' @keywords internal
# .detect_local_backends <- function(timeout = getOption("gptr.timeout", 5)) {
#   cand <- .local_candidates()
#   out <- list()
#
#   test_ok <- function(url) {
#     tryCatch(
#       {
#         resp <- httr2::req_perform(httr2::request(url) %>% httr2::req_timeout(timeout))
#         httr2::resp_status(resp) < 400
#       },
#       error = function(e) FALSE
#     )
#   }
#
#   for (key in names(cand)) {
#     probes <- cand[[key]]$probes %||% c(cand[[key]]$probe) # compat
#     good <- NULL
#     for (pr in probes) {
#       if (isTRUE(test_ok(pr))) {
#         good <- pr
#         break
#       }
#     }
#     if (!is.null(good)) {
#       models <- .probe_models(good, timeout = timeout)
#       out[[length(out) + 1L]] <- list(
#         backend  = cand[[key]]$key %||% key,
#         name     = cand[[key]]$name,
#         base_url = cand[[key]]$base_url,
#         models   = models
#       )
#     }
#   }
#
#   if (!length(out)) {
#     return(data.frame(
#       backend = character(0),
#       name = character(0),
#       base_url = character(0),
#       models = I(list()),
#       stringsAsFactors = FALSE
#     ))
#   }
#
#   data.frame(
#     backend = vapply(out, `[[`, "", "backend"),
#     name = vapply(out, `[[`, "", "name"),
#     base_url = vapply(out, `[[`, "", "base_url"),
#     models = I(lapply(out, `[[`, "models")),
#     stringsAsFactors = FALSE
#   )
# }

# Choose a backend from detected ones by preference and/or required model
#' @keywords internal
# .pick_local_backend <- function(available_df,
#                                 prefer = getOption(
#                                   "gptr.local_prefer",
#                                   c("lmstudio", "ollama", "localai")
#                                 ),
#                                 require_model = NULL) {
#   if (!nrow(available_df)) {
#     return(NULL)
#   }
#
#   ord <- order(match(available_df$backend, prefer, nomatch = length(prefer) + 1L))
#   df <- available_df[ord, , drop = FALSE]
#
#   if (!is.null(require_model) && nzchar(require_model)) {
#     req <- tolower(require_model)
#     has_model <- vapply(df$models, function(vec) {
#       is.character(vec) && any(tolower(vec) == req)
#     }, FALSE)
#     if (any(has_model)) {
#       return(df[which(has_model)[1L], , drop = FALSE])
#     }
#   }
#
#   df[1L, , drop = FALSE]
# }

# Probe /v1/models and return a character vector of model ids
#' @keywords internal
# .probe_models <- function(models_url, timeout = getOption("gptr.timeout", 5)) {
#   if (!requireNamespace("httr2", quietly = TRUE)) {
#     return(character(0))
#   }
#   resp <- tryCatch(
#     httr2::req_perform(httr2::request(models_url) %>% httr2::req_timeout(timeout)),
#     error = function(e) NULL
#   )
#   if (is.null(resp) || httr2::resp_status(resp) >= 400) {
#     return(character(0))
#   }
#
#   txt <- httr2::resp_body_string(resp)
#   j <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
#   if (is.null(j)) {
#     return(character(0))
#   }
#
#   get_ids <- function(x) {
#     if (!length(x)) {
#       return(character(0))
#     }
#     vapply(x, function(m) {
#       id <- tryCatch(m$id, error = function(e) NULL)
#       if (is.character(id) && length(id) == 1L && nzchar(id)) id else ""
#     }, "", USE.NAMES = FALSE)
#   }
#
#   if (!is.null(j$data) && is.list(j$data)) {
#     return(get_ids(j$data)[nzchar(get_ids(j$data))])
#   }
#   if (!is.null(j$models) && is.list(j$models)) {
#     return(get_ids(j$models)[nzchar(get_ids(j$models))])
#   }
#   if (is.list(j) && all(vapply(j, function(m) is.list(m) && !is.null(m$id), TRUE))) {
#     ids <- get_ids(j)
#     return(ids[nzchar(ids)])
#   }
#   if (is.character(j)) {
#     return(j[nzchar(j)])
#   }
#   character(0)
# }

# Public helper -------------------------------------------------------------

#' List configured local backend endpoints
#'
#' Returns the configured local backend endpoints from package defaults or
#' user options, without checking whether they are actually running.
#'
#' To change defaults globally, set them via `options()` in your `.Rprofile`:
#' \preformatted{
#' options(
#'   gptr.lmstudio_base_url = "http://localhost:1234/v1/chat/completions",
#'   gptr.ollama_base_url   = "http://localhost:11434/v1/chat/completions"
#' )
#' }
#'
#' Add custom backends via \code{options(gptr.extra_local_backends = list(...))}.
#'
#' @return A data frame with columns: backend, name, base_url, probe.
#' @examples
#' list_local_backends()
#  list_local_backends <- function() {
#   x <- .local_candidates()
#   probe1 <- vapply(x, function(e) {
#     p <- e$probes %||% e$probe
#     if (is.null(p)) "" else as.character(p[[1]])
#   }, "")
#   data.frame(
#     backend = vapply(seq_along(x), function(i) (x[[i]]$key %||% names(x)[i]), ""),
#     name = vapply(x, `[[`, "", "name"),
#     base_url = vapply(x, `[[`, "", "base_url"),
#     probe = probe1,
#     row.names = NULL, stringsAsFactors = FALSE
#   )
# }



#' Call a local OpenAI-compatible backend using httr2
#' @noRd
#' @param payload List already shaped like OpenAI chat payload, or a JSON string
#' @param base_url Base URL of the server. Accepts ".../v1" or ".../v1/chat/completions"
#' @param timeout_sec Numeric timeout in seconds
#' @param max_tries Retries (total tries = max_tries)
#' @return list(status = <int>, body = <parsed json list>)
#' @importFrom httr2 request req_url_path_append req_headers req_body_json req_body_raw
#' @importFrom httr2 req_user_agent req_timeout req_retry req_perform resp_status resp_body_json
#' @export
request_local <- function(payload,
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
