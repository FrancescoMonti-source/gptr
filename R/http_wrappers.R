# ------------------------------------------------------------------------------
# Why do we have tiny HTTP wrappers (.http_request, .http_perform, etc.)?
#
# 1) Testability: one seam to mock
#    - In tests we can patch these *package-internal* functions with
#      `testthat::local_mocked_bindings(.env = asNamespace("gptr"))`.
#    - If the code called httr2 directly (e.g. `httr2::req_perform()`),
#      those calls bypass package scope and are much harder to intercept
#      reliably (qualified calls, imports, different environments).
#
# 2) Deterministic, network-free tests
#    - We return fake responses and status codes from the wrappers so tests
#      never hit the network and are fully deterministic.
#
# 3) Decoupling from httr2
#    - All usage of httr2 is centralized. If we ever swap HTTP libs or tweak
#      retry/backoff behavior, we change it in one place instead of hunting
#      through call sites.
#
# 4) Consistent policy and observability
#    - Timeouts, retries, transient-error rules, and logging/metrics can be
#      applied uniformly inside these wrappers.
#
# 5) Clear failure mapping
#    - Wrappers are the single place to translate transport errors into our
#      small, stable status vocabulary (“unreachable”, “non_json”, “http_503”).
#
# Practical upshot:
#   * Production code ONLY calls the .http_* wrappers (never httr2 directly).
#   * Tests mock the .http_* wrappers; no need to patch httr2 or fight with `::`.
#
# Example (prod):
#   resp <- .http_request(url) |>
#           .http_headers(Authorization = paste("Bearer", key)) |>
#           .http_timeout(timeout) |>
#           .http_retry(max_tries = 3, backoff = function(i) 0.2*i, ...) |>
#           .http_perform()
#   sc <- .http_status(resp)
#   j  <- .http_body_json(resp, simplifyVector = FALSE)
#
# Example (test):
#   testthat::local_mocked_bindings(
#     .http_request   = function(url) list(.url = url),
#     .http_headers   = function(req, ...) req,
#     .http_timeout   = function(req, ...) req,
#     .http_retry     = function(req, ...) req,
#     .http_perform   = function(req, ...) structure(list(.url = req$.url), class="httr2_response"),
#     .http_status    = function(resp) 401L,
#     .http_body_json = function(...) stop("boom"),
#     .env = asNamespace("gptr")
#   )
#   # now .list_openai_live("sk") yields status = "auth_error" without real I/O.
# ------------------------------------------------------------------------------


#' @keywords internal
.http_request  <- function(url)                  httr2::request(url)
#' @keywords internal
.http_headers  <- function(req, ...)             httr2::req_headers(req, ...)
#' @keywords internal
.http_timeout  <- function(req, ...)             httr2::req_timeout(req, ...)
#' @keywords internal
.http_retry    <- function(req, ...)             httr2::req_retry(req, ...)
#' @keywords internal
.http_perform  <- function(req, ...)             httr2::req_perform(req, ...)
#' @keywords internal
.http_status   <- function(resp)                  httr2::resp_status(resp)
#' @keywords internal
.http_body_json<- function(resp, simplifyVector=FALSE) httr2::resp_body_json(resp, simplifyVector = simplifyVector)
