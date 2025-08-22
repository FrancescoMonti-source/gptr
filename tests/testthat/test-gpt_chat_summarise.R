test_that("gpt_chat_summarise uses wrappers and handles errors", {
    # fake chat env
    fake_chat <- function() NULL
    environment(fake_chat) <- new.env(parent = emptyenv())
    assign("history", list(
        list(role = "user", content = "hi"),
        list(role = "assistant", content = "hello there")
    ), envir = environment(fake_chat))
    assign("gpt_chat", fake_chat, envir = parent.env(environment()))  # make it visible

    # success payload
    payload <- list(choices = list(list(message = list(content = "short summary"))))

    testthat::local_mocked_bindings(
        .http_request       = function(url) list(.url = url),
        .http_req_headers       = function(req, ...) req,
        .http_req_timeout       = function(req, ...) req,
        .http_req_retry         = function(req, ...) req,
        .http_req_body_json = function(req, body) { req$body <- body; req },
        .http_req_perform       = function(req, ...) structure(list(), class = "httr2_response"),
        .http_resp_status        = function(resp) 200L,
        .http_resp_body_json     = function(resp, simplifyVector = FALSE) payload,
        .env = asNamespace("gptr")
    )

    expect_silent(gpt_chat_summarise(base_url = "http://127.0.0.1:1234"))
    env <- environment(gpt_chat)
    expect_equal(length(get("history", envir = env)), 1L)
})
