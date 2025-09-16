fake_resp <- function(model = "dummy") {
    body <- list(
        model = model,
        choices = list(list(message = list(content = "ok")))
    )
    resp <- httr2::response(
        status = 200L,
        body = charToRaw(jsonlite::toJSON(body, auto_unbox = TRUE)),
        headers = list("content-type" = "application/json")
    )
    list(body = body, resp = resp)
}

test_that("backend-specific model options override local_model", {
    withr::local_options(list(gptr.local_model = "modelA", gptr.ollama_model = "modelB"))

    called <- NULL
    testthat::local_mocked_bindings(
        .fetch_models_cached = function(provider = NULL, base_url = NULL,
                                            openai_api_key = "", ...) {
            data.frame(model_id = c("modelA", "modelB"), stringsAsFactors = FALSE)
        },
        .request_local = function(payload, base_url, timeout = 30, ssl_cert = NULL) {
            called <<- payload$model %||% ""
            fake_resp(model = payload$model %||% "")
        },
        .env = asNamespace("gptr")
    )

    gpt("hi", provider = "ollama", print_raw = FALSE)

    expect_identical(called, "modelB")
})

test_that("gpt forwards ssl_cert to OpenAI backend", {
    cert <- withr::local_tempfile(fileext = ".crt")
    writeLines("dummy", cert)

    called <- NULL
    testthat::local_mocked_bindings(
        .fetch_models_cached = function(provider = NULL, base_url = NULL,
                                            openai_api_key = "", ...) {
            data.frame(model_id = "gpt-4o-mini", stringsAsFactors = FALSE)
        },
        openai_send_request = function(payload, base_url, api_key,
                                       timeout = 30, ssl_cert = NULL) {
            called <<- ssl_cert
            fake_resp(model = payload$model %||% "gpt-4o-mini")
        },
        .env = asNamespace("gptr")
    )

    gpt("hi",
        provider = "openai",
        openai_api_key = "sk-test",
        ssl_cert = cert,
        print_raw = FALSE)

    expect_identical(called, cert)
})

test_that("gpt forwards ssl_cert to local backend", {
    cert <- withr::local_tempfile(fileext = ".crt")
    writeLines("dummy", cert)

    called <- NULL
    testthat::local_mocked_bindings(
        .fetch_models_cached = function(provider = NULL, base_url = NULL,
                                            openai_api_key = "", ...) {
            data.frame(model_id = "mistral", stringsAsFactors = FALSE)
        },
        .request_local = function(payload, base_url, timeout_sec = 180,
                                  max_tries = 2, user_agent = "ua",
                                  debug_http = FALSE, ssl_cert = NULL) {
            called <<- ssl_cert
            fake_resp(model = payload$model %||% "mistral")
        },
        .env = asNamespace("gptr")
    )

    gpt("hi",
        provider = "local",
        base_url = "http://127.0.0.1:1234",
        model = "mistral",
        strict_model = FALSE,
        ssl_cert = cert,
        print_raw = FALSE)

    expect_identical(called, cert)
})

test_that("gpt forwards ssl_cert to model discovery (openai path)", {
    cert <- withr::local_tempfile(fileext = ".crt")
    writeLines("dummy", cert)

    recorded <- new.env(parent = emptyenv())
    recorded$resolver <- NULL
    recorded$fetch <- NULL

    testthat::local_mocked_bindings(
        .resolve_model_provider = function(model, openai_api_key = "", ssl_cert = NULL, ...) {
            recorded$resolver <- ssl_cert
            data.frame(
                provider = "openai",
                base_url = "https://api.openai.com",
                model_id = model,
                stringsAsFactors = FALSE
            )
        },
        .fetch_models_cached = function(provider = NULL, base_url = NULL,
                                        openai_api_key = "", refresh = FALSE,
                                        ssl_cert = NULL, ...) {
            if (identical(provider, "openai")) {
                recorded$fetch <- ssl_cert
            }
            data.frame(model_id = "gpt-4o-mini", stringsAsFactors = FALSE)
        },
        openai_send_request = function(payload, base_url, api_key,
                                       timeout = 30, ssl_cert = NULL) {
            fake_resp(model = payload$model %||% "gpt-4o-mini")
        },
        .request_local = function(payload, base_url, timeout = 30, ssl_cert = NULL) {
            fake_resp(model = payload$model %||% "local-model")
        },
        .env = asNamespace("gptr")
    )

    gpt("hi",
        model = "gpt-4o-mini",
        provider = "auto",
        openai_api_key = "sk-test",
        ssl_cert = cert,
        print_raw = FALSE)

    expect_identical(recorded$resolver, cert)
    expect_identical(recorded$fetch, cert)
})

test_that("gpt forwards ssl_cert to local model probes", {
    cert <- withr::local_tempfile(fileext = ".crt")
    writeLines("dummy", cert)

    seen <- new.env(parent = emptyenv())
    seen$local <- NULL

    testthat::local_mocked_bindings(
        .fetch_models_cached = function(provider = NULL, base_url = NULL,
                                        openai_api_key = "", refresh = FALSE,
                                        ssl_cert = NULL, ...) {
            if (identical(provider, "ollama")) {
                seen$local <- ssl_cert
            }
            data.frame(model_id = "mistral", stringsAsFactors = FALSE)
        },
        .request_local = function(payload, base_url, timeout_sec = 180,
                                  max_tries = 2, user_agent = "ua",
                                  debug_http = FALSE, ssl_cert = NULL) {
            fake_resp(model = payload$model %||% "mistral")
        },
        openai_send_request = function(payload, base_url, api_key,
                                       timeout = 30, ssl_cert = NULL) {
            fake_resp(model = payload$model %||% "gpt")
        },
        .env = asNamespace("gptr")
    )

    withr::local_options(list(gptr.local_model = "mistral"))

    gpt("hi",
        provider = "ollama",
        ssl_cert = cert,
        strict_model = FALSE,
        print_raw = FALSE)

    expect_identical(seen$local, cert)
})

test_that("autoswitch probes include ssl_cert when scanning locals", {
    cert <- withr::local_tempfile(fileext = ".crt")
    writeLines("dummy", cert)

    seen <- list()

    testthat::local_mocked_bindings(
        .fetch_models_cached = function(provider = NULL, base_url = NULL,
                                        openai_api_key = "", refresh = FALSE,
                                        ssl_cert = NULL, ...) {
            if (!is.null(provider)) {
                seen[[provider]] <<- ssl_cert
            }
            data.frame(model_id = "local-default", stringsAsFactors = FALSE)
        },
        .request_local = function(payload, base_url, timeout = 30, ssl_cert = NULL) {
            fake_resp(model = payload$model %||% "local-default")
        },
        openai_send_request = function(payload, base_url, api_key,
                                       timeout = 30, ssl_cert = NULL) {
            fake_resp(model = payload$model %||% "openai")
        },
        .env = asNamespace("gptr")
    )

    withr::local_options(list(gptr.local_model = "local-default"))

    gpt("hi",
        provider = "auto",
        strict_model = FALSE,
        openai_api_key = "",
        ssl_cert = cert,
        print_raw = FALSE)

    expect_true(length(seen) >= 1L)
    expect_true(any(vapply(seen, identical, logical(1), cert)))
})

test_that("openai_send_request sets cainfo when ssl_cert supplied", {
    cert <- withr::local_tempfile(fileext = ".crt")
    writeLines("dummy", cert)

    captured <- NULL
    testthat::local_mocked_bindings(
        req_options = function(req, ...) {
            captured <<- list(...)
            req
        },
        req_perform = function(req) {
            body <- list(choices = list(list(message = list(content = "ok"))))
            httr2::response(
                status = 200L,
                headers = list("content-type" = "application/json"),
                body = charToRaw(jsonlite::toJSON(body, auto_unbox = TRUE))
            )
        },
        .env = asNamespace("httr2")
    )

    res <- openai_send_request(
        payload = list(model = "dummy", messages = list()),
        base_url = "https://api.openai.com/v1/chat/completions",
        api_key = "sk-test",
        ssl_cert = cert
    )

    expect_identical(captured, list(cainfo = cert))
    expect_s3_class(res$resp, "httr2_response")
})

test_that(".request_local sets cainfo when ssl_cert supplied", {
    cert <- withr::local_tempfile(fileext = ".crt")
    writeLines("dummy", cert)

    captured <- NULL
    testthat::local_mocked_bindings(
        req_options = function(req, ...) {
            captured <<- list(...)
            req
        },
        req_perform = function(req) {
            fake_resp()$resp
        },
        .env = asNamespace("httr2")
    )

    res <- .request_local(
        payload = list(model = "dummy"),
        base_url = "http://127.0.0.1:1234",
        ssl_cert = cert
    )

    expect_identical(captured, list(cainfo = cert))
    expect_equal(res$status, 200L)
})

test_that("auto + openai model routes to OpenAI", {
    called <- NULL
    testthat::local_mocked_bindings(
        .resolve_model_provider = function(model, openai_api_key = "", ...) {
            data.frame(
                provider = "openai",
                base_url = "https://api.openai.com",
                model_id = "gpt-4o-mini",
                stringsAsFactors = FALSE
            )
        },
        .fetch_models_cached = function(provider = NULL, base_url = NULL,
                                            openai_api_key = "", ...) {
            data.frame(model_id = "gpt-4o-mini", stringsAsFactors = FALSE)
        },
        openai_send_request = function(payload, base_url, api_key, timeout = 30, ssl_cert = NULL) {
            called <<- c(called, "openai")
            fake_resp(model = payload$model %||% "gpt-4o-mini")
        },
        .request_local = function(payload, base_url, timeout = 30, ssl_cert = NULL) {
            called <<- c(called, "local")
            fake_resp(model = payload$model %||% "local-model")
        },
        .env = asNamespace("gptr")
    )
    res <- gpt("hi", model = "gpt-4o-mini", provider = "auto",
               openai_api_key = "sk-test", print_raw = FALSE)
    expect_identical(called, "openai")
})

test_that("auto + local model routes to local", {
    called <- NULL
    testthat::local_mocked_bindings(
        .resolve_model_provider = function(model, openai_api_key = "", ...) {
            data.frame(
                provider = "lmstudio",
                base_url = "http://127.0.0.1:1234",
                model_id = model,
                stringsAsFactors = FALSE
            )
        },
        .fetch_models_cached = function(provider = NULL, base_url = NULL,
                                            openai_api_key = "", ...) {
            data.frame(model_id = "mistralai/mistral-7b-instruct-v0.3",
                       stringsAsFactors = FALSE)
        },
        .request_local = function(payload, base_url, timeout = 30, ssl_cert = NULL) {
            called <<- c(called, paste0("local@", base_url))
            fake_resp(model = payload$model %||% "mistral")
        },
        openai_send_request = function(payload, base_url, api_key, timeout = 30, ssl_cert = NULL) {
            called <<- c(called, "openai")
            fake_resp(model = payload$model %||% "gpt")
        },
        .env = asNamespace("gptr")
    )
    res <- gpt("hi", model = "mistralai/mistral-7b-instruct-v0.3",
               provider = "auto", print_raw = FALSE)
    expect_true(length(called) == 1L)
    expect_match(called, "^local@http://127\\.0\\.0\\.1:1234$")
})

test_that("auto + duplicate model prefers locals via gptr.local_prefer", {
    withr::local_options(list(gptr.local_prefer = c("ollama","lmstudio","localai")))

    called <- NULL
    testthat::local_mocked_bindings(
        .resolve_model_provider = function(model, openai_api_key = "", ...) {
            data.frame(
                provider = c("openai", "ollama"),
                base_url  = c("https://api.openai.com", "http://127.0.0.1:11434"),
                model_id  = c("o1-mini", "o1-mini"),
                stringsAsFactors = FALSE
            )
        },
        .fetch_models_cached = function(provider = NULL, base_url = NULL,
                                            openai_api_key = "", ...) {
            data.frame(model_id = "o1-mini", stringsAsFactors = FALSE)
        },
        .request_local = function(payload, base_url, timeout = 30, ssl_cert = NULL) {
            called <<- c(called, paste0("local@", base_url))
            fake_resp(model = payload$model %||% "o1-mini")
        },
        openai_send_request = function(payload, base_url, api_key, timeout = 30, ssl_cert = NULL) {
            called <<- c(called, "openai")
            fake_resp(model = payload$model %||% "o1-mini")
        },
        .env = asNamespace("gptr")
    )
    res <- gpt("hi", model = "o1-mini", provider = "auto", print_raw = FALSE)
    expect_identical(called, "local@http://127.0.0.1:11434")
})

test_that("auto + unknown model errors asking for provider", {
    called <- character()
    testthat::local_mocked_bindings(
        req_perform = function(req, ...) {
            url <- httr2::req_url(req)
            called <<- c(called, url)
            fake_resp()
        },
        .env = asNamespace("httr2")
    )
    testthat::local_mocked_bindings(
        .fetch_models_cached = function(provider = NULL, base_url = NULL,
                                        openai_api_key = "", ...) {
            data.frame(model_id = character(), stringsAsFactors = FALSE)
        },
        .env = asNamespace("gptr")
    )
    testthat::local_mocked_bindings(
        .resolve_model_provider = function(...) data.frame(),
        .env = asNamespace("gptr")
    )
    expect_error(
        gpt("hi", model = "nonexistent-model", provider = "auto", print_raw = FALSE),
        "Model 'nonexistent-model' is not available; specify a provider.",
        fixed = TRUE
    )
    expect_identical(called, character())
})

test_that("auto + model resolution returning NULL errors asking for provider", {
    testthat::local_mocked_bindings(
        .resolve_model_provider = function(model, openai_api_key = "", ...) NULL,
        .env = asNamespace("gptr")
    )
    expect_error(
        gpt("hi", model = "missing", provider = "auto", print_raw = FALSE),
        "Model 'missing' is not available; specify a provider.",
        fixed = TRUE
    )
})

test_that("auto + model resolution returning empty data frame errors asking for provider", {
    testthat::local_mocked_bindings(
        .resolve_model_provider = function(model, openai_api_key = "", ...) {
            data.frame(provider = character(), base_url = character(),
                       model_id = character(), stringsAsFactors = FALSE)
        },
        .env = asNamespace("gptr")
    )
    expect_error(
        gpt("hi", model = "missing", provider = "auto", print_raw = FALSE),
        "Model 'missing' is not available; specify a provider.",
        fixed = TRUE
    )
})

test_that("auto with no local backend falls back to OpenAI", {
    called <- NULL
    testthat::local_mocked_bindings(
        .resolve_model_provider = function(model, openai_api_key = "", ...) {
            data.frame(provider = character(), base_url = character(),
                       model_id = character(), stringsAsFactors = FALSE)
        },
        .fetch_models_cached = function(provider = NULL, base_url = NULL,
                                            openai_api_key = "", ...) {
            data.frame(model_id = character(), stringsAsFactors = FALSE)
        },
        openai_send_request = function(payload, base_url, api_key, timeout = 30, ssl_cert = NULL) {
            called <<- c(called, "openai")
            fake_resp(model = payload$model %||% "fallback")
        },
        .request_local = function(payload, base_url, timeout = 30, ssl_cert = NULL) {
            called <<- c(called, "local")
            fake_resp(model = payload$model %||% "local")
        },
        .env = asNamespace("gptr")
    )
    res <- gpt("hi", provider = "auto", openai_api_key = "sk", print_raw = FALSE)
    expect_identical(called, "openai")
})

test_that("disable backend autoswitch avoids probing and fallback", {
    withr::local_options(list(gptr.local_prefer = c("ollama","lmstudio","localai")))

    called <- NULL
    testthat::local_mocked_bindings(
        .fetch_models_cached = function(provider = NULL, base_url = NULL,
                                        openai_api_key = "", ...) {
            stop("should not probe")
        },
        .request_local = function(payload, base_url, timeout = 30, ssl_cert = NULL) {
            called <<- c(called, paste0("local@", base_url))
            fake_resp(model = payload$model %||% "fallback")
        },
        openai_send_request = function(payload, base_url, api_key, timeout = 30, ssl_cert = NULL) {
            called <<- c(called, "openai")
            fake_resp(model = payload$model %||% "fallback")
        },
        .env = asNamespace("gptr")
    )
    gpt("hi", provider = "auto", allow_backend_autoswitch = FALSE,
        openai_api_key = "sk", strict_model = FALSE, print_raw = FALSE)
    expect_identical(called, "local@http://127.0.0.1:11434")
})

test_that("provider=openai routes to OpenAI even if locals have models", {
    called <- NULL
    testthat::local_mocked_bindings(
        .fetch_models_cached = function(provider = NULL, base_url = NULL,
                                            openai_api_key = "", ...) {
            data.frame(model_id = "gpt-4o-mini", stringsAsFactors = FALSE)
        },
        openai_send_request = function(payload, base_url, api_key, timeout = 30, ssl_cert = NULL) {
            called <<- c(called, "openai")
            fake_resp(model = payload$model %||% "gpt-4o-mini")
        },
        .env = asNamespace("gptr")
    )
    res <- gpt("hi", model = "gpt-4o-mini", provider = "openai",
               openai_api_key = "sk-test", print_raw = FALSE)
    expect_identical(called, "openai")
})

test_that("missing openai model falls back to default", {
    used <- NULL
    testthat::local_mocked_bindings(
        .fetch_models_cached = function(provider = NULL, base_url = NULL,
                                            openai_api_key = "", ...) {
            data.frame(model_id = "gpt-4o-mini", stringsAsFactors = FALSE)
        },
        openai_send_request = function(payload, base_url, api_key, timeout = 30, ssl_cert = NULL) {
            used <<- payload$model
            fake_resp(model = payload$model %||% "gpt-4o-mini")
        }
    )
    res <- gpt("hi", model = "unknown", provider = "openai",
               openai_api_key = "sk-test", print_raw = FALSE)
    expect_identical(used, "gpt-4o-mini")
})

# If the user explicitly says provider = "local" and gives a base_url = "http://…"
# then that exact URL must be used, not replaced by defaults or by whatever is in the cache.
test_that("explicit local base_url is honored", {
    called <- NULL
    testthat::local_mocked_bindings(
        .fetch_models_cached = function(provider = NULL, base_url = NULL,
                                            openai_api_key = "", ...) {
            data.frame(model_id = "mistralai/mistral-7b-instruct-v0.3",
                       stringsAsFactors = FALSE)
        },
        .request_local = function(payload, base_url, timeout = 30, ssl_cert = NULL) {
            called <<- c(called, paste0("local@", base_url))
            fake_resp(model = payload$model %||% "mistral")
        },
        .env = asNamespace("gptr")
    )
    res <- gpt("hi",
               provider = "local",
               base_url = "http://192.168.1.50:1234",
               model = "mistralai/mistral-7b-instruct-v0.3",
               strict_model = TRUE,
               print_raw = FALSE)
    expect_identical(called, "local@http://192.168.1.50:1234")
})

test_that("strict_model errors when model not installed (local)", {
    testthat::local_mocked_bindings(
        .fetch_models_cached = function(provider = NULL, base_url = NULL,
                                            openai_api_key = "", ...) {
            data.frame(model_id = "mistral", stringsAsFactors = FALSE)
        },
        .request_local = function(payload, base_url, timeout = 30, ssl_cert = NULL) {
            fake_resp(model = payload$model %||% "mistral")
        },
        .env = asNamespace("gptr")
    )
    expect_error(
        gpt("hi",
            provider = "local",
            model = "llama3:latest",
            strict_model = TRUE,
            print_raw = FALSE),
        "Model 'llama3:latest' not found"
    )
})

test_that("strict_model ignored when model listing unavailable", {
    called_models <- FALSE
    called_chat <- FALSE
    testthat::local_mocked_bindings(
        req_perform = function(req, ...) {
            called_models <<- TRUE
            fake_resp()
        },
        resp_status = function(resp, ...) 404L,
        .env = asNamespace("httr2")
    )
    testthat::local_mocked_bindings(
        .request_local = function(payload, base_url, timeout = 30, ssl_cert = NULL) {
            called_chat <<- TRUE
            fake_resp(model = "fallback")
        },
        .env = asNamespace("gptr")
    )
    expect_error(
        gpt("hi",
            provider = "local",
            model = "llama3:latest",
            strict_model = TRUE,
            print_raw = FALSE),
        NA
    )
    expect_true(called_models)
    expect_true(called_chat)
})

test_that("model match is case-insensitive", {
    called <- NULL
    testthat::local_mocked_bindings(
        .resolve_model_provider = function(model, openai_api_key = "", ...) {
            data.frame(
                provider = "openai",
                base_url = "https://api.openai.com",
                model_id = "GPT-4O-MINI",
                stringsAsFactors = FALSE
            )
        },
        .fetch_models_cached = function(provider = NULL, base_url = NULL,
                                            openai_api_key = "", ...) {
            data.frame(model_id = "GPT-4O-MINI", stringsAsFactors = FALSE)
        },
        openai_send_request = function(payload, base_url, api_key, timeout = 30, ssl_cert = NULL) {
            called <<- "openai"
            fake_resp(model = payload$model %||% "GPT-4O-MINI")
        },
        .env = asNamespace("gptr")
    )
    res <- gpt("hi", model = "gpt-4o-mini", provider = "auto",
               openai_api_key = "sk-test", print_raw = FALSE)
    expect_identical(called, "openai")
})

test_that("no backend mocks persist across tests", {
    expect_identical(gptr:::.resolve_model_provider, .orig_resolve_model_provider)
    expect_identical(gptr:::.fetch_models_cached, .orig_fetch_models_cached)
})

