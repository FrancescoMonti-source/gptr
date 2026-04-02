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
        .package = "gptr"
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
        .package = "gptr"
    )

    gpt("hi",
        provider = "openai",
        openai_api_key = "sk-test",
        ssl_cert = cert,
        allow_remote = TRUE,
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
        .package = "gptr"
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

test_that("gpt forwards ssl_cert to strict OpenAI model validation", {
    cert <- withr::local_tempfile(fileext = ".crt")
    writeLines("dummy", cert)

    recorded <- NULL

    testthat::local_mocked_bindings(
        .fetch_models_cached = function(provider = NULL, base_url = NULL,
                                        openai_api_key = "", refresh = FALSE,
                                        ssl_cert = NULL, ...) {
            recorded <<- ssl_cert
            data.frame(model_id = "gpt-4o-mini", stringsAsFactors = FALSE)
        },
        openai_send_request = function(payload, base_url, api_key,
                                       timeout = 30, ssl_cert = NULL) {
            fake_resp(model = payload$model %||% "gpt-4o-mini")
        },
        .request_local = function(payload, base_url, timeout = 30, ssl_cert = NULL) {
            fake_resp(model = payload$model %||% "local-model")
        },
        .package = "gptr"
    )

    gpt("hi",
        model = "gpt-4o-mini",
        provider = "openai",
        openai_api_key = "sk-test",
        ssl_cert = cert,
        allow_remote = TRUE,
        print_raw = FALSE)

    expect_identical(recorded, cert)
})

test_that("gpt forwards ssl_cert to strict local model validation", {
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
        .package = "gptr"
    )

    withr::local_options(list(gptr.local_model = "mistral", gptr.ollama_model = "mistral"))

    gpt("hi",
        provider = "ollama",
        ssl_cert = cert,
        strict_model = TRUE,
        print_raw = FALSE)

    expect_identical(seen$local, cert)
})

test_that("auto chooses the preferred local route without provider discovery", {
    called <- NULL
    testthat::local_mocked_bindings(
        .resolve_model_provider = function(...) {
            stop("should not resolve providers")
        },
        .fetch_models_cached = function(provider = NULL, base_url = NULL,
                                        openai_api_key = "", refresh = FALSE,
                                        ssl_cert = NULL, ...) {
            stop("should not probe model listings")
        },
        .request_local = function(payload, base_url, timeout = 30, ssl_cert = NULL) {
            called <<- base_url
            fake_resp(model = payload$model %||% "local-default")
        },
        openai_send_request = function(payload, base_url, api_key,
                                       timeout = 30, ssl_cert = NULL) {
            stop("should not call OpenAI")
        },
        .package = "gptr"
    )

    withr::local_options(list(
        gptr.local_prefer = c("ollama", "lmstudio", "localai"),
        gptr.local_model = "local-default"
    ))

    gpt("hi",
        provider = "auto",
        strict_model = FALSE,
        print_raw = FALSE)

    expect_identical(called, "http://127.0.0.1:11434")
})

test_that("openai_send_request sets cainfo when ssl_cert supplied", {
    cert <- withr::local_tempfile(fileext = ".crt")
    writeLines("dummy", cert)

    captured <- NULL
    res <- testthat::with_mocked_bindings(
        openai_send_request(
            payload = list(model = "dummy", messages = list()),
            base_url = "https://api.openai.com/v1/chat/completions",
            api_key = "sk-test",
            ssl_cert = cert
        ),
        .req_apply_ssl_cert = function(req, ssl_cert = NULL) {
            captured <<- ssl_cert
            req
        },
        .httr_req_perform = function(req) {
            body <- list(choices = list(list(message = list(content = "ok"))))
            httr2::response(
                status = 200L,
                headers = list("content-type" = "application/json"),
                body = charToRaw(jsonlite::toJSON(body, auto_unbox = TRUE))
            )
        },
        .package = "gptr"
    )

    expect_identical(captured, cert)
    expect_s3_class(res$resp, "httr2_response")
})

test_that(".request_local sets cainfo when ssl_cert supplied", {
    cert <- withr::local_tempfile(fileext = ".crt")
    writeLines("dummy", cert)

    captured <- NULL
    res <- testthat::with_mocked_bindings(
        .request_local(
            payload = list(model = "dummy"),
            base_url = "http://127.0.0.1:1234",
            ssl_cert = cert
        ),
        .req_apply_ssl_cert = function(req, ssl_cert = NULL) {
            captured <<- ssl_cert
            req
        },
        .httr_req_perform = function(req) {
            fake_resp()$resp
        },
        .package = "gptr"
    )

    expect_identical(captured, cert)
    expect_equal(res$status, 200L)
})

test_that("auto keeps using the chosen local route even when a model looks remote", {
    called <- NULL
    testthat::local_mocked_bindings(
        .resolve_model_provider = function(...) {
            stop("should not resolve providers")
        },
        .fetch_models_cached = function(provider = NULL, base_url = NULL,
                                        openai_api_key = "", ...) {
            stop("should not probe model listings")
        },
        openai_send_request = function(payload, base_url, api_key, timeout = 30, ssl_cert = NULL) {
            called <<- c(called, "openai")
            fake_resp(model = payload$model %||% "gpt")
        },
        .request_local = function(payload, base_url, timeout = 30, ssl_cert = NULL) {
            called <<- c(called, paste0("local@", base_url), payload$model %||% "")
            fake_resp(model = payload$model %||% "gpt-4o-mini")
        },
        .package = "gptr"
    )
    res <- gpt("hi", model = "gpt-4o-mini", provider = "auto",
               openai_api_key = "sk-test", allow_remote = TRUE,
               strict_model = FALSE, print_raw = FALSE)
    expect_identical(called, c("local@http://127.0.0.1:1234", "gpt-4o-mini"))
})

test_that("auto honors an explicit local backend", {
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
        openai_send_request = function(payload, base_url, api_key, timeout = 30, ssl_cert = NULL) {
            called <<- c(called, "openai")
            fake_resp(model = payload$model %||% "gpt")
        },
        .package = "gptr"
    )
    res <- gpt("hi", model = "mistralai/mistral-7b-instruct-v0.3",
               provider = "auto", backend = "ollama", print_raw = FALSE)
    expect_true(length(called) == 1L)
    expect_match(called, "^local@http://127\\.0\\.0\\.1:11434$")
})

test_that("provider=openai is blocked unless remote transmission is explicitly allowed", {
    called <- FALSE
    testthat::local_mocked_bindings(
        openai_send_request = function(payload, base_url, api_key, timeout = 30, ssl_cert = NULL) {
            called <<- TRUE
            fake_resp(model = payload$model %||% "gpt-4o-mini")
        },
        .package = "gptr"
    )

    expect_error(
        gpt("hi",
            provider = "openai",
            model = "gpt-4o-mini",
            openai_api_key = "sk-test",
            print_raw = FALSE),
        "would send data to a non-local endpoint"
    )
    expect_false(called)
})

test_that("auto with strict_model validates only the chosen local route", {
    testthat::local_mocked_bindings(
        .resolve_model_provider = function(...) {
            stop("should not resolve providers")
        },
        .fetch_models_cached = function(provider = NULL, base_url = NULL,
                                        openai_api_key = "", ...) {
            expect_identical(provider, "lmstudio")
            expect_identical(base_url, "http://127.0.0.1:1234")
            data.frame(model_id = "mistral", stringsAsFactors = FALSE)
        },
        .package = "gptr"
    )
    expect_error(
        gpt("hi", model = "missing", provider = "auto", print_raw = FALSE),
        "Model 'missing' not found on http://127.0.0.1:1234.",
        fixed = TRUE
    )
})

test_that("auto falls back to OpenAI only when no local route is configured", {
    called <- NULL
    withr::local_options(list(
        gptr.local_base_url = NULL,
        gptr.lmstudio_base_url = "",
        gptr.ollama_base_url = "",
        gptr.localai_base_url = "",
        gptr.local_prefer = character()
    ))
    testthat::local_mocked_bindings(
        .fetch_models_cached = function(provider = NULL, base_url = NULL,
                                        openai_api_key = "", ...) {
            expect_identical(provider, "openai")
            data.frame(model_id = "gpt-4o-mini", stringsAsFactors = FALSE)
        },
        openai_send_request = function(payload, base_url, api_key, timeout = 30, ssl_cert = NULL) {
            called <<- c(called, "openai")
            fake_resp(model = payload$model %||% "gpt-4o-mini")
        },
        .request_local = function(payload, base_url, timeout = 30, ssl_cert = NULL) {
            called <<- c(called, "local")
            fake_resp(model = payload$model %||% "local")
        },
        .package = "gptr"
    )
    res <- gpt("hi", provider = "auto", openai_api_key = "sk", allow_remote = TRUE, print_raw = FALSE)
    expect_identical(called, "openai")
})

test_that("allow_backend_autoswitch is ignored for backward compatibility", {
    called <- NULL
    testthat::local_mocked_bindings(
        .resolve_model_provider = function(...) {
            stop("should not resolve providers")
        },
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
        .package = "gptr"
    )
    withr::local_options(list(gptr.local_prefer = c("ollama","lmstudio","localai")))
    gpt("hi", provider = "auto", allow_backend_autoswitch = FALSE,
        strict_model = FALSE, print_raw = FALSE)
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
        .package = "gptr"
    )
    res <- gpt("hi", model = "gpt-4o-mini", provider = "openai",
               openai_api_key = "sk-test", allow_remote = TRUE, print_raw = FALSE)
    expect_identical(called, "openai")
})

test_that("strict_model = FALSE sends the requested OpenAI model unchanged", {
    used <- NULL
    testthat::local_mocked_bindings(
        openai_send_request = function(payload, base_url, api_key, timeout = 30, ssl_cert = NULL) {
            used <<- payload$model
            fake_resp(model = payload$model %||% "gpt-4o-mini")
        },
        .package = "gptr"
    )
    res <- gpt("hi", model = "unknown", provider = "openai",
               openai_api_key = "sk-test", strict_model = FALSE, allow_remote = TRUE, print_raw = FALSE)
    expect_identical(used, "unknown")
})

test_that("strict_model errors when OpenAI model is unavailable", {
    testthat::local_mocked_bindings(
        .fetch_models_cached = function(provider = NULL, base_url = NULL,
                                        openai_api_key = "", ...) {
            data.frame(model_id = "gpt-4o-mini", stringsAsFactors = FALSE)
        },
        .package = "gptr"
    )

    expect_error(
        gpt("hi",
            model = "unknown",
            provider = "openai",
            openai_api_key = "sk-test",
            strict_model = TRUE,
            allow_remote = TRUE,
            print_raw = FALSE),
        "Model 'unknown' not found for OpenAI."
    )
})

test_that("explicit non-loopback local base_url is blocked unless remote transmission is allowed", {
    expect_error(
        gpt("hi",
            provider = "local",
            base_url = "http://192.168.1.50:1234",
            model = "mistralai/mistral-7b-instruct-v0.3",
            strict_model = FALSE,
            print_raw = FALSE),
        "would send data to a non-local endpoint"
    )
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
        .package = "gptr"
    )
    res <- gpt("hi",
               provider = "local",
               base_url = "http://192.168.1.50:1234",
               model = "mistralai/mistral-7b-instruct-v0.3",
               strict_model = TRUE,
               allow_remote = TRUE,
               print_raw = FALSE)
    expect_identical(called, "local@http://192.168.1.50:1234")
})

test_that("local base_url path variants are normalized", {
    urls <- c("http://custom:1234/v1", "http://custom:1234/v1/chat/completions")
    testthat::local_mocked_bindings(
        .fetch_models_cached = function(provider = NULL, base_url = NULL,
                                            openai_api_key = "", ...) {
            data.frame(model_id = "custom-model", stringsAsFactors = FALSE)
        },
        .request_local = function(payload, base_url, timeout = 30, ssl_cert = NULL) {
            captured <<- c(captured, base_url)
            fake_resp(model = payload$model %||% "custom-model")
        },
        .package = "gptr"
    )

    for (url in urls) {
        captured <- character()
        gpt("hi",
            provider = "local",
            base_url = url,
            model = "custom-model",
            strict_model = TRUE,
            allow_remote = TRUE,
            print_raw = FALSE)
        expect_identical(captured, "http://custom:1234")
    }
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
        .package = "gptr"
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

test_that("strict_model still errors if the server swaps models after listing failure", {
    called_models <- FALSE
    called_chat <- FALSE
    testthat::local_mocked_bindings(
        .fetch_models_cached = function(provider = NULL, base_url = NULL,
                                        openai_api_key = "", refresh = FALSE,
                                        ssl_cert = NULL, ...) {
            called_models <<- TRUE
            data.frame(model_id = character(), stringsAsFactors = FALSE)
        },
        .request_local = function(payload, base_url, timeout_sec = 180, max_tries = 2,
                                  user_agent = "ua", debug_http = FALSE, ssl_cert = NULL, ...) {
            called_chat <<- TRUE
            fake_resp(model = "fallback")
        },
        .package = "gptr"
    )
    expect_error(
        gpt("hi",
            provider = "local",
            model = "llama3:latest",
            openai_api_key = "",
            strict_model = TRUE,
            print_raw = FALSE),
        "Server used model 'fallback' instead of requested 'llama3:latest'.",
        fixed = TRUE
    )
    expect_true(called_models)
    expect_true(called_chat)
})

test_that("OpenAI strict_model matching is case-insensitive", {
    called <- NULL
    testthat::local_mocked_bindings(
        .fetch_models_cached = function(provider = NULL, base_url = NULL,
                                            openai_api_key = "", ...) {
            data.frame(model_id = "GPT-4O-MINI", stringsAsFactors = FALSE)
        },
        openai_send_request = function(payload, base_url, api_key, timeout = 30, ssl_cert = NULL) {
            called <<- "openai"
            fake_resp(model = payload$model %||% "GPT-4O-MINI")
        },
        .package = "gptr"
    )
    res <- gpt("hi", model = "gpt-4o-mini", provider = "openai",
               openai_api_key = "sk-test", allow_remote = TRUE, print_raw = FALSE)
    expect_identical(called, "openai")
})

test_that("no backend mocks persist across tests", {
    expect_true(is.function(gptr:::.resolve_model_provider))
    expect_true(is.function(gptr:::.fetch_models_cached))
})


