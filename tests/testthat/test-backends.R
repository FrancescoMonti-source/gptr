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

test_that("auto + openai model routes to OpenAI", {
    called <- NULL
    httr2::with_mock(
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
            list(df = data.frame(id = "gpt-4o-mini", stringsAsFactors = FALSE),
                 status = "ok")
        },
        request_openai = function(payload, base_url, api_key, timeout = 30) {
            called <<- c(called, "openai")
            fake_resp(model = payload$model %||% "gpt-4o-mini")
        },
        request_local = function(payload, base_url, timeout = 30) {
            called <<- c(called, "local")
            fake_resp(model = payload$model %||% "local-model")
        },
        {
            res <- gpt("hi", model = "gpt-4o-mini", provider = "auto",
                       openai_api_key = "sk-test", print_raw = FALSE)
            expect_identical(called, "openai")
        },
        .env = asNamespace("gptr")
    )
})

test_that("auto + local model routes to local", {
    called <- NULL
    httr2::with_mock(
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
            list(df = data.frame(id = "mistralai/mistral-7b-instruct-v0.3",
                                 stringsAsFactors = FALSE), status = "ok")
        },
        request_local = function(payload, base_url, timeout = 30) {
            called <<- c(called, paste0("local@", base_url))
            fake_resp(model = payload$model %||% "mistral")
        },
        request_openai = function(payload, base_url, api_key, timeout = 30) {
            called <<- c(called, "openai")
            fake_resp(model = payload$model %||% "gpt")
        },
        {
            res <- gpt("hi", model = "mistralai/mistral-7b-instruct-v0.3",
                       provider = "auto", print_raw = FALSE)
            expect_true(length(called) == 1L)
            expect_match(called, "^local@http://127\\.0\\.0\\.1:1234$")
        },
        .env = asNamespace("gptr")
    )
})

test_that("auto + duplicate model prefers locals via gptr.local_prefer", {
    old <- options(gptr.local_prefer = c("ollama","lmstudio","localai"))
    on.exit(options(old), add = TRUE)

    called <- NULL
    httr2::with_mock(
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
            list(df = data.frame(id = "o1-mini", stringsAsFactors = FALSE), status = "ok")
        },
        request_local = function(payload, base_url, timeout = 30) {
            called <<- c(called, paste0("local@", base_url))
            fake_resp(model = payload$model %||% "o1-mini")
        },
        request_openai = function(payload, base_url, api_key, timeout = 30) {
            called <<- c(called, "openai")
            fake_resp(model = payload$model %||% "o1-mini")
        },
        {
            res <- gpt("hi", model = "o1-mini", provider = "auto", print_raw = FALSE)
            expect_identical(called, "local@http://127.0.0.1:11434")
        },
        .env = asNamespace("gptr")
    )
})

test_that("auto + unknown model errors asking for provider", {
    delete_models_cache()
    called <- character()
    httr2::local_mock(
        req_perform = function(req, ...) {
            url <- httr2::req_url(req)
            called <<- c(called, url)
            structure(list(url = url), class = "fake_resp")
        },
        .fetch_models_cached = function(provider = NULL, base_url = NULL,
                                            openai_api_key = "", ...) {
            list(df = data.frame(id = character(), stringsAsFactors = FALSE), status = "ok")
        },
        {
            expect_error(
                gpt("hi", model = "nonexistent-model", provider = "auto", print_raw = FALSE),
                "specify a provider"
            )
        }
    )
})

test_that("auto with no local backend falls back to OpenAI", {
    called <- NULL
    httr2::with_mock(
        .resolve_model_provider = function(model, openai_api_key = "", ...) {
            data.frame(provider = character(), base_url = character(),
                       model_id = character(), stringsAsFactors = FALSE)
        },
        .fetch_models_cached = function(provider = NULL, base_url = NULL,
                                            openai_api_key = "", ...) {
            list(df = data.frame(id = character(), stringsAsFactors = FALSE), status = "ok")
        },
        request_openai = function(payload, base_url, api_key, timeout = 30) {
            called <<- c(called, "openai")
            fake_resp(model = payload$model %||% "fallback")
        },
        request_local = function(payload, base_url, timeout = 30) {
            called <<- c(called, "local")
            fake_resp(model = payload$model %||% "local")
        },
        {
            res <- gpt("hi", provider = "auto", openai_api_key = "sk", print_raw = FALSE)
            expect_identical(called, "openai")
        },
        .env = asNamespace("gptr")
    )
})

test_that("provider=openai routes to OpenAI even if locals have models", {
    called <- NULL
    httr2::with_mock(
        .fetch_models_cached = function(provider = NULL, base_url = NULL,
                                            openai_api_key = "", ...) {
            list(df = data.frame(id = "gpt-4o-mini", stringsAsFactors = FALSE),
                 status = "ok")
        },
        request_openai = function(payload, base_url, api_key, timeout = 30) {
            called <<- c(called, "openai")
            fake_resp(model = payload$model %||% "gpt-4o-mini")
        },
        {
            res <- gpt("hi", model = "gpt-4o-mini", provider = "openai",
                       openai_api_key = "sk-test", print_raw = FALSE)
            expect_identical(called, "openai")
        },
        .env = asNamespace("gptr")
    )
})

test_that("missing openai model falls back to default", {
    used <- NULL
    with_mocked_bindings(
        .fetch_models_cached = function(provider = NULL, base_url = NULL,
                                            openai_api_key = "", ...) {
            list(df = data.frame(id = "gpt-4o-mini", stringsAsFactors = FALSE), status = "ok")
        },
        request_openai = function(payload, base_url, api_key, timeout = 30) {
            used <<- payload$model
            fake_resp(model = payload$model %||% "gpt-4o-mini")
        },
        {
            res <- gpt("hi", model = "unknown", provider = "openai",
                       openai_api_key = "sk-test", print_raw = FALSE)
            expect_identical(used, "gpt-4o-mini")
        }
    )
})

# If the user explicitly says provider = "local" and gives a base_url = "http://…"
# then that exact URL must be used, not replaced by defaults or by whatever is in the cache.
test_that("explicit local base_url is honored", {
    called <- NULL
    httr2::with_mock(
        .fetch_models_cached = function(provider = NULL, base_url = NULL,
                                            openai_api_key = "", ...) {
            list(df = data.frame(id = "mistralai/mistral-7b-instruct-v0.3",
                                 stringsAsFactors = FALSE), status = "ok")
        },
        request_local = function(payload, base_url, timeout = 30) {
            called <<- c(called, paste0("local@", base_url))
            fake_resp(model = payload$model %||% "mistral")
        },
        {
            res <- gpt("hi",
                       provider = "local",
                       base_url = "http://192.168.1.50:1234",
                       model = "mistralai/mistral-7b-instruct-v0.3",
                       strict_model = TRUE,
                       print_raw = FALSE)
            expect_identical(called, "local@http://192.168.1.50:1234")
        },
        .env = asNamespace("gptr")
    )
})

test_that("strict_model errors when model not installed (local)", {
    httr2::with_mock(
        .fetch_models_cached = function(provider = NULL, base_url = NULL,
                                            openai_api_key = "", ...) {
            list(df = data.frame(id = "mistral", stringsAsFactors = FALSE), status = "ok")
        },
        request_local = function(payload, base_url, timeout = 30) {
            fake_resp(model = payload$model %||% "mistral")
        },
        {
            expect_error(
                gpt("hi",
                    provider = "local",
                    model = "llama3:latest",
                    strict_model = TRUE,
                    print_raw = FALSE),
                "Model 'llama3:latest' not found"
            )
        },
        .env = asNamespace("gptr")
    )
})

test_that("strict_model ignored when model listing unavailable", {
    delete_models_cache()
    called <- FALSE
    httr2::local_mock(
        req_perform = function(req, ...) {
            called <<- TRUE
            structure(list(), class = "httr2_response")
        },
        resp_status = function(resp, ...) 404L,
        .env = asNamespace("httr2")
    )
    httr2::local_mock(
        request_local = function(payload, base_url, timeout = 30) {
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
        NA
    )
    expect_true(called)
})

test_that("model match is case-insensitive", {
    called <- NULL
    httr2::with_mock(
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
            list(df = data.frame(id = "GPT-4O-MINI", stringsAsFactors = FALSE), status = "ok")
        },
        request_openai = function(payload, base_url, api_key, timeout = 30) {
            called <<- "openai"
            fake_resp(model = payload$model %||% "GPT-4O-MINI")
        },
        {
            res <- gpt("hi", model = "gpt-4o-mini", provider = "auto",
                       openai_api_key = "sk-test", print_raw = FALSE)
            expect_identical(called, "openai")
        },
        .env = asNamespace("gptr")
    )
})

