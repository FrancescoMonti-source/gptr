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

# 1. Model present on both OpenAI and local backend (gptr.local_prefer honored)
test_that("duplicate model favors local backend per gptr.local_prefer", {
    withr::local_options(list(gptr.local_prefer = c("lmstudio", "ollama", "localai")))
    called <- NULL
    local_gptr_mock(
        .resolve_model_provider = function(model, openai_api_key = "", ...) {
            data.frame(
                provider = c("openai", "lmstudio"),
                base_url = c("https://api.openai.com", "http://127.0.0.1:1234"),
                model_id = c(model, model),
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
        }
    )
    res <- gpt("hi", model = "o1-mini", provider = "auto", openai_api_key = "sk-test", print_raw = FALSE)
    out <- capture.output(print(res))
    expect_identical(out, "ok")
    expect_identical(called, "local@http://127.0.0.1:1234")
})

# 2. Model known only to OpenAI
test_that("openai-only model routes to OpenAI", {
    called <- NULL
    local_gptr_mock(
        .resolve_model_provider = function(model, openai_api_key = "", ...) {
            data.frame(
                provider = "openai",
                base_url = "https://api.openai.com",
                model_id = model,
                stringsAsFactors = FALSE
            )
        },
        .fetch_models_cached = function(provider = NULL, base_url = NULL,
                                        openai_api_key = "", ...) {
            list(df = data.frame(id = "gpt-4o-mini", stringsAsFactors = FALSE), status = "ok")
        },
        request_openai = function(payload, base_url, api_key, timeout = 30) {
            called <<- c(called, "openai")
            fake_resp(model = payload$model %||% "gpt-4o-mini")
        },
        request_local = function(payload, base_url, timeout = 30) {
            called <<- c(called, "local")
            fake_resp(model = payload$model %||% "local")
        }
    )
    res <- gpt("hi", model = "gpt-4o-mini", provider = "auto", openai_api_key = "sk-test", print_raw = FALSE)
    out <- capture.output(print(res))
    expect_identical(out, "ok")
    expect_identical(called, "openai")
})

# 3. Model known only to a local backend
test_that("local-only model routes to local backend", {
    called <- NULL
    local_gptr_mock(
        .resolve_model_provider = function(model, openai_api_key = "", ...) {
            data.frame(
                provider = "ollama",
                base_url = "http://127.0.0.1:11434",
                model_id = model,
                stringsAsFactors = FALSE
            )
        },
        .fetch_models_cached = function(provider = NULL, base_url = NULL,
                                        openai_api_key = "", ...) {
            list(df = data.frame(id = "mistral", stringsAsFactors = FALSE), status = "ok")
        },
        request_local = function(payload, base_url, timeout = 30) {
            called <<- c(called, paste0("local@", base_url))
            fake_resp(model = payload$model %||% "mistral")
        },
        request_openai = function(payload, base_url, api_key, timeout = 30) {
            called <<- c(called, "openai")
            fake_resp(model = payload$model %||% "gpt")
        }
    )
    res <- gpt("hi", model = "mistral", provider = "auto", openai_api_key = "sk-test", print_raw = FALSE)
    out <- capture.output(print(res))
    expect_identical(out, "ok")
    expect_identical(called, "local@http://127.0.0.1:11434")
})

# 4. Unknown model should error
test_that("unknown model errors requesting explicit provider", {
    local_gptr_mock(
        .resolve_model_provider = function(model, openai_api_key = "", ...) {
            data.frame(
                provider = character(),
                base_url = character(),
                model_id = character(),
                stringsAsFactors = FALSE
            )
        },
        .fetch_models_cached = function(provider = NULL, base_url = NULL,
                                        openai_api_key = "", ...) {
            list(df = data.frame(id = character(), stringsAsFactors = FALSE), status = "ok")
        }
    )
    expect_error(
        gpt("hi", model = "ghost-model", provider = "auto", print_raw = FALSE),
        "Model 'ghost-model' is not available; specify a provider.",
        fixed = TRUE
    )
})

