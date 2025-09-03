fake_resp <- function(model = "dummy") {
    # Minimal body that .to_skeleton() can digest when print_raw = FALSE
    list(
        body = list(
            model = model,
            choices = list(list(message = list(content = "ok")))
        ),
        resp = list()
    )
}

test_that("auto + openai model routes to OpenAI", {
    called <- NULL
    with_mocked_bindings(
        .resolve_model_provider = function(model, openai_api_key = "", ...) {
            data.frame(
                provider = "openai",
                base_url = "https://api.openai.com",
                model_id = "gpt-4o-mini",
                stringsAsFactors = FALSE
            )
        },
        .list_models_cached = function(provider = NULL, base_url = NULL,
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
        }
    )
})

test_that("auto + local model routes to local", {
    called <- NULL
    with_mocked_bindings(
        .resolve_model_provider = function(model, openai_api_key = "", ...) {
            data.frame(
                provider = "lmstudio",
                base_url = "http://127.0.0.1:1234",
                model_id = model,
                stringsAsFactors = FALSE
            )
        },
        .list_models_cached = function(provider = NULL, base_url = NULL,
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
        }
    )
})

test_that("auto + duplicate model prefers locals via gptr.local_prefer", {
    old <- options(gptr.local_prefer = c("ollama","lmstudio","localai"))
    on.exit(options(old), add = TRUE)

    called <- NULL
    with_mocked_bindings(
        .resolve_model_provider = function(model, openai_api_key = "", ...) {
            data.frame(
                provider = c("openai", "ollama"),
                base_url  = c("https://api.openai.com", "http://127.0.0.1:11434"),
                model_id  = c("o1-mini", "o1-mini"),
                stringsAsFactors = FALSE
            )
        },
        .list_models_cached = function(provider = NULL, base_url = NULL,
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
        }
    )
})

test_that("auto + unknown model falls back to first preferred local", {
    old <- options(gptr.local_prefer = c("lmstudio","ollama","localai"))
    on.exit(options(old), add = TRUE)

    delete_models_cache()
    called <- character()
    httr2::with_mocked_bindings(
        req_perform = function(req, ...) {
            url <- httr2::req_url(req)
            called <<- c(called, url)
            structure(list(url = url), class = "fake_resp")
        },
        resp_status = function(resp) 404L,
        resp_body_json = function(resp, ...) {
            list(model = "fallback",
                 choices = list(list(message = list(content = "ok"))))
        },
        {
            res <- gpt("hi", model = "nonexistent-model", provider = "auto", print_raw = FALSE)
            expect_true(grepl("^http://127\\.0\\.0\\.1:1234", tail(called, 1)))
        }
    )
})

test_that("auto with no local backend falls back to OpenAI", {
    called <- NULL
    with_mocked_bindings(
        .resolve_model_provider = function(model, openai_api_key = "", ...) {
            data.frame(provider = character(), base_url = character(),
                       model_id = character(), stringsAsFactors = FALSE)
        },
        .list_models_cached = function(provider = NULL, base_url = NULL,
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
        }
    )
})

test_that("provider=openai routes to OpenAI even if locals have models", {
    called <- NULL
    with_mocked_bindings(
        .list_models_cached = function(provider = NULL, base_url = NULL,
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
        }
    )
})

# If the user explicitly says provider = "local" and gives a base_url = "http://…"
# then that exact URL must be used, not replaced by defaults or by whatever is in the cache.
test_that("explicit local base_url is honored", {
    called <- NULL
    with_mocked_bindings(
        .list_models_cached = function(provider = NULL, base_url = NULL,
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
        }
    )
})

test_that("strict_model errors when model not installed (local)", {
    with_mocked_bindings(
        .list_models_cached = function(provider = NULL, base_url = NULL,
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
        }
    )
})

test_that("strict_model ignored when model listing unavailable", {
    delete_models_cache()
    called <- FALSE
    httr2::with_mocked_bindings(
        req_perform = function(req, ...) {
            called <<- TRUE
            stop("mock req_perform failure")
        },
        {
            with_mocked_bindings(
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
                        NA
                    )
                }
            )
        }
    )
    expect_true(called)
})

test_that("model match is case-insensitive", {
    called <- NULL
    with_mocked_bindings(
        .resolve_model_provider = function(model, openai_api_key = "", ...) {
            data.frame(
                provider = "openai",
                base_url = "https://api.openai.com",
                model_id = "GPT-4O-MINI",
                stringsAsFactors = FALSE
            )
        },
        .list_models_cached = function(provider = NULL, base_url = NULL,
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
        }
    )
})

