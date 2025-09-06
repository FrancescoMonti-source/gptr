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

test_that("auto + local model routes to local", {
    called <- NULL
    local_gptr_mock(
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
        })
    res <- gpt("hi", model = "mistralai/mistral-7b-instruct-v0.3",
               provider = "auto", print_raw = FALSE)
    expect_length(called, 1)
    expect_match(called, "^local@http://127\\.0\\.0\\.1:1234$")
})

test_that("auto chooses local backend when available", {
    called <- NULL
    local_gptr_mock(
        .fetch_models_cached = function(provider = NULL, base_url = NULL,
                                        openai_api_key = "", ...) {
            if (identical(provider, "lmstudio")) {
                list(
                    df = data.frame(id = "mistralai/mistral-7b-instruct-v0.3",
                                    stringsAsFactors = FALSE),
                    status = "ok"
                )
            } else {
                list(df = data.frame(id = character(), stringsAsFactors = FALSE),
                     status = "ok")
            }
        },
        request_local = function(payload, base_url, timeout = 30) {
            called <<- c(called, paste0("local@", base_url))
            fake_resp(model = payload$model %||% "mistral")
        },
        request_openai = function(payload, base_url, api_key, timeout = 30) {
            called <<- c(called, "openai")
            fake_resp(model = payload$model %||% "gpt")
        })
    res <- gpt("hi", provider = "auto", openai_api_key = "sk-test", print_raw = FALSE)
    expect_identical(called, "local@http://127.0.0.1:1234")
})

test_that("auto + duplicate model prefers locals via gptr.local_prefer", {
    withr::local_options(list(gptr.local_prefer = c("ollama","lmstudio","localai")))

    called <- NULL
    local_gptr_mock(
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
        })
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
    local_gptr_mock(
        .fetch_models_cached = function(provider = NULL, base_url = NULL,
                                        openai_api_key = "", ...) {
            list(df = data.frame(id = character(), stringsAsFactors = FALSE), status = "ok")
        })
    local_gptr_mock(
        .resolve_model_provider = function(...) data.frame())
    expect_error(
        gpt("hi", model = "nonexistent-model", provider = "auto", print_raw = FALSE),
        "Model 'nonexistent-model' is not available; specify a provider.",
        fixed = TRUE
    )
    expect_identical(called, character())
})

test_that("auto with no local backend falls back to OpenAI", {
    called <- NULL
    local_gptr_mock(
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
        })
    res <- gpt("hi", provider = "auto", openai_api_key = "sk", print_raw = FALSE)
    expect_identical(called, "openai")
})

test_that("auto with empty caches uses default local base URL", {
    called <- NULL
    local_gptr_mock(
        .fetch_models_cached = function(provider = NULL, base_url = NULL,
                                            openai_api_key = "", ...) {
            list(df = data.frame(id = character(), stringsAsFactors = FALSE), status = "ok")
        },
        request_local = function(payload, base_url, timeout = 30) {
            called <<- c(called, base_url)
            fake_resp(model = payload$model %||% "local")
        },
        request_openai = function(payload, base_url, api_key, timeout = 30) {
            called <<- c(called, paste0("openai@", base_url))
            fake_resp(model = payload$model %||% "openai")
        })
    res <- gpt("hi", provider = "auto", openai_api_key = "", print_raw = FALSE)
    expect_identical(called, "http://127.0.0.1:1234")
})

aliases <- c("lmstudio","ollama","localai")
defaults <- c(
    lmstudio = "http://127.0.0.1:1234",
    ollama   = "http://127.0.0.1:11434",
    localai  = "http://127.0.0.1:8080"
)
for (alias in aliases) {
    test_that(sprintf("provider=%s uses default local base URL", alias), {
        called <- NULL
        local_gptr_mock(
            .fetch_models_cached = function(provider = NULL, base_url = NULL,
                                            openai_api_key = "", ...) {
                list(df = data.frame(id = "local-model", stringsAsFactors = FALSE),
                     status = "ok")
            },
            request_local = function(payload, base_url, timeout = 30) {
                called <<- c(called, base_url)
                fake_resp(model = payload$model %||% "local-model")
            })
        res <- gpt("hi", model = "local-model", provider = alias, print_raw = FALSE)
        expect_identical(called, defaults[[alias]])
        expect_identical(attr(res, "backend"), alias)
    })
}

test_that("backend argument selects explicit local backend", {
    fetch_called <- NULL
    request_called <- NULL
    local_gptr_mock(
        .fetch_models_cached = function(provider = NULL, base_url = NULL,
                                        openai_api_key = "", ...) {
            fetch_called <<- c(fetch_called, base_url)
            if (identical(provider, "ollama")) {
                list(df = data.frame(id = "id", stringsAsFactors = FALSE), status = "ok")
            } else {
                list(df = data.frame(id = character(), stringsAsFactors = FALSE), status = "ok")
            }
        },
        request_local = function(payload, base_url, timeout = 30) {
            request_called <<- c(request_called, base_url)
            fake_resp(model = payload$model %||% "id")
        }
    )
    res <- gpt("hi", provider = "local", backend = "ollama", model = "id", print_raw = FALSE)
    expect_identical(fetch_called, "http://127.0.0.1:11434")
    expect_identical(request_called, "http://127.0.0.1:11434")
    expect_identical(attr(res, "backend"), "ollama")
})

test_that("missing openai model falls back to default", {
    expected_model <- getOption("gptr.openai_model")
    used <- NULL
    local_gptr_mock(
        .fetch_models_cached = function(provider = NULL, base_url = NULL,
                                            openai_api_key = "", ...) {
            list(
                df = data.frame(id = expected_model, stringsAsFactors = FALSE),
                status = "ok"
            )
        },
        request_openai = function(payload, base_url, api_key, timeout = 30) {
            used <<- payload$model
            fake_resp(model = payload$model %||% getOption("gptr.openai_model"))
        })
    res <- gpt("hi", model = "unknown", provider = "openai",
               openai_api_key = "sk-test", print_raw = FALSE)
    expect_identical(used, expected_model)
})

# If the user explicitly says provider = "local" and gives a base_url = "http://…"
# then that exact URL must be used, not replaced by defaults or by whatever is in the cache.
test_that("explicit local base_url is honored", {
    called <- NULL
    local_gptr_mock(
        .fetch_models_cached = function(provider = NULL, base_url = NULL,
                                            openai_api_key = "", ...) {
            list(df = data.frame(id = "mistralai/mistral-7b-instruct-v0.3",
                                 stringsAsFactors = FALSE), status = "ok")
        },
        request_local = function(payload, base_url, timeout = 30) {
            called <<- c(called, paste0("local@", base_url))
            fake_resp(model = payload$model %||% "mistral")
        })
    res <- gpt("hi",
               provider = "local",
               base_url = "http://192.168.1.50:1234",
               model = "mistralai/mistral-7b-instruct-v0.3",
               strict_model = TRUE,
               print_raw = FALSE)
    expect_identical(called, "local@http://192.168.1.50:1234")
})

test_that("strict_model errors when model not installed (local)", {
    called <- FALSE
    local_gptr_mock(
        .fetch_models_cached = function(provider = NULL, base_url = NULL,
                                            openai_api_key = "", ...) {
            list(df = data.frame(id = "mistral", stringsAsFactors = FALSE), status = "ok")
        },
        request_local = function(payload, base_url, timeout = 30) {
            called <<- TRUE
            fake_resp(model = payload$model %||% "mistral")
        })
    expect_error(
        gpt("hi",
            provider = "local",
            base_url = "http://127.0.0.1:1234",
            model = "llama3:latest",
            strict_model = TRUE,
            print_raw = FALSE),
        "Model 'llama3:latest' not found",
    )
    expect_false(called)
})

test_that("strict_model skipped when model listing status is 'error'", {
    called <- FALSE
    local_gptr_mock(
        .fetch_models_cached = function(provider = NULL, base_url = NULL,
                                            openai_api_key = "", ...) {
            list(df = data.frame(id = character(), stringsAsFactors = FALSE),
                 status = "error")
        },
        request_local = function(payload, base_url, timeout = 30) {
            called <<- TRUE
            fake_resp(model = payload$model %||% "fallback")
        })
    expect_error(
        gpt("hi",
            provider = "local",
            base_url = "http://127.0.0.1:1234",
            model = "llama3:latest",
            strict_model = TRUE,
            print_raw = FALSE),
        NA
    )
    expect_true(called)
})

test_that("strict_model skipped when model listing df is NULL", {
    called <- FALSE
    local_gptr_mock(
        .fetch_models_cached = function(provider = NULL, base_url = NULL,
                                            openai_api_key = "", ...) {
            list(df = NULL, status = "ok")
        },
        request_local = function(payload, base_url, timeout = 30) {
            called <<- TRUE
            fake_resp(model = payload$model %||% "fallback")
        })
    expect_error(
        gpt("hi",
            provider = "local",
            base_url = "http://127.0.0.1:1234",
            model = "llama3:latest",
            strict_model = TRUE,
            print_raw = FALSE),
        NA
    )
    expect_true(called)
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
    local_gptr_mock(
        request_local = function(payload, base_url, timeout = 30) {
            called_chat <<- TRUE
            fake_resp(model = "fallback")
        })
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

test_that("local model match is case-insensitive", {
    called <- FALSE
    local_gptr_mock(
        .fetch_models_cached = function(provider = NULL, base_url = NULL,
                                        openai_api_key = "", ...) {
            list(df = data.frame(id = "mistral-7b", stringsAsFactors = FALSE), status = "ok")
        },
        request_local = function(payload, base_url, timeout = 30) {
            called <<- TRUE
            fake_resp(model = payload$model %||% "mistral-7b")
        })
    expect_error(
        gpt("hi", model = "MISTRAL-7B", provider = "local",
            base_url = "http://127.0.0.1:1234", print_raw = FALSE),
        NA
    )
    expect_true(called)
})

test_that("gpt surfaces parse errors from request_local", {
    local_gptr_mock(
        .fetch_models_cached = function(provider = NULL, base_url = NULL,
                                        openai_api_key = "", ...) {
            list(df = data.frame(id = "mistral-7b", stringsAsFactors = FALSE),
                 status = "ok")
        })
    testthat::local_mocked_bindings(
        req_perform = function(req, ...) {
            httr2::response(
                status = 200L,
                body = charToRaw("{"),
                headers = list("content-type" = "application/json")
            )
        },
        .env = asNamespace("httr2")
    )
    expect_error(
        gpt("hi",
            provider = "local",
            model = "mistral-7b",
            base_url = "http://127.0.0.1:1234",
            print_raw = FALSE),
        "Local backend HTTP 200",
        fixed = FALSE
    )
})

test_that("mocked request_local HTTP 200 invalid JSON surfaces parse error", {
    local_gptr_mock(
        .fetch_models_cached = function(provider = NULL, base_url = NULL,
                                        openai_api_key = "", ...) {
            list(df = data.frame(id = "mistral-7b", stringsAsFactors = FALSE),
                 status = "ok")
        },
        request_local = function(payload, base_url, timeout = 30) {
            stop("Local backend HTTP 200 at http://127.0.0.1:1234\nBody: {", call. = FALSE)
        }
    )
    expect_error(
        gpt("hi",
            provider = "local",
            model = "mistral-7b",
            base_url = "http://127.0.0.1:1234",
            print_raw = FALSE),
        "Local backend HTTP 200",
        fixed = FALSE
    )
})

test_that("mocked request_local HTTP 500 propagates error message", {
    local_gptr_mock(
        .fetch_models_cached = function(provider = NULL, base_url = NULL,
                                        openai_api_key = "", ...) {
            list(df = data.frame(id = "mistral-7b", stringsAsFactors = FALSE),
                 status = "ok")
        },
        request_local = function(payload, base_url, timeout = 30) {
            stop("Local backend error: boom (http://127.0.0.1:1234)", call. = FALSE)
        }
    )
    expect_error(
        gpt("hi",
            provider = "local",
            model = "mistral-7b",
            base_url = "http://127.0.0.1:1234",
            print_raw = FALSE),
        "Local backend error: boom",
        fixed = TRUE
    )
})

test_that("no backend mocks persist across tests", {
    expect_identical(gptr:::.resolve_model_provider, .orig_resolve_model_provider)
    expect_identical(gptr:::.fetch_models_cached, .orig_fetch_models_cached)
    current_opts <- options()[grepl("^gptr\\.", names(options()))]
    expect_identical(current_opts, .gptr_default_options)
})

