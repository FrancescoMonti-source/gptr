fake_local_resp <- function(model = "dummy") {
    list(
        status = 200L,
        body = list(
            model = model,
            choices = list(list(message = list(content = "ok")))
        )
    )
}

backend_opts <- list(
    lmstudio = "gptr.lmstudio_base_url",
    ollama  = "gptr.ollama_base_url",
    localai = "gptr.localai_base_url"
)

defaults <- c(
    lmstudio = "http://127.0.0.1:1234",
    ollama   = "http://127.0.0.1:11434",
    localai  = "http://127.0.0.1:8080"
)

for (bk in names(backend_opts)) {
    opt <- backend_opts[[bk]]

    testthat::test_that(paste0("base_url arg wins over ", opt, " for ", bk), {
        arg_url <- paste0("http://arg-", bk, ".test")
        opt_url <- paste0("http://opt-", bk, ".test")
        withr::local_options(stats::setNames(list(opt_url), opt))
        called <- NULL
        local_gptr_mock(
            .fetch_models_cached = function(provider = NULL, base_url = NULL, openai_api_key = "", ...) {
                list(df = data.frame(id = "test-model", stringsAsFactors = FALSE), status = "ok")
            },
            request_local = function(payload, base_url, ...) {
                called <<- base_url
                fake_local_resp(model = payload$model %||% "test-model")
            }
        )
        res <- gpt("hi", provider = bk, base_url = arg_url, print_raw = FALSE)
        out <- capture.output(print(res))
        testthat::expect_identical(out, "ok")
        testthat::expect_identical(called, arg_url)
    })

    testthat::test_that(paste0(opt, " is used when provider is ", bk), {
        opt_url <- paste0("http://sentinel-", bk, ".test")
        withr::local_options(stats::setNames(list(opt_url), opt))
        called <- NULL
        local_gptr_mock(
            .fetch_models_cached = function(provider = NULL, base_url = NULL, openai_api_key = "", ...) {
                list(df = data.frame(id = "test-model", stringsAsFactors = FALSE), status = "ok")
            },
            request_local = function(payload, base_url, ...) {
                called <<- base_url
                fake_local_resp(model = payload$model %||% "test-model")
            }
        )
        res <- gpt("hi", provider = bk, print_raw = FALSE)
        out <- capture.output(print(res))
        testthat::expect_identical(out, "ok")
        testthat::expect_identical(called, opt_url)
    })

    testthat::test_that(paste0("default base_url is used for ", bk), {
        withr::local_options(stats::setNames(list(NULL), opt))
        called <- NULL
        local_gptr_mock(
            .fetch_models_cached = function(provider = NULL, base_url = NULL, openai_api_key = "", ...) {
                list(df = data.frame(id = "test-model", stringsAsFactors = FALSE), status = "ok")
            },
            request_local = function(payload, base_url, ...) {
                called <<- base_url
                fake_local_resp(model = payload$model %||% "test-model")
            }
        )
        res <- gpt("hi", provider = bk, print_raw = FALSE)
        out <- capture.output(print(res))
        testthat::expect_identical(out, "ok")
        testthat::expect_identical(called, defaults[[bk]])
    })
}
