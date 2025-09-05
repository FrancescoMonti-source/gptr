fake_local_resp <- function(model = "dummy") {
    list(
        status = 200L,
        body = list(
            model = model,
            choices = list(list(message = list(content = "ok")))
        )
    )
}

backends <- list(
    lmstudio = "gptr.lmstudio_base_url",
    ollama  = "gptr.ollama_base_url",
    localai = "gptr.localai_base_url"
)

for (bk in names(backends)) {
    opt <- backends[[bk]]
    testthat::test_that(paste0(opt, " is used when provider is ", bk), {
        sentinel <- paste0("http://sentinel-", bk, ".test")
        withr::local_options(structure(list(sentinel), names = opt))
        called <- NULL
        testthat::local_mocked_bindings(
            .fetch_models_cached = function(provider = NULL, base_url = NULL, openai_api_key = "", ...) {
                list(df = data.frame(id = "test-model", stringsAsFactors = FALSE), status = "ok")
            },
            request_local = function(payload, base_url, ...) {
                called <<- base_url
                fake_local_resp(model = payload$model %||% "test-model")
            },
            .env = asNamespace("gptr")
        )
        gpt("hi", provider = bk, print_raw = FALSE)
        testthat::expect_identical(called, sentinel)
    })
}
