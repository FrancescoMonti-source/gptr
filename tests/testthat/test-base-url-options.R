fake_local_resp <- function(model = "dummy") {
    list(
        status = 200L,
        body = list(
            model = model,
            choices = list(list(message = list(content = "ok")))
        )
    )
}

aliases <- c("lmstudio", "ollama", "localai")

for (alias in aliases) {
    opt <- paste0("gptr.", alias, "_base_url")
    testthat::test_that(paste0(opt, " is used when provider is ", alias), {
        sentinel <- paste0("http://sentinel-", alias, ".test")
        withr::local_options(structure(list(sentinel), names = opt))
        called <- NULL
        local_gptr_mock(
            .fetch_models_cached = function(provider = NULL, base_url = NULL, openai_api_key = "", ...) {
                list(df = data.frame(id = "alias-model", stringsAsFactors = FALSE), status = "ok")
            },
            request_local = function(payload, base_url, ...) {
                called <<- base_url
                fake_local_resp(model = payload$model %||% "alias-model")
            })
        res <- gpt("hi", provider = alias, model = "alias-model", print_raw = FALSE)
        testthat::expect_identical(called, sentinel)
        testthat::expect_identical(attr(res, "backend"), alias)
    })
}
