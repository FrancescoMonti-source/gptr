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

test_that("gpt forwards file_path to OpenAI message builder", {
    tmp <- withr::local_tempfile(fileext = ".txt")
    writeLines("hello", tmp)

    seen <- new.env(parent = emptyenv())
    seen$file_paths <- NULL

    testthat::local_mocked_bindings(
        openai_build_messages = function(system = NULL, user = NULL, image_paths = NULL,
                                         file_paths = NULL, on_missing = c("warn", "skip", "error")) {
            seen$file_paths <- file_paths
            list(list(role = "user", content = list(list(type = "text", text = user))))
        },
        .fetch_models_cached = function(provider = NULL, base_url = NULL,
                                        openai_api_key = "", ...) {
            data.frame(model_id = "gpt-4o-mini", stringsAsFactors = FALSE)
        },
        openai_send_request = function(payload, base_url, api_key, timeout = 30, ssl_cert = NULL) {
            fake_resp(model = payload$model %||% "gpt-4o-mini")
        },
        .package = "gptr"
    )

    res <- gpt(
        "hi",
        provider = "openai",
        model = "gpt-4o-mini",
        openai_api_key = "sk-test",
        allow_remote = TRUE,
        file_path = tmp,
        print_raw = FALSE
    )

    expect_identical(as.vector(res), "ok")
    expect_identical(seen$file_paths, tmp)
})

test_that("gpt forwards file_path to local message builder", {
    tmp <- withr::local_tempfile(fileext = ".txt")
    writeLines("hello", tmp)

    seen <- new.env(parent = emptyenv())
    seen$file_paths <- NULL

    testthat::local_mocked_bindings(
        openai_build_messages = function(system = NULL, user = NULL, image_paths = NULL,
                                         file_paths = NULL, on_missing = c("warn", "skip", "error")) {
            seen$file_paths <- file_paths
            list(list(role = "user", content = list(list(type = "text", text = user))))
        },
        .fetch_models_cached = function(provider = NULL, base_url = NULL,
                                        openai_api_key = "", ...) {
            data.frame(model_id = "mistral", stringsAsFactors = FALSE)
        },
        .request_local = function(payload, base_url, timeout = 30, ssl_cert = NULL) {
            fake_resp(model = payload$model %||% "mistral")
        },
        .package = "gptr"
    )

    res <- gpt(
        "hi",
        provider = "local",
        base_url = "http://127.0.0.1:1234",
        model = "mistral",
        file_path = tmp,
        strict_model = FALSE,
        print_raw = FALSE
    )

    expect_identical(as.vector(res), "ok")
    expect_identical(seen$file_paths, tmp)
})

test_that("gpt errors clearly when file_path is missing", {
    expect_error(
        gpt(
            "hi",
            provider = "openai",
            model = "gpt-4o-mini",
            openai_api_key = "sk-test",
            file_path = "definitely-missing-file.txt"
        ),
        "File not found"
    )
})

test_that("gpt_column forwards file_path to gpt", {
    seen <- new.env(parent = emptyenv())
    seen$file_path <- NULL

    testthat::local_mocked_bindings(
        gpt = function(prompt, ..., file_path = NULL, image_path = NULL) {
            seen$file_path <- file_path
            '{"answer":"ok"}'
        },
        .package = "gptr"
    )

    out <- gpt_column(
        data = data.frame(text = "hello", stringsAsFactors = FALSE),
        col = text,
        prompt = "Return JSON only: {json_format}\nText: {text}",
        keys = list(answer = "character"),
        provider = "openai",
        model = "gpt-4o-mini",
        file_path = "https://example.com/file.txt",
        progress = FALSE
    )

    expect_identical(seen$file_path, "https://example.com/file.txt")
    expect_identical(out$answer[[1]], "ok")
})

