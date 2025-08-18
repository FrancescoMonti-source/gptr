.gptr_registry <- local({
    list(
        lmstudio = list(default_base = "http://127.0.0.1:1234/v1/chat/completions"),
        ollama   = list(default_base = "http://127.0.0.1:11434/v1/chat/completions"),
        localai  = list(default_base = "http://127.0.0.1:8080/v1/chat/completions"),
        openai   = list(default_base = "https://api.openai.com/v1/chat/completions")
    )
})

.normalize_provider <- function(provider) {
    if (identical(provider, "local")) "auto" else provider
}

.resolve_base_url <- function(provider, base_url) {
    provider <- .normalize_provider(provider)
    if (!is.null(base_url) && nzchar(base_url)) return(base_url)
    if (provider %in% c("auto","lmstudio","ollama","localai")) {
        pinned <- getOption("gpt.local_base_url", "")
        if (nzchar(pinned)) return(pinned)
    }
    if (provider %in% names(.gptr_registry)) return(.gptr_registry[[provider]]$default_base)
    if (provider == "auto") return(getOption("gpt.lmstudio_base_url", .gptr_registry$lmstudio$default_base))
    stop(sprintf("Unknown provider '%s'.", provider))
}

.ensure_backend_up <- function(base_url, provider) {
    provider <- .normalize_provider(provider)
    if (provider == "openai") return(invisible(TRUE))
    root <- sub("/chat/completions$", "", base_url)
    if (exists(root, envir = .gptr_state$ping_cache, inherits = FALSE)) {
        ok <- get(root, envir = .gptr_state$ping_cache)
        if (isTRUE(ok)) return(invisible(TRUE))
        stop(attr(ok, "msg"), call. = FALSE)
    }
    vmsg <- isTRUE(getOption("gpt.verbose_preflight", FALSE))
    ok <- tryCatch({
        httr2::request(paste0(root, "/models")) |>
            httr2::req_timeout(1) |>
            httr2::req_perform()
        TRUE
    }, error = function(e) e)
    if (isTRUE(ok)) {
        if (vmsg) message(sprintf("[gptr] %s reachable at %s", provider, base_url))
        assign(root, TRUE, envir = .gptr_state$ping_cache)
        return(invisible(TRUE))
    } else {
        msg <- sprintf(
            "Could not reach '%s' at %s. Pass `base_url=` or set options(gpt.%s_base_url=...).",
            provider, base_url,
            if (provider %in% c("lmstudio","ollama","localai")) provider else "local"
        )
        bad <- FALSE; attr(bad, "msg") <- msg
        assign(root, bad, envir = .gptr_state$ping_cache)
        stop(msg, call. = FALSE)
    }
}
