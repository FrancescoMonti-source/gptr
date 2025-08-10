.onLoad <- function(libname, pkgname) {
    op <- options()
    op.gptr <- list(
        gpt.provider = "local",
        # generic local defaults (can be redirected to Ollama/LocalAI by changing options)
        gpt.local_base_url = NULL,  # let autodetect fill this if NULL
        gpt.local_model = "mistralai/mistral-7b-instruct-v0.3",
        # backend-specific overrides (optional)
        gpt.lmstudio_base_url = "http://127.0.0.1:1234/v1/chat/completions",
        gpt.lmstudio_model    = "mistralai/mistral-7b-instruct-v0.3",
        gpt.ollama_base_url   = "http://127.0.0.1:11434/v1/chat/completions",
        gpt.localai_base_url  = "http://127.0.0.1:8080/v1/chat/completions",
        # OpenAI
        gpt.openai_model = "gpt-4o-mini",
        # misc
        gpt.timeout = 5,                 # short timeout for snappy local probes
        gpt.local_verbose = FALSE        # set TRUE to message which backend was detected
    )
    toset <- !(names(op.gptr) %in% names(op))
    if (any(toset)) options(op.gptr[toset])
    invisible()
}
