.onLoad <- function(libname, pkgname) {
  op <- options()
  op.gptr <- list(
    gptr.context_window = 15000,
    gptr.provider = "auto",
    # generic local defaults (can be redirected to Ollama/LocalAI by changing options)
    gptr.local_base_url = NULL, # If NULL, autodetect will fill this. Useful if the local provider isnt among those already taken into consideration
    gptr.local_model = "mistralai/mistral-7b-instruct-v0.3",
    # backend-specific overrides (optional)
    gptr.lmstudio_model = "mistralai/mistral-7b-instruct-v0.3",
    gptr.lmstudio_base_url = "http://127.0.0.1:1234",
    gptr.ollama_base_url = "http://127.0.0.1:11434",
    gptr.localai_base_url = "http://127.0.0.1:8080",
    # OpenAI
    gptr.openai_model = "gpt-4o-mini",
    # misc
    gptr.request_timeout = 180, # short timeout for snappy local probes
    gptr.max_tries = 2,
    gptr.local_verbose = FALSE, # set TRUE to message which backend was detected
    gptr.verbose_preflight = FALSE,
    gptr.check_model_once = TRUE, # probe once per session (default)
    gptr.model_cache_ttl = 3600 # seconds used when check_model_once = FALSE
  )
  toset <- !(names(op.gptr) %in% names(op))
  if (any(toset)) options(op.gptr[toset])
  invisible()
}
