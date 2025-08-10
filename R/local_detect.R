#' @keywords internal
.local_candidates <- function() {
    lmstudio_base <- getOption("gpt.lmstudio_base_url",
                               "http://127.0.0.1:1234/v1/chat/completions")
    ollama_base   <- getOption("gpt.ollama_base_url",
                               "http://127.0.0.1:11434/v1/chat/completions")
    localai_base  <- getOption("gpt.localai_base_url",
                               "http://127.0.0.1:8080/v1/chat/completions")

    to_models <- function(base_url) sub("/chat/completions$", "/models", base_url)

    list(
        lmstudio = list(name = "LM Studio", base_url = lmstudio_base, probe = to_models(lmstudio_base)),
        ollama   = list(name = "Ollama",    base_url = ollama_base,   probe = to_models(ollama_base)),
        localai  = list(name = "LocalAI",   base_url = localai_base,  probe = to_models(localai_base))
    )
}

#' Detect a running local OpenAI-compatible backend
#' @keywords internal
.detect_local_backend <- function(timeout = getOption("gpt.timeout", 5)) {
    if (!requireNamespace("httr", quietly = TRUE)) return(NULL)
    cand <- .local_candidates()
    for (key in names(cand)) {
        pr <- cand[[key]]$probe
        ok <- tryCatch({
            resp <- httr::GET(pr, httr::timeout(timeout))
            httr::status_code(resp) < 400
        }, error = function(e) FALSE)
        if (ok) {
            return(list(backend = key, name = cand[[key]]$name, base_url = cand[[key]]$base_url))
        }
    }
    NULL
}

#' List configured local backends
#' @return A data frame with backend, name, base_url and probe.
#' @export
list_local_backends <- function() {
    x <- .local_candidates()
    data.frame(
        backend  = names(x),
        name     = vapply(x, `[[`, "", "name"),
        base_url = vapply(x, `[[`, "", "base_url"),
        probe    = vapply(x, `[[`, "", "probe"),
        row.names = NULL, stringsAsFactors = FALSE
    )
}
