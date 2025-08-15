# Return a function to call the LLM:
# - If tests set options(gptr.gpt_fun = mock_gpt), use that (no HTTP)
# - Else call the real gpt(), passing provider only when present
.gptr_resolve_backend <- function(provider = NULL) {
    fun_override <- getOption("gptr.gpt_fun", NULL)
    if (is.function(fun_override)) return(fun_override)

    provider_val <- provider  # capture; DO NOT create a formal named 'provider' below
    function(prompt, ...) {
        if (is.null(provider_val)) {
            gpt(prompt = prompt, ...)  # no provider arg -> gpt() picks default
        } else {
            gpt(prompt = prompt, provider = provider_val, ...)
        }
    }
}
