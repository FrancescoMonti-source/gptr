#' List configured local backend endpoints
#'
#' Returns the *configured* local backend endpoints from the package defaults
#' (set in `.onLoad()` inside `zzz.R`) without attempting to detect whether these
#' backends are actually running or reachable.
#'
#' To change these defaults globally, set them via `options()` in your `.Rprofile`
#' or session, for example:
#' \preformatted{
#' options(
#'   gpt.lmstudio_base_url = "http://localhost:1234/v1/chat/completions",
#'   gpt.ollama_base_url   = "http://localhost:11434/v1/chat/completions"
#' )
#' }
#'
#' @return A named character vector of base URLs for supported local backends.
#' @examples
#' list_local_backends()
#' @export
list_local_backends <- function() {
    cli::cli_inform(
        "Listing configured local backend endpoints (no detection performed, these are the values set in zzz.R)."
    )
    c(
        lmstudio = getOption("gpt.lmstudio_base_url"),
        ollama   = getOption("gpt.ollama_base_url"),
        localai  = getOption("gpt.localai_base_url")
    )
}
