#' Build a legacy/raw prompt string from a template and schema
#'
#' `build_prompt()` fills a prompt template by injecting the raw input text
#' (`{text}`) and, if provided, a one‑line JSON **format hint** (`{json_format}`)
#' derived from `keys`. It remains available for expert use and backward
#' compatibility with the raw `prompt` interface in `gpt_column()`.
#'
#' @param template Character scalar. A prompt template that may contain the
#'   placeholders `{text}` and `{json_format}`. These are replaced using
#'   \pkg{glue}.
#' @param text Character scalar. The input text to inject into `{text}`.
#'   If `NULL`, `NA`, or empty (`""`), the function returns `"Texte manquant"`.
#' @param keys Named list or `NULL`. When not `NULL`, each element value is a
#'   **type string** (`"integer"`, `"numeric"`, `"character"`, `"logical"`),
#'   which is mapped to a human‑readable “allowed values” hint that forms the
#'   `{json_format}` block shown to the model. (This is a *display aid* only;
#'   parsing/validation happens in \code{gpt_column()}).
#'
#' @return A single character string: the fully rendered prompt.
#'
#' @details
#' - The `{json_format}` block is rendered as a one‑line example like
#'   \code{{ "isolement_bin": 0 ou 1 ou NA, "score": un nombre (ex: 3.14) }}.
#' - This helper does **not** enforce types—it's purely for prompt readability.
#'   Type enforcement and allowed‑set checks should be implemented in the
#'   downstream parser (e.g., \code{gpt_column()}).
#'
#' @examples
#' template <- paste0(
#'   "Tu es un assistant.\n\n",
#'   "Texte :\n\"{text}\"\n\n",
#'   "Réponds avec un JSON une seule ligne, format :\n",
#'   "{json_format}\n",
#'   "- Clés entre guillemets. Aucune autre sortie."
#' )
#'
#' keys <- list(
#'   isolement_bin = "integer",
#'   score         = "numeric"
#' )
#'
#' build_prompt(
#'   template,
#'   text = "Patient vit seul depuis 2 ans.",
#'   keys = keys
#' )
#'
#' # Without a schema, {json_format} is empty and simply omitted:
#' build_prompt(template, text = "Texte libre", keys = NULL)
#'
#' @seealso [gpt_column()] for the preferred instruction-first extraction path,
#'   and [gpt()] to send a single prompt to a model.
#' @importFrom glue glue
#' @importFrom purrr map_chr
#' @importFrom rlang env
#' @export
build_prompt <- function(template, text, keys = NULL) {
    if (is.na(text) || is.null(text) || nchar(text) == 0) {
        return("Texte manquant")
    }
    json_format <- .schema_prompt_hint(keys)

    glue::glue(template, .envir = rlang::env(text = text, json_format = json_format))
}
