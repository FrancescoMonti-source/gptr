#' Trim text by "tokens" without hard Python deps
#' @param text character scalar
#' @param max_tokens integer max boundary
#' @param token_mode "words" (default), "chars", or "custom"
#' @param custom_tokenizer optional function(text, max_tokens) -> trimmed_text
trim_text <- function(text,
                      max_tokens = Inf,
                      token_mode = c("words", "chars", "custom"),
                      custom_tokenizer = NULL) {
  token_mode <- match.arg(token_mode)

  if (!is.character(text) || length(text) != 1L || is.na(text) || !nzchar(text)) {
    return(text)
  }
  if (!is.finite(max_tokens)) {
    return(text)
  }

  if (token_mode == "chars") {
    # Very fast, very rough
    return(substr(text, 1L, max_tokens))
  }

  if (token_mode == "words") {
    if (!requireNamespace("tokenizers", quietly = TRUE)) {
      # Fallback: split on whitespace to avoid adding a hard dep
      words <- strsplit(text, "\\s+")[[1]]
    } else {
      words <- tokenizers::tokenize_words(text, lowercase = FALSE)[[1]]
    }
    if (length(words) <= max_tokens) {
      return(text)
    }
    return(paste(words[seq_len(max_tokens)], collapse = " "))
  }

  if (token_mode == "custom") {
    stopifnot(is.function(custom_tokenizer))
    return(custom_tokenizer(text, max_tokens))
  }

  text
}
