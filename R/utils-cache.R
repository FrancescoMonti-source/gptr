#' Clear gptr internal caches (backend detection, model list, ping)
#'
#' Useful in tests to ensure a clean state.
#' @export
gpt_clear_caches <- function() {
  .gptr_state$detected_backend <- NULL
  .gptr_state$models_cache <- NULL
  .gptr_state$models_base <- NULL
  .gptr_state$warned_missing <- FALSE
  .gptr_state$ping_cache <- new.env(parent = emptyenv())
  invisible(TRUE)
}


#' @export
gpt_refresh_models <- function(base_url = getOption("gpt.local_base_url", NULL)) {
  if (is.null(base_url) || !nzchar(base_url)) stop("Provide base_url or set options(gpt.local_base_url).", call. = FALSE)
  invisible(.get_model_ids(base_url, force = TRUE))
}
