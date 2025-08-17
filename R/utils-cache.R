#' Clear gptr internal caches (backend detection, model list, ping)
#'
#' Useful in tests to ensure a clean state.
#' @export
gpt_clear_caches <- function() {
    .gptr_state$detected_backend <- NULL
    .gptr_state$models_cache     <- NULL
    .gptr_state$models_base      <- NULL
    .gptr_state$warned_missing   <- FALSE
    .gptr_state$ping_cache       <- new.env(parent = emptyenv())
    invisible(TRUE)
}
