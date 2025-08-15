#' Set or reset the LLM backend used by gptr
#' @export
set_gpt_backend <- function(fun = NULL) {
    if (!is.null(fun)) stopifnot(is.function(fun))
    options(gptr.gpt_fun = fun)
}

# internal getter
.gptr_get_gpt <- function() {
    getOption("gptr.gpt_fun", gpt)  # default to real gpt()
}
