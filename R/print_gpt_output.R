#' @export
print.gpt_output <- function(x, ...) {
    cat(as.character(x), "\n")
    invisible(x)
}
