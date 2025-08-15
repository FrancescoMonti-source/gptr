#' Convert a named list to a one-row tibble (no forced character)
#'
#' Keeps values as-is (picks first element if length > 1), so column typing can
#' be done later at the column level by finalize_columns().
#'
#' @param x Named list ready to become a row.
#' @param expected_keys Character vector of expected keys (can be NULL).
#' @return A one-row tibble.
#' @export
row_to_tibble <- function(x, expected_keys) {
    if (is.null(names(x)) || any(names(x) == "")) {
        if (!is.null(expected_keys)) {
            x <- setNames(rep(NA, length(expected_keys)), expected_keys)
        }
    }
    x2 <- lapply(x, function(v) {
        if (is.null(v) || length(v) == 0) return(NA)
        if (length(v) > 1L) v[[1L]] else v
    })
    tibble::as_tibble_row(x2)
}
