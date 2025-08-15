#' Convert a named list (or scalar) to a one-row tibble (no forced character)
#'
#' Keeps values as-is (first element if length > 1). If x is an unnamed scalar
#' (relaxed mode), it becomes a column named `.parsed` by default.
#'
#' @param x Named list ready to become a row; or a scalar (relaxed mode).
#' @param expected_keys Character vector of expected keys (can be NULL).
#' @param raw_col_name Name to use when x is an unnamed scalar (default ".parsed").
#' @return A one-row tibble.
#' @export
row_to_tibble <- function(x, expected_keys, raw_col_name = ".parsed") {
    # 0) Empty list + schema → fill schema with NA
    if (is.list(x) && length(x) == 0L && !is.null(expected_keys)) {
        x <- setNames(rep(NA, length(expected_keys)), expected_keys)
    }

    # 1) Unnamed atomic scalar (relaxed mode): give it a name
    if (is.atomic(x) && (is.null(names(x)) || length(names(x)) == 0L)) {
        x <- setNames(list(x), raw_col_name)
    }

    # 2) If names missing but we have expected keys, fill them
    if ((is.null(names(x)) || any(names(x) == "")) && !is.null(expected_keys)) {
        x <- setNames(rep(NA, length(expected_keys)), expected_keys)
    }

    # 3) Scalarize multi-length elements
    x2 <- lapply(x, function(v) {
        if (is.null(v) || length(v) == 0L) return(NA)
        if (length(v) > 1L) v[[1L]] else v
    })
    tibble::as_tibble_row(x2)
}
