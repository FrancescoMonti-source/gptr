#' Convert a named list (or scalar) to a one-row tibble (no forced character)
#'
#' Keeps values as-is (first element if length > 1). If x is an unnamed scalar
#' (relaxed mode), it becomes a column named `.parsed` by default.
#'
#' @param x Named list ready to become a row; or a scalar (relaxed mode).
#' @param expected_keys Character vector of expected keys (can be NULL).
#' @return A one-row tibble.
#' @export
row_to_tibble <- function(x, expected_keys = NULL) {
    # Happy path: named list -> one-row tibble, fill/order expected keys
    if (is.list(x) && !is.data.frame(x) && !is.null(names(x))) {
        if (!is.null(expected_keys)) {
            miss <- setdiff(expected_keys, names(x))
            if (length(miss)) x[miss] <- NA
            x <- x[expected_keys]
        }
        return(tibble::as_tibble(x, .name_repair = "minimal"))
    }
    # Relaxed/no-schema path: keep whatever we got in a .parsed col
    if (is.character(x) && length(x) == 1L) {
        return(tibble::tibble(.parsed = x))
    }
    tibble::tibble(.parsed = list(x))
}

parsed_df <- purrr::map_dfr(parsed_results, safe_row_to_tibble, expected_keys = expected_keys)

# Only finalize when scalar columns really exist
has_scalar <- !is.null(expected_keys) && all(expected_keys %in% names(parsed_df))
if (has_scalar) {
    parsed_df <- finalize_columns(
        parsed_df,
        expected_keys = expected_keys,
        key_specs     = key_specs,
        mode          = if (!is.null(key_specs)) "schema" else "as_is"  # or your 'mode' variable
    )
    parsed_df$.parsed <- NULL  # drop helper if present
}
