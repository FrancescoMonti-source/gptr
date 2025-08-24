#' Convert a named list (or scalar) to a one-row tibble (no forced character)
#'
#' Keeps values as-is (first element if length > 1). If x is an unnamed scalar
#' (relaxed mode), it becomes a column named `.parsed` by default.
#'
#' @param x Named list ready to become a row; or a scalar (relaxed mode).
#' @param expected_keys Character vector of expected keys (can be NULL).
#' @return A one-row tibble.
row_to_tibble <- function(x, expected_keys = NULL) {
  # SCHEMA-FIRST: if a schema is provided, always emit exactly those columns.
  if (!is.null(expected_keys)) {
    vals <- list()
    if (is.list(x) && !is.data.frame(x) && !is.null(names(x))) {
      # keep only schema keys
      vals <- x[intersect(names(x), expected_keys)]
    }
    # pad missing
    miss <- setdiff(expected_keys, names(vals))
    if (length(miss)) vals[miss] <- NA
    # order
    vals <- vals[expected_keys]
    return(tibble::as_tibble(vals, .name_repair = "minimal"))
  }

  # NO-SCHEMA path (unchanged):
  # named list -> one-row tibble
  if (is.list(x) && !is.data.frame(x) && !is.null(names(x))) {
    return(tibble::as_tibble(x, .name_repair = "minimal"))
  }
  # scalar character -> simple .parsed
  if (is.character(x) && length(x) == 1L) {
    return(tibble::tibble(.parsed = x))
  }
  tibble::tibble(.parsed = list(x))
}
