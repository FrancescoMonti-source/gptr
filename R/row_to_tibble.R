#' Convert a named list (or scalar) to a one-row tibble (no forced character)
#'
#' Keeps values as-is (first element if length > 1 by default). If x is an
#' unnamed scalar (relaxed mode), it becomes a column named `.parsed` by
#' default.
#'
#' @param x Named list ready to become a row; or a scalar (relaxed mode).
#' @param expected_keys Character vector of expected keys (can be NULL).
#' @param multi_value Strategy when a column name maps to multiple values.
#'   Defaults to `"first"` (keep the first element). Use `"error"` to abort or
#'   `"list"` to keep list-columns.
#' @return A one-row tibble.
row_to_tibble <- function(x, expected_keys = NULL,
                          multi_value = c("first", "error", "list")) {
  multi_value <- match.arg(multi_value)

  normalize_value <- function(v, key = NULL) {
    if (is.null(v) || length(v) == 0L) {
      return(if (multi_value == "list") list(NA) else NA)
    }
    if (multi_value == "list") {
      return(list(v))
    }
    if (length(v) == 1L) {
      return(v)
    }
    if (multi_value == "first") {
      return(v[[1]])
    }
    col <- if (is.null(key)) "<unnamed>" else paste0("'", key, "'")
    stop(
      sprintf(
        "Column %s has multiple values (%d) but `multi_value = \"error\"`.",
        col, length(v)
      ),
      call. = FALSE
    )
  }

  normalize_missing <- function() {
    if (multi_value == "list") list(NA) else NA
  }

  # SCHEMA-FIRST: if a schema is provided, always emit exactly those columns.
  if (!is.null(expected_keys)) {
    vals <- setNames(vector("list", length(expected_keys)), expected_keys)
    if (is.list(x) && !is.data.frame(x) && !is.null(names(x))) {
      # keep only schema keys
      for (key in intersect(names(x), expected_keys)) {
        vals[[key]] <- normalize_value(x[[key]], key)
      }
    }
    # pad missing
    for (key in expected_keys) {
      if (is.null(vals[[key]])) {
        vals[[key]] <- normalize_missing()
      }
    }
    return(tibble::as_tibble(vals, .name_repair = "minimal"))
  }

  # NO-SCHEMA path (unchanged):
  # named list -> one-row tibble
  if (is.list(x) && !is.data.frame(x) && !is.null(names(x))) {
    if (length(x)) {
      col_names <- names(x)
      x <- Map(function(name, value) normalize_value(value, name),
               col_names, x, USE.NAMES = FALSE)
      names(x) <- col_names
    }
    return(tibble::as_tibble(x, .name_repair = "minimal"))
  }
  # scalar character -> simple .parsed
  if (is.character(x) && length(x) == 1L) {
    return(tibble::tibble(.parsed = x))
  }
  tibble::tibble(.parsed = list(x))
}
