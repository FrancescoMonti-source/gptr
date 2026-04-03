#' Final column typing after row binding
#' @param df Data frame with extracted columns (already bound row-wise).
#' @param expected_keys Character vector of expected keys.
#' @param key_specs Named list of specs (from .parse_key_spec()) or NULL.
#' @param mode One of "schema", "infer", "as_is".
#' @return Data frame with harmonized column types.
#' @keywords internal
.finalize_columns <- function(df, expected_keys, key_specs, mode = c("schema", "infer", "as_is")) {
  mode <- match.arg(mode)
  if (is.null(expected_keys) || ncol(df) == 0L) {
    return(df)
  }

  for (k in expected_keys) {
    if (!k %in% names(df)) next
    col <- df[[k]]
    if (mode == "as_is") next

    if (mode == "schema") {
      # Prefer explicit schema if provided…
      if (!is.null(key_specs) && k %in% names(key_specs)) {
        spec <- key_specs[[k]]
        target_type <- spec$type %||% spec$allowed_type
        if (!is.null(target_type)) {
          df[[k]] <- .coerce_type(col, target_type)
          next
        }
      }
      df[[k]] <- .infer_vector_type(col)
    } else if (mode == "infer") {
      df[[k]] <- .infer_vector_type(df[[k]])
    }
  }
  df
}
