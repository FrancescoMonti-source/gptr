#' Final column typing after row binding
#'
#' @param df Data frame with extracted columns (already bound row-wise).
#' @param expected_keys Character vector of expected keys.
#' @param key_specs Named list of specs (from parse_key_spec()) or NULL.
#' @param mode One of "schema", "infer", "as_is".
#' @return Data frame with harmonized column types.
#' @export
finalize_columns <- function(df, expected_keys, key_specs, mode = c("schema","infer","as_is")) {
    mode <- match.arg(mode)
    if (is.null(expected_keys) || ncol(df) == 0L) return(df)

    cast_to <- function(x, type) {
        if (type == "integer")  return(suppressWarnings(as.integer(x)))
        if (type == "numeric")  return(suppressWarnings(as.numeric(x)))
        if (type == "logical") {
            if (is.logical(x)) return(x)
            if (is.numeric(x)) return(x != 0)
            if (is.character(x)) {
                v <- tolower(trimws(x))
                out <- ifelse(v %in% c("true","1"), TRUE,
                              ifelse(v %in% c("false","0"), FALSE, NA))
                return(out)
            }
            return(as.logical(x))
        }
        as.character(x) # character fallback
    }

    infer_type <- function(x) {
        # 1) logical?
        xl <- suppressWarnings(as.logical(x))
        if (all(is.na(xl) == is.na(x))) return(xl)

        # 2) integer?
        # We'll accept integer if numeric conversion doesn't introduce *more* NAs
        xn <- suppressWarnings(as.numeric(x))
        xi <- suppressWarnings(as.integer(x))
        if (any(!is.na(xn))) {
            # If all non-NA numerics are whole numbers, prefer integer
            if (all(is.na(xn) | (abs(xn - round(xn)) < .Machine$double.eps^0.5))) {
                return(xi)
            }
            # 3) numeric
            return(xn)
        }

        # 4) fallback character
        as.character(x)
    }

    for (k in expected_keys) {
        if (!k %in% names(df)) next
        col <- df[[k]]
        if (mode == "as_is") next

        if (mode == "schema" && !is.null(key_specs) && k %in% names(key_specs)) {
            spec <- key_specs[[k]]
            if (!is.null(spec$type)) {
                df[[k]] <- cast_to(col, spec$type)
            } else {
                df[[k]] <- as.character(col)  # enums: keep as character
            }
        } else if (mode == "infer") {
            df[[k]] <- infer_type(col)
        }
    }

    df
}
