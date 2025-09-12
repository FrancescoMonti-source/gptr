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

  # helper 1
  # normalize common type synonyms coming from specs
  norm_type <- function(t) {
    t <- tolower(trimws(as.character(t)))
    if (t %in% c("integer", "int", "long", "whole")) {
      return("integer")
    }
    if (t %in% c("numeric", "double", "float", "number", "real")) {
      return("numeric")
    }
    if (t %in% c("logical", "bool", "boolean")) {
      return("logical")
    }
    if (t %in% c("character", "string", "text")) {
      return("character")
    }
    t
  }

  # helper 2
  cast_to <- function(x, type) {
    type <- norm_type(type)
    if (type == "integer") {
      # Allow integer cast from numeric/character (e.g. "1","0","64")
      return(suppressWarnings(as.integer(x)))
    }
    if (type == "numeric") {
      return(suppressWarnings(as.numeric(x)))
    }
    if (type == "logical") {
      if (is.logical(x)) {
        return(x)
      }
      if (is.numeric(x)) {
        return(x != 0)
      }
      if (is.character(x)) {
        v <- tolower(trimws(x))
        # IMPORTANT: do NOT treat "1"/"0" as logical here; they should remain numeric/integer.
        out <- ifelse(v %in% c("true", "t", "yes", "y"), TRUE,
          ifelse(v %in% c("false", "f", "no", "n"), FALSE, NA)
        )
        return(out)
      }
      return(suppressWarnings(as.logical(x)))
    }
    as.character(x) # character fallback
  }

  # helper 3
  infer_type <- function(x) {
    # Prefer logical --> integer --> numeric --> character
    if (is.character(x)) {
      v <- tolower(trimws(x))
      # logical from strings (EXCLUDE "1"/"0" so they become integers)
      logic_words <- c("true", "t", "yes", "y", "false", "f", "no", "n")
      maybe_logic <- v %in% c(logic_words, "na", "")
      if (all(maybe_logic)) {
        return(ifelse(v %in% c("true", "t", "yes", "y"), TRUE,
          ifelse(v %in% c("false", "f", "no", "n"), FALSE, NA)
        ))
      }
      # numeric/integer from strings
      xn <- suppressWarnings(as.numeric(v))
      if (any(!is.na(xn))) {
        if (all(is.na(xn) | (abs(xn - round(xn)) < .Machine$double.eps^0.5))) {
          return(suppressWarnings(as.integer(xn)))
        }
        return(xn)
      }
      return(as.character(x))
    }
    # non-character inputs: try logical by value, then integer/numeric
    xl <- suppressWarnings(as.logical(x))
    if (all(is.na(xl) == is.na(x))) {
      return(xl)
    }
    xn <- suppressWarnings(as.numeric(x))
    if (any(!is.na(xn))) {
      if (all(is.na(xn) | (abs(xn - round(xn)) < .Machine$double.eps^0.5))) {
        return(suppressWarnings(as.integer(xn)))
      }
      return(xn)
    }
    as.character(x)
  }

  # helper 4
  infer_vec <- function(x) {
    if (is.character(x)) {
      v <- tolower(trimws(x))
      # logical words ONLY (do not include "1"/"0" here)
      if (all(v %in% c("true", "t", "yes", "y", "false", "f", "no", "n", "na", ""))) {
        return(ifelse(v %in% c("true", "t", "yes", "y"), TRUE,
          ifelse(v %in% c("false", "f", "no", "n"), FALSE, NA)
        ))
      }
      # numeric / integer from strings
      xn <- suppressWarnings(as.numeric(v))
      if (any(!is.na(xn))) {
        if (all(is.na(xn) | abs(xn - round(xn)) < .Machine$double.eps^0.5)) {
          return(suppressWarnings(as.integer(xn)))
        }
        return(xn)
      }
      return(as.character(x))
    }
    # non-character: prefer logical by value, then integer/numeric
    xl <- suppressWarnings(as.logical(x))
    if (all(is.na(xl) == is.na(x))) {
      return(xl)
    }
    xn <- suppressWarnings(as.numeric(x))
    if (any(!is.na(xn))) {
      if (all(is.na(xn) | abs(xn - round(xn)) < .Machine$double.eps^0.5)) {
        return(suppressWarnings(as.integer(xn)))
      }
      return(xn)
    }
    as.character(x)
  }


  for (k in expected_keys) {
    if (!k %in% names(df)) next
    col <- df[[k]]
    if (mode == "as_is") next

    if (mode == "schema") {
      # Prefer explicit schema if provided…
      if (!is.null(key_specs) && k %in% names(key_specs)) {
        spec <- key_specs[[k]]
        if (!is.null(spec$type)) {
          df[[k]] <- cast_to(col, spec$type)
          next
        }
      }
      # …otherwise apply a schema-style fallback cast:
      # integers if whole numbers; logical for TRUE/FALSE words; else numeric then character.
      if (is.character(col)) {
        v <- tolower(trimws(col))
        xn <- suppressWarnings(as.numeric(v))
        # logical words only (no "1"/"0")
        if (all(v %in% c("true", "t", "yes", "y", "false", "f", "no", "n", "na", ""))) {
          df[[k]] <- ifelse(v %in% c("true", "t", "yes", "y"), TRUE,
            ifelse(v %in% c("false", "f", "no", "n"), FALSE, NA)
          )
        } else if (any(!is.na(xn))) {
          if (all(is.na(xn) | (abs(xn - round(xn)) < .Machine$double.eps^0.5))) {
            df[[k]] <- suppressWarnings(as.integer(xn))
          } else {
            df[[k]] <- xn
          }
        } else {
          df[[k]] <- as.character(col)
        }
      } else if (is.numeric(col)) {
        if (all(is.na(col) | (abs(col - round(col)) < .Machine$double.eps^0.5))) {
          df[[k]] <- suppressWarnings(as.integer(col))
        } else {
          df[[k]] <- as.numeric(col)
        }
      } else {
        # logical stays logical; other classes fallback to character
        if (!is.logical(col)) df[[k]] <- as.character(col)
      }
    } else if (mode == "infer") {
      df[[k]] <- infer_vec(df[[k]])
    }
  }
  df
}
