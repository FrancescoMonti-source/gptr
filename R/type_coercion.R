.gptr_explicit_logical_true_tokens <- c("true", "t", "yes", "y", "1", "oui")
.gptr_explicit_logical_false_tokens <- c("false", "f", "no", "n", "0", "non")
.gptr_infer_logical_true_tokens <- c("true", "t", "yes", "y")
.gptr_infer_logical_false_tokens <- c("false", "f", "no", "n")

# Normalize user-facing type aliases to the small canonical set used internally.
# This keeps schema parsing, explicit coercion, and final typing on the same vocabulary.
.normalize_type_name <- function(type) {
  if (is.null(type) || !length(type) || is.na(type[[1]])) {
    return(NULL)
  }

  type <- tolower(trimws(as.character(type[[1]])))
  if (type %in% c("integer", "int", "long", "whole")) {
    return("integer")
  }
  if (type %in% c("numeric", "double", "float", "number", "real")) {
    return("numeric")
  }
  if (type %in% c("logical", "bool", "boolean")) {
    return("logical")
  }
  if (type %in% c("character", "string", "text")) {
    return("character")
  }

  NULL
}

.coerce_explicit_logical <- function(value) {
  if (is.null(value)) {
    return(NA)
  }

  if (is.list(value) && !is.data.frame(value)) {
    return(lapply(value, .coerce_explicit_logical))
  }

  if (is.logical(value)) {
    return(value)
  }

  if (is.numeric(value)) {
    out <- rep(NA, length(value))
    ok <- !is.na(value)
    out[ok & value == 1] <- TRUE
    out[ok & value == 0] <- FALSE
    return(out)
  }

  tokens <- trimws(tolower(as.character(value)))
  out <- rep(NA, length(tokens))
  ok <- !is.na(tokens)
  out[ok & tokens %in% .gptr_explicit_logical_true_tokens] <- TRUE
  out[ok & tokens %in% .gptr_explicit_logical_false_tokens] <- FALSE
  out
}

# Shared explicit coercion for schema-driven paths.
# Explicit logical coercion is intentionally broader than inference: it accepts
# common boolean words plus 1/0 and oui/non when a field is declared logical.
.coerce_type <- function(value, type) {
  type <- .normalize_type_name(type) %||% "character"

  if (is.list(value) && !is.data.frame(value)) {
    return(lapply(value, .coerce_type, type = type))
  }

  if (type == "integer") {
    return(suppressWarnings(as.integer(value)))
  }
  if (type == "numeric") {
    return(suppressWarnings(as.numeric(value)))
  }
  if (type == "logical") {
    return(.coerce_explicit_logical(value))
  }

  if (is.null(value)) {
    return(NA_character_)
  }
  as.character(value)
}

# Conservative inference used only when we do not have an explicit target type.
# Keep 1/0 numeric so inference does not silently reinterpret common count/binary fields.
.infer_vector_type <- function(x) {
  if (is.list(x) && !is.data.frame(x)) {
    return(x)
  }

  if (is.character(x)) {
    values <- tolower(trimws(x))
    logic_like <- is.na(values) | values %in% c(.gptr_infer_logical_true_tokens, .gptr_infer_logical_false_tokens, "na", "")
    if (all(logic_like)) {
      out <- rep(NA, length(values))
      ok <- !is.na(values)
      out[ok & values %in% .gptr_infer_logical_true_tokens] <- TRUE
      out[ok & values %in% .gptr_infer_logical_false_tokens] <- FALSE
      return(out)
    }

    numeric_values <- suppressWarnings(as.numeric(values))
    if (any(!is.na(numeric_values))) {
      if (all(is.na(numeric_values) | abs(numeric_values - round(numeric_values)) < .Machine$double.eps^0.5)) {
        return(suppressWarnings(as.integer(numeric_values)))
      }
      return(numeric_values)
    }

    return(as.character(x))
  }

  if (is.logical(x)) {
    return(x)
  }

  if (is.numeric(x)) {
    if (all(is.na(x) | abs(x - round(x)) < .Machine$double.eps^0.5)) {
      return(suppressWarnings(as.integer(x)))
    }
    return(as.numeric(x))
  }

  as.character(x)
}
