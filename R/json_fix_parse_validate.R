#' Fix, parse, and (optionally) validate/coerce a JSON string (one row)
#'
#' 1) Fix common JSON issues with tidy_json(); 2) parse via jsonlite; 3) validate
#'    and optionally coerce values according to key_specs.
#'
#' @param out Raw model output (character).
#' @param key_specs Named list of specs from parse_key_spec() or NULL.
#' @param na_values Character vector treated as NA.
#' @param verbose Logical; print repair notes and validation issues.
#' @param i Optional row index for logging (for messages).
#' @param coerce_types Logical; if TRUE, coerce values by spec$type (default TRUE).
#' @param coerce_when Optional predicate function(key, value, spec, row_index) -> TRUE/FALSE,
#'   overrides coerce_types per value when provided.
#' @return list(ok = TRUE/FALSE, value = parsed list or raw string, note = "parsed"/"not-json"/"NA-like")
#' @export

json_fix_parse_validate <- function(out,
                                    key_specs,
                                    na_values,
                                    verbose = FALSE,
                                    i = NA_integer_,
                                    coerce_types = TRUE,
                                    coerce_when = NULL) {
  # Step A: parse JSON (with aggressive tidy_json first)
  rep_pair <- tidy_json(out, na_values = na_values, aggressive = TRUE)
  txt <- rep_pair$txt
  parsed <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = TRUE),
                     error = function(e) NULL)

  # Attempt trailing-comma repair if needed
  if (is.null(parsed)) {
    repaired <- gsub(",\\s*([}\\]])", "\\1", txt)
    parsed <- tryCatch(jsonlite::fromJSON(repaired, simplifyVector = TRUE),
                       error = function(e) NULL)
  }

  # If still not parsable, return failure
  if (is.null(parsed)) {
    return(list(ok = FALSE, value = NULL))
  }

  x <- parsed

  # Step B: align keys if schema provided
  if (!is.null(key_specs) && length(key_specs)) {
    expected_keys <- names(key_specs)
    x <- json_keys_align(
      x,
      expected_keys   = expected_keys,
      auto_correct    = getOption("gptr.auto_correct_keys", TRUE),
      keep_unexpected = getOption("gptr.keep_unexpected_keys", FALSE),
      fuzzy_model     = getOption("gptr.fuzzy_model", "lev_ratio"),
      fuzzy_threshold = getOption("gptr.fuzzy_threshold", 0.3)
    )
  }

  # Step C: per-row, per-key coercion BEFORE binding
  if (isTRUE(coerce_types)) {
    norm_type <- function(t) {
      t <- tolower(trimws(as.character(t)))
      if (t %in% c("integer", "int", "long", "whole")) return("integer")
      if (t %in% c("numeric", "double", "float", "number", "real")) return("numeric")
      if (t %in% c("logical", "bool", "boolean")) return("logical")
      if (t %in% c("character", "string", "text")) return("character")
      t
    }
    cast_one <- function(val, type) {
      type <- norm_type(type)
      if (type == "integer") {
        return(suppressWarnings(as.integer(val)))
      }
      if (type == "numeric") {
        return(suppressWarnings(as.numeric(val)))
      }
      if (type == "logical") {
        if (is.logical(val)) return(val)
        if (is.numeric(val)) return(val != 0)
        if (is.character(val)) {
          v <- tolower(trimws(val))
          return(ifelse(v %in% c("true", "t", "yes", "y"), TRUE,
                        ifelse(v %in% c("false", "f", "no", "n"), FALSE, NA)))
        }
        return(suppressWarnings(as.logical(val)))
      }
      as.character(val)
    }
    target_type <- function(k) {
      if (!is.null(key_specs) && k %in% names(key_specs)) {
        tt <- key_specs[[k]]$type
        if (!is.null(tt)) return(tt)
      }
      if (!is.null(coerce_when) && k %in% names(coerce_when)) {
        return(coerce_when[[k]])
      }
      NULL
    }
    for (k in names(x)) {
      tt <- target_type(k)
      if (!is.null(tt)) {
        x[[k]] <- cast_one(x[[k]], tt)
      }
    }
  }

  list(ok = TRUE, value = x)
}
