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
json_fix_parse_validate <- function(out, key_specs, na_values,
                                    verbose = FALSE, i = NA_integer_,
                                    coerce_types = TRUE, coerce_when = NULL) {
    # If the whole output is NA-like, treat it as empty object
    if (any(trimws(tolower(out)) %in% trimws(tolower(na_values))))
        return(list(ok = TRUE, value = list(), note = "NA-like"))

    rep_pair <- tidy_json(out, na_values, aggressive = FALSE)
    if (verbose && length(rep_pair$log))
        message(glue::glue("Row {i}: Repaired JSON -> {paste(rep_pair$log, collapse = ', ')}"))

    parsed <- tryCatch(jsonlite::fromJSON(rep_pair$txt, simplifyVector = TRUE),
                       error = function(e) NULL)

    if (!is.list(parsed) || is.null(names(parsed)) || any(names(parsed) == "")) {
        return(list(ok = FALSE, value = out, note = "not-json"))
    }

    # Validate & (optionally) coerce
    if (!is.null(key_specs)) {
        parsed <- purrr::imap(parsed, function(value, key) {
            if (is_na_like(value, na_values)) return(NA)

            if (key %in% names(key_specs)) {
                spec <- key_specs[[key]]

                # Per-value decision: coerce or not?
                do_coerce <- if (is.function(coerce_when)) {
                    isTRUE(coerce_when(key, value, spec, i))
                } else {
                    isTRUE(coerce_types)
                }

                if (do_coerce && !is.null(spec$type)) {
                    value <- coerce_type(value, spec$type)
                }

                if (!is.null(spec$allowed)) {
                    if (length(value) > 1L) value <- value[[1L]]
                    if (!in_allowed(value, spec$allowed)) {
                        if (verbose) message(glue::glue(
                            "Value '{normalize_token(value)}' not in allowed set for key '{key}'. Setting NA."))
                        return(NA)
                    }
                }
            }

            value
        })
    }

    list(ok = TRUE, value = parsed, note = "parsed")
}
