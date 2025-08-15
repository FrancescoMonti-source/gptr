#' Fix, parse, and (optionally) validate/coerce a JSON string
#'
#' @param out Raw model output (character).
#' @param key_specs Named list of specs from parse_key_spec(); NULL for relaxed/no schema.
#' @param na_values Character vector treated as NA.
#' @param verbose Logical; print repair notes and validation issues.
#' @param i Optional row index for logging.
#' @param coerce_types Logical; if TRUE, coerce by spec$type (default TRUE).
#' @param coerce_when Optional predicate: function(key, value, spec, row_index) -> TRUE/FALSE
#'   to decide per-value coercion. Overrides coerce_types when provided.
#' @return list(ok = TRUE/FALSE, value = parsed list or raw string, note = "parsed"/"not-json"/"NA-like")
#' @export
json_fix_parse_validate <- function(out, key_specs, na_values, verbose = FALSE, i = NA_integer_,
                                    coerce_types = TRUE, coerce_when = NULL) {
    # Treat NA-like whole outputs
    if (any(trimws(tolower(out)) %in% trimws(tolower(na_values))))
        return(list(ok = TRUE, value = list(), note = "NA-like"))

    rep_pair <- tidy_json(out, na_values, aggressive = FALSE)
    if (verbose && length(rep_pair$log))
        message(glue::glue("Row {i}: Repaired JSON -> {paste(rep_pair$log, collapse = ', ')}"))

    parsed <- tryCatch(jsonlite::fromJSON(rep_pair$txt, simplifyVector = TRUE),
                       error = function(e) NULL)
    if (!is.list(parsed) || is.null(names(parsed)) || any(names(parsed) == ""))
        return(list(ok = FALSE, value = out, note = "not-json"))

    # Validation & optional coercion
    if (!is.null(key_specs)) {
        parsed <- purrr::imap(parsed, function(value, key) {
            if (is_na_like(value, na_values)) return(NA)

            if (key %in% names(key_specs)) {
                spec <- key_specs[[key]]

                # Decide whether to coerce this specific value
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
