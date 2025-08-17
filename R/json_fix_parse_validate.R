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
json_fix_parse_validate <- function(text,
                                    key_specs    = NULL,
                                    na_values    = c("NA","N/A","null","None",""),
                                    verbose      = FALSE,
                                    i            = NULL,
                                    coerce_types = FALSE,
                                    coerce_when  = NULL) {
    looks_like_json <- function(s) {
        s <- trimws(s)
        (startsWith(s, "{") && grepl("}\\s*$", s)) || (startsWith(s, "\\[") && grepl("\\]\\s*$", s))
    }
    parse_try <- function(z, simplify = FALSE) {
        try(jsonlite::fromJSON(z, simplifyVector = simplify), silent = TRUE)
    }

    # A) Try RAW text first (pre-tidy): unwrap quoted JSON string literal
    raw_txt <- as.character(text)
    val0 <- parse_try(raw_txt, simplify = FALSE)

    if (!inherits(val0, "try-error") && is.character(val0) && length(val0) == 1L) {
        inner <- trimws(val0)
        if (looks_like_json(inner)) {
            if (isTRUE(verbose)) message("Row ", if (is.null(i)) "" else i, ": unwrapped JSON string literal (pre-tidy)")
            val <- parse_try(inner, simplify = FALSE)
            if (!inherits(val, "try-error") && is.list(val) && !is.null(names(val))) {
                return(list(ok = TRUE, value = val, meta = NULL))
            }
        }
    }

    # B) Light cleanup via tidy_json()
    tj <- tidy_json(text, na_values = na_values)
    s  <- tj$txt
    if (is.null(s) || !nzchar(s)) {
        meta <- data.frame(stage = "parse", note = "empty_after_tidy", stringsAsFactors = FALSE)
        return(list(ok = FALSE, value = NULL, meta = meta))
    }

    val1 <- parse_try(s, simplify = FALSE)
    if (!inherits(val1, "try-error") && is.list(val1) && !is.null(names(val1))) {
        return(list(ok = TRUE, value = val1, meta = NULL))
    }

    # C) Pattern fix for {\\"key\\":...} created by blob extraction
    if (grepl("^\\s*\\{\\\\\"", s)) {
        s_fix <- gsub('\\\\\\"', '"', s, perl = TRUE)
        val2  <- parse_try(s_fix, simplify = FALSE)
        if (!inherits(val2, "try-error") && is.list(val2) && !is.null(names(val2))) {
            if (isTRUE(verbose)) message("Row ", if (is.null(i)) "" else i, ": fixed over-escaped key quotes")
            return(list(ok = TRUE, value = val2, meta = NULL))
        }
    }

    # D) Last resort: extract first {...} or [...] again and try
    m <- regexpr("\\{[\\s\\S]*\\}|\\[[\\s\\S]*\\]", s, perl = TRUE)
    if (m[1] != -1L) {
        s2  <- substr(s, m[1], m[1] + attr(m, "match.length") - 1L)
        val3 <- parse_try(s2, simplify = FALSE)
        if (!inherits(val3, "try-error") && is.list(val3) && !is.null(names(val3))) {
            return(list(ok = TRUE, value = val3, meta = NULL))
        }
    }

    meta <- data.frame(stage = "parse",
                       note  = "not_named_object_or_parse_failed",
                       stringsAsFactors = FALSE)
    list(ok = FALSE, value = NULL, meta = meta)
}



