#' Repairs: { "text_rewritten": "<long messy text>" }
#' @keywords Internal
.json_last_resort <- function(raw) {
    stopifnot(is.character(raw), length(raw) == 1L)
    s <- trimws(raw)

    # 1) first {...} blob
    m <- regexpr("\\{[\\s\\S]*\\}", s, perl = TRUE)
    if (m[1] == -1L) stop("No JSON object found.")
    s <- substr(s, m[1], m[1] + attr(m, "match.length") - 1L)

    # 2) normalize opener
    if (!grepl('"text_rewritten"\\s*:\\s*"', s, perl = TRUE)) {
        s <- sub('"text_rewritten"\\s*:\\s*', '"text_rewritten": "', s, perl = TRUE)
    }

    # 3) locate value quotes
    k <- regexpr('"text_rewritten"\\s*:\\s*"', s, perl = TRUE)
    val_start <- k + attr(k, "match.length")
    qpos <- gregexpr('"', s, fixed = TRUE)[[1]]
    rbrace <- tail(gregexpr("}", s, fixed = TRUE)[[1]], 1)
    last_q <- max(qpos[qpos < rbrace])
    if (!is.finite(last_q) || last_q <= val_start) {
        s <- sub("\\}\\s*$", "\"}", s)
        qpos <- gregexpr('"', s, fixed = TRUE)[[1]]
        rbrace <- tail(gregexpr("}", s, fixed = TRUE)[[1]], 1)
        last_q <- max(qpos[qpos < rbrace])
    }
    val <- substr(s, val_start, last_q - 1L)

    # 4) clean inside the value
    val <- gsub('\\s*"\\s*"\\s*', " ", val, perl = TRUE) # " " --> space
    val <- gsub('\\s*""\\s*', " ", val, perl = TRUE) # ""  --> space
    val <- gsub("\\bIF\\s*<>\\b", "", val, perl = TRUE) # remove IF <>
    val <- gsub("\\s*\\*\\s*", " ", val, perl = TRUE) # remove lone *
    val <- gsub('\\\\(?!["\\\\/bfnrtu])', "", val, perl = TRUE) # kill invalid escapes (e.g. \~)
    val <- gsub("[ \t]+", " ", val, perl = TRUE)
    val <- gsub(" *\\n *", "\\n", val, perl = TRUE)
    val <- trimws(val)
    val <- gsub('(?<!\\\\)"', '\\"', val, perl = TRUE) # escape any naked "

    fixed <- paste0('{ "text_rewritten": "', val, '" }')
    if (!jsonlite::validate(fixed)) stop("Still invalid after repair.")
    jsonlite::fromJSON(fixed, simplifyVector = TRUE)
}

#' Clean + repair model JSON output in a safe, staged way.
#' Returns a list(txt = <string>, log = <character vector of applied fixes>)
#' @keywords internal
.tidy_json <- function(x,
                       na_values = c("NA", "null", "", "[]", "{}", "None"),
                       aggressive = FALSE) {
    strip_fences <- function(s) {
        s <- gsub("^\\s*```[a-zA-Z0-9_-]*\\s*", "", s)
        gsub("```\\s*$", "", s)
    }

    extract_json_blob <- function(s) {
        m1 <- regexpr("\\{[\\s\\S]*\\}", s, perl = TRUE)
        if (m1[1] != -1L) {
            return(substr(s, m1[1], m1[1] + attr(m1, "match.length") - 1L))
        }
        m2 <- regexpr("\\[[\\s\\S]*\\]", s, perl = TRUE)
        if (m2[1] != -1L) {
            return(substr(s, m2[1], m2[1] + attr(m2, "match.length") - 1L))
        }
        s
    }

    has_odd_unescaped_quotes <- function(z) {
        stopifnot(is.character(z))
        if (length(z) != 1L) z <- z[[1L]]
        qpos <- gregexpr('"', z, fixed = TRUE)[[1]]
        if (identical(qpos, -1L)) {
            return(FALSE)
        }
        is_unesc <- function(i) if (i <= 1L) TRUE else substr(z, i - 1L, i - 1L) != "\\"
        sum(vapply(qpos, is_unesc, logical(1))) %% 2L == 1L
    }

    close_final_string_before_brace <- function(z) {
        stopifnot(is.character(z))
        if (length(z) != 1L) z <- z[[1L]]
        if (!grepl("\\}\\s*$", z)) {
            return(z)
        }
        if (!has_odd_unescaped_quotes(z)) {
            return(z)
        }
        pre <- sub("\\}\\s*$", "", z)
        if (grepl('\"\\s*$', pre)) {
            return(z)
        }
        sub("\\}\\s*$", "\"}", z)
    }

    escape_rx <- function(z) gsub("([\\[\\]{}()+*.^$?|\\\\])", "\\\\\\1", z, perl = TRUE)
    na_vals_pattern <- function(vals) {
        vals <- vals[vals != ""]
        paste(vapply(vals, escape_rx, character(1)), collapse = "|")
    }

    log <- character(0)
    if (is.null(x) || !nzchar(x)) {
        return(list(txt = "", log = log))
    }

    # Normalize and strip wrappers
    s0 <- x
    s <- sub("^\ufeff", "", x, perl = TRUE)
    s <- strip_fences(s)
    s <- gsub('\\\\\\\\"', '\\"', s, perl = TRUE) # over-escaped quotes
    s <- gsub("[\u201c\u201d\u201e\u201f\u2033\u2036]", '"', s, perl = TRUE)
    s <- gsub("[\u2018\u2019\u2032\u2035]", "'", s, perl = TRUE)
    s <- gsub("\\bTrue\\b", "true", s)
    s <- gsub("\\bFalse\\b", "false", s)
    s <- gsub("\\bNone\\b", "null", s)
    s <- gsub("[\r\n\t]", " ", s, perl = TRUE)
    s <- trimws(s)
    s <- extract_json_blob(s)
    if (!identical(s, s0)) log <- c(log, "cleaned")

    # Early unwrap of JSON string literal
    parsed1 <- tryCatch(jsonlite::fromJSON(s, simplifyVector = FALSE), error = function(e) e)
    if (!inherits(parsed1, "error")) {
        if (is.character(parsed1) && length(parsed1) == 1L) {
            inner <- trimws(parsed1)
            if ((startsWith(inner, "{") && grepl("\\}\\s*$", inner)) ||
                (startsWith(inner, "[") && grepl("\\]\\s*$", inner))) {
                parsed2 <- tryCatch(jsonlite::fromJSON(inner, simplifyVector = TRUE), error = function(e) NULL)
                if (!is.null(parsed2)) {
                    log <- c(log, "unwrapped-json-string")
                    return(list(txt = inner, log = log))
                }
            }
        } else {
            return(list(txt = s, log = log))
        }
    }

    # Conservative repairs
    re_vals <- na_vals_pattern(na_values)

    s1 <- s
    # quote bare NA-like
    s1a <- gsub(paste0(":(\\s*)(", re_vals, ")(\\s*[,}])"),
                ': "NA"\\3', s1,
                ignore.case = TRUE, perl = TRUE
    )
    if (!identical(s1a, s1)) log <- c(log, "quoted bare NA")

    # strip trailing commas
    s1b <- gsub(",\\s*([}\\]])", "\\1", s1a, perl = TRUE)
    if (!identical(s1b, s1a)) log <- c(log, "removed trailing comma")

    # quote bare keys
    s1c <- gsub("([{,]\\s*)([A-Za-z_][A-Za-z0-9_]*)(\\s*:)", '\\1"\\2"\\3', s1b, perl = TRUE)
    if (!identical(s1c, s1b)) log <- c(log, "quoted bare keys")

    # --- Extra repairs ---
    # Fix over-escaped key quotes
    s_keys_fixed <- gsub('([{,]\\s*)\\\\"([A-Za-z_][A-Za-z0-9_\\-]*)\\\\"(\\s*:)',
                         '\\1"\\2"\\3', s1c,
                         perl = TRUE
    )
    if (!identical(s_keys_fixed, s1c)) log <- c(log, "unescaped key quotes")

    # Remove invalid backslash escapes (e.g. \*)
    s_invalid_esc <- gsub('\\\\(?!["\\\\/bfnrtu])', "", s_keys_fixed, perl = TRUE)
    if (!identical(s_invalid_esc, s_keys_fixed)) log <- c(log, "removed invalid escapes")

    # Close final string if needed
    s_closed <- close_final_string_before_brace(s_invalid_esc)
    if (!identical(s_closed, s_invalid_esc)) log <- c(log, "closed trailing string before }")

    parsed <- tryCatch(jsonlite::fromJSON(s_closed, simplifyVector = TRUE), error = function(e) NULL)
    if (!is.null(parsed)) {
        return(list(txt = s_closed, log = log))
    }

    # Aggressive fallback: single->double quotes
    if (aggressive && !grepl('"', s_closed, fixed = TRUE) && grepl("'", s_closed, fixed = TRUE)) {
        s2 <- gsub("'", '"', s_closed, fixed = TRUE)
        if (!identical(s2, s_closed)) log <- c(log, "single quotes -> double quotes")
        parsed <- tryCatch(jsonlite::fromJSON(s2, simplifyVector = TRUE), error = function(e) NULL)
        if (!is.null(parsed)) {
            return(list(txt = s2, log = log))
        }
    }

    list(txt = s_closed, log = c(log, "unparsed-after-repair"))
}


#' Fix, parse, and (optionally) validate/coerce a JSON string (one row)
#'
#' 1) Fix common JSON issues with .tidy_json(); 2) parse via jsonlite; 3) validate
#'    and optionally coerce values according to key_specs.
#'
#' @param out Raw model output (character).
#' @param key_specs Named list of specs from .parse_key_spec() or NULL.
#' @param na_values Character vector treated as NA.
#' @param verbose Logical; print repair notes and validation issues.
#' @param i Optional row index for logging (for messages).
#' @param .coerce_types Logical; if TRUE, coerce values by spec$type (default TRUE).
#' @param coerce_when Optional predicate function(key, value, spec, row_index) -> TRUE/FALSE,
#'   overrides .coerce_types per value when provided.
#' @return list(ok = TRUE/FALSE, value = parsed list or raw string, note = "parsed"/"not-json"/"NA-like")
#' @export
json_fix_parse_validate <- function(text,
                                    key_specs = NULL,
                                    na_values = c("NA", "N/A", "null", "None", ""),
                                    verbose = FALSE,
                                    i = NULL,
                                    .coerce_types = FALSE,
                                    coerce_when = NULL) {
  looks_like_json <- function(s) {
    s <- trimws(s)
    (startsWith(s, "{") && grepl("\\}\\s*$", s)) || (startsWith(s, "[") && grepl("\\]\\s*$", s))
  }
  parse_try <- function(z, simplify = FALSE) {
    try(jsonlite::fromJSON(z, simplifyVector = simplify), silent = TRUE)
  }

  # A) Try RAW text first (pre-tidy): unwrap quoted JSON string literal
  # removed, baked into .tidy_json


  # B) Light cleanup via .tidy_json()
  tj <- .tidy_json(text, na_values = na_values)
  s <- tj$txt
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
    val2 <- parse_try(s_fix, simplify = FALSE)
    if (!inherits(val2, "try-error") && is.list(val2) && !is.null(names(val2))) {
      if (isTRUE(verbose)) message("Row ", if (is.null(i)) "" else i, ": fixed over-escaped key quotes")
      return(list(ok = TRUE, value = val2, meta = NULL))
    }
  }

  # D) extract first {...} or [...] again and try
  m <- regexpr("\\{[\\s\\S]*\\}|\\[[\\s\\S]*\\]", s, perl = TRUE)
  if (m[1] != -1L) {
    s2 <- substr(s, m[1], m[1] + attr(m, "match.length") - 1L)
    val3 <- parse_try(s2, simplify = FALSE)
    if (!inherits(val3, "try-error") && is.list(val3) && !is.null(names(val3))) {
      return(list(ok = TRUE, value = val3, meta = NULL))
    }
  }

  # E) last resord
  val4 <- try(.json_last_resort(s), silent = TRUE)
  if (!inherits(val4, "try-error") && is.list(val4)) {
    return(list(ok = TRUE, value = val4, meta = NULL))
  }

  meta <- data.frame(
    stage = "parse",
    note = "not_named_object_or_parse_failed",
    stringsAsFactors = FALSE
  )
  list(ok = FALSE, value = NULL, meta = meta)
}
