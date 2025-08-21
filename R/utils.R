# Link to openai platform documentation
browse_openai_documentation = function(url = "https://platform.openai.com/docs/"){
    browseURL(url)
}



match_arg_tol <- function(x, choices, several.ok = FALSE, max.distance = .2) {
    match_vec <- vapply(x, function(k) {
        matched <- agrep(k, choices, value = TRUE, ignore.case = TRUE, max.distance = max.distance)
        if (length(matched) == 1) matched else NA_character_
    }, character(1))
    return(match_vec)
}


#' Interpret a key spec (type string or allowed set)
#' @param spec Either a type string ("integer","numeric","character","logical")
#'   or a vector of allowed values.
#' @return list(type = <chr|NULL>, allowed = <vector|NULL>)
#' @export
parse_key_spec <- function(spec) {
    types <- c("integer","numeric","character","logical")
    if (is.character(spec) && length(spec) == 1L && spec %in% types) {
        list(type = spec, allowed = NULL)
    } else {
        list(type = NULL, allowed = spec)
    }
}


# normalize a scalar token for comparisons
normalize_token <- function(x) {
    if (is.null(x)) return(NA_character_)
    if (is.logical(x) && length(x) == 1L) return(tolower(as.character(x)))
    if (is.numeric(x) && length(x) == 1L) return(as.character(x))
    if (is.character(x) && length(x) == 1L) return(trimws(x))
    if (length(x) == 1L) return(as.character(x))
    NA_character_
}

# coerce a value to a requested type
coerce_type <- function(value, type) {
    if (type == "integer")  return(suppressWarnings(as.integer(value)))
    if (type == "numeric")  return(suppressWarnings(as.numeric(value)))
    if (type == "logical") {
        # accept true/false/0/1/"true"/"false"
        if (is.character(value)) {
            v <- tolower(trimws(value))
            if (v %in% c("true","1"))  return(TRUE)
            if (v %in% c("false","0")) return(FALSE)
            return(NA)
        }
        return(as.logical(value))
    }
    # character
    if (is.null(value)) return(NA_character_)
    as.character(value)
}

# check if a scalar belongs to an allowed set (case-insensitive for character)
in_allowed <- function(value, allowed) {
    if (is.null(allowed)) return(TRUE)
    if (length(allowed) == 0L) return(TRUE)
    v <- normalize_token(value)
    if (is.na(v)) return(FALSE)

    if (is.character(allowed)) {
        any(tolower(v) == tolower(trimws(allowed)))
    } else {
        # numeric/integer/logical sets: compare as characterized scalars
        any(v == vapply(allowed, normalize_token, character(1)))
    }
}


# Determine if a scalar should be treated as NA (case-insensitive for characters)
is_na_like <- function(x, na_vals = c("NA", "null", "", "[]", "{}", "None")) {
    if (is.null(x)) return(TRUE)
    if (length(x) != 1L) return(FALSE)
    if (is.character(x)) {
        xv <- trimws(tolower(x))
        return(xv %in% trimws(tolower(na_vals)))
    }
    FALSE
}


# Clean + repair model JSON output in a safe, staged way.
# Returns a list(txt = <string>, log = <character vector of applied fixes>)
#' @export
tidy_json <- function(x,
                      na_values = c("NA", "null", "", "[]", "{}", "None"),
                      aggressive = FALSE) {

    strip_fences <- function(s) {
        s <- gsub("^\\s*```[a-zA-Z0-9_-]*\\s*", "", s)
        gsub("```\\s*$", "", s)
    }

    extract_json_blob <- function(s) {
        m1 <- regexpr("\\{[\\s\\S]*\\}", s, perl = TRUE)
        if (m1[1] != -1L) return(substr(s, m1[1], m1[1] + attr(m1, "match.length") - 1L))
        m2 <- regexpr("\\[[\\s\\S]*\\]", s, perl = TRUE)
        if (m2[1] != -1L) return(substr(s, m2[1], m2[1] + attr(m2, "match.length") - 1L))
        s
    }

    has_odd_unescaped_quotes <- function(z) {
        stopifnot(is.character(z))
        if (length(z) != 1L) z <- z[[1L]]
        qpos <- gregexpr('"', z, fixed = TRUE)[[1]]
        if (identical(qpos, -1L)) return(FALSE)
        is_unesc <- function(i) if (i <= 1L) TRUE else substr(z, i - 1L, i - 1L) != "\\"
        sum(vapply(qpos, is_unesc, logical(1))) %% 2L == 1L
    }

    close_final_string_before_brace <- function(z) {
        stopifnot(is.character(z))
        if (length(z) != 1L) z <- z[[1L]]
        if (!grepl("\\}\\s*$", z)) return(z)
        if (!has_odd_unescaped_quotes(z)) return(z)
        pre <- sub("\\}\\s*$", "", z)
        if (grepl('\"\\s*$', pre)) return(z)
        sub("\\}\\s*$", "\"}", z)
    }

    escape_rx <- function(z) gsub("([\\[\\]{}()+*.^$?|\\\\])", "\\\\\\1", z, perl = TRUE)
    na_vals_pattern <- function(vals) {
        vals <- vals[vals != ""]
        paste(vapply(vals, escape_rx, character(1)), collapse = "|")
    }

    log <- character(0)
    if (is.null(x) || !nzchar(x)) return(list(txt = "", log = log))

    # Normalize and strip wrappers
    s0 <- x
    s  <- sub("^\ufeff", "", x, perl = TRUE)
    s  <- strip_fences(s)
    s  <- gsub('\\\\\\\\"', '\\"', s, perl = TRUE)  # over-escaped quotes
    s  <- gsub('[\u201c\u201d\u201e\u201f\u2033\u2036]', '"', s, perl = TRUE)
    s  <- gsub('[\u2018\u2019\u2032\u2035]', "'",  s, perl = TRUE)
    s  <- gsub("\\bTrue\\b",  "true",  s)
    s  <- gsub("\\bFalse\\b", "false", s)
    s  <- gsub("\\bNone\\b",  "null",  s)
    s  <- gsub("[\r\n\t]", " ", s, perl = TRUE)
    s  <- trimws(s)
    s  <- extract_json_blob(s)
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
                ': "NA"\\3', s1, ignore.case = TRUE, perl = TRUE)
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
                         '\\1"\\2"\\3', s1c, perl = TRUE)
    if (!identical(s_keys_fixed, s1c)) log <- c(log, "unescaped key quotes")

    # Remove invalid backslash escapes (e.g. \*)
    s_invalid_esc <- gsub('\\\\(?!["\\\\/bfnrtu])', '', s_keys_fixed, perl = TRUE)
    if (!identical(s_invalid_esc, s_keys_fixed)) log <- c(log, "removed invalid escapes")

    # Close final string if needed
    s_closed <- close_final_string_before_brace(s_invalid_esc)
    if (!identical(s_closed, s_invalid_esc)) log <- c(log, "closed trailing string before }")

    parsed <- tryCatch(jsonlite::fromJSON(s_closed, simplifyVector = TRUE), error = function(e) NULL)
    if (!is.null(parsed)) return(list(txt = s_closed, log = log))

    # Aggressive fallback: single->double quotes
    if (aggressive && !grepl('"', s_closed, fixed = TRUE) && grepl("'", s_closed, fixed = TRUE)) {
        s2 <- gsub("'", '"', s_closed, fixed = TRUE)
        if (!identical(s2, s_closed)) log <- c(log, "single quotes -> double quotes")
        parsed <- tryCatch(jsonlite::fromJSON(s2, simplifyVector = TRUE), error = function(e) NULL)
        if (!is.null(parsed)) return(list(txt = s2, log = log))
    }

    list(txt = s_closed, log = c(log, "unparsed-after-repair"))
}


# Repairs: { "text_rewritten": "<long messy text>" }
repair_json4 <- repair_text_rewritten_json <- function(raw) {
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
    val <- gsub('\\s*"\\s*"\\s*', " ", val, perl = TRUE)    # " " → space
    val <- gsub('\\s*""\\s*',       " ", val, perl = TRUE)  # ""  → space
    val <- gsub("\\bIF\\s*<>\\b",   "",  val, perl = TRUE)  # remove IF <>
    val <- gsub("\\s*\\*\\s*",      " ", val, perl = TRUE)  # remove lone *
    val <- gsub('\\\\(?!["\\\\/bfnrtu])', '', val, perl = TRUE) # kill invalid escapes (e.g. \~)
    val <- gsub("[ \t]+",           " ", val, perl = TRUE)
    val <- gsub(" *\\n *",          "\\n", val, perl = TRUE)
    val <- trimws(val)
    val <- gsub('(?<!\\\\)"', '\\"', val, perl = TRUE)       # escape any naked "

    fixed <- paste0('{ "text_rewritten": "', val, '" }')
    if (!jsonlite::validate(fixed)) stop("Still invalid after repair.")
    jsonlite::fromJSON(fixed, simplifyVector = TRUE)
}





`%+%` <- function(a,b) paste0(a,b)

# ---- %||% -----
#' @importFrom rlang %||%
NULL


