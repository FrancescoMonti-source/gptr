# Link to openai platform documentation
browse_openai_documentation = function(url = "https://platform.openai.com/docs/"){
    browseURL(url)
}

#' Null-coalescing operator
#' @keywords internal
`%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0) y else x
}

match_arg_tol <- function(x, choices, several.ok = FALSE, max.distance = .2) {
    match_vec <- vapply(x, function(k) {
        matched <- agrep(k, choices, value = TRUE, ignore.case = TRUE, max.distance = max.distance)
        if (length(matched) == 1) matched else NA_character_
    }, character(1))
    return(match_vec)
}


# interpret a key spec: either a type string OR a vector of allowed values
parse_key_spec <- function(spec) {
    types <- c("integer","numeric","character","logical")
    if (is.character(spec) && length(spec) == 1L && spec %in% types) {
        list(type = spec, allowed = NULL)
    } else {
        # treat as allowed set; keep as-is so character/numeric/logical sets work
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

# =========================
# utils.R
# =========================

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
tidy_json <- function(x,
                      na_values = c("NA", "null", "", "[]", "{}", "None"),
                      aggressive = FALSE) {

    # --- internal helpers (not exported) ---
    strip_fences <- function(s) {
        # remove starting/ending ```... fences if present
        s <- gsub("^\\s*```[a-zA-Z0-9_-]*\\s*", "", s)
        gsub("```\\s*$", "", s)
    }
    extract_json_blob <- function(s) {
        # first {...} else first [...]
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
    escape_rx <- function(z) gsub("([\\[\\]{}()+*.^$?|\\\\])", "\\\\\\1", z, perl = TRUE)
    na_vals_pattern <- function(vals) {
        vals <- vals[vals != ""]
        paste(vapply(vals, escape_rx, character(1)), collapse = "|")
    }

    log <- character(0)
    if (is.null(x) || !nzchar(x)) return(list(txt = "", log = log))

    # Pass 0: light normalization
    s <- sub("^\ufeff", "", x, perl = TRUE)        # strip BOM
    s <- strip_fences(s)
    # collapse over-escaped quotes (\\\" -> \")
    s <- gsub('\\\\\\\\"', '\\"', s, perl = TRUE)
    # normalize smart quotes
    s <- gsub('[\u201c\u201d\u201e\u201f\u2033\u2036]', '"', s, perl = TRUE)
    s <- gsub('[\u2018\u2019\u2032\u2035]', "'",  s, perl = TRUE)
    # normalize common tokens
    s <- gsub("\\bTrue\\b",  "true",  s)
    s <- gsub("\\bFalse\\b", "false", s)
    s <- gsub("\\bNone\\b",  "null",  s)
    # whitespace
    s <- gsub("[\r\n\t]", " ", s, perl = TRUE)
    s <- trimws(s)
    # isolate the first JSON blob if surrounded by chatter
    s <- extract_json_blob(s)
    log <- c(log, "cleaned")

    # Early parse attempt
    parsed <- tryCatch(jsonlite::fromJSON(s, simplifyVector = TRUE), error = function(e) NULL)
    if (!is.null(parsed)) return(list(txt = s, log = log))

    # Pass 1: conservative repairs

    # Quote bare NA-like tokens after a colon:  "key: NA," -> "key: \"NA\","
    re_vals <- na_vals_pattern(na_values)
    s2 <- gsub(paste0(":(\\s*)(", re_vals, ")(\\s*[,}])"),
               ': "NA"\\3', s, ignore.case = TRUE, perl = TRUE)

    # Strip trailing commas before } or ]
    s2 <- gsub(",\\s*([}\\]])", "\\1", s2, perl = TRUE)

    # Quote bare object keys: { key: ... } -> { "key": ... }
    s2q <- gsub("([{,]\\s*)([A-Za-z_][A-Za-z0-9_]*)(\\s*:)", '\\1"\\2"\\3', s2, perl = TRUE)

    parsed <- tryCatch(jsonlite::fromJSON(s2q, simplifyVector = TRUE), error = function(e) NULL)
    if (!is.null(parsed)) {
        return(list(txt = s2q, log = c(log, "quoted bare NA", "removed trailing comma", "quoted bare keys")))
    }

    # Pass 2: aggressive (optional) — convert single quotes to double quotes when no double quotes present
    if (aggressive && !grepl('"', s2q, fixed = TRUE) && grepl("'", s2q, fixed = TRUE)) {
        s3 <- gsub("'", '"', s2q, fixed = TRUE)
        parsed <- tryCatch(jsonlite::fromJSON(s3, simplifyVector = TRUE), error = function(e) NULL)
        if (!is.null(parsed)) {
            return(list(txt = s3, log = c(log, "single quotes -> double quotes")))
        }
    }

    # Still unparsed; return best-effort string and log
    list(txt = s2q, log = c(log, "unparsed-after-repair"))
}
