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
    escape_rx <- function(z) gsub("([\\[\\]{}()+*.^$?|\\\\])", "\\\\\\1", z, perl = TRUE)
    na_vals_pattern <- function(vals) {
        vals <- vals[vals != ""]
        paste(vapply(vals, escape_rx, character(1)), collapse = "|")
    }

    # If x itself is a JSON string *containing* JSON, unwrap it first
    if (is.character(x) && length(x) == 1L) {
        inner_try <- try(jsonlite::fromJSON(x, simplifyVector = TRUE), silent = TRUE)
        if (!inherits(inner_try, "try-error") && is.character(inner_try) && length(inner_try) == 1L) {
            x <- inner_try
        }
    }


    log <- character(0)
    if (is.null(x) || !nzchar(x)) return(list(txt = "", log = log))

    # Pass 0: light normalization
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

    ## Early parse (with unwrap of JSON string-literal)
    # Early parse (with unwrap for JSON string-literal)
    parsed1 <- tryCatch(jsonlite::fromJSON(s, simplifyVector = FALSE), error = function(e) e)

    if (!inherits(parsed1, "error")) {
        if (is.character(parsed1) && length(parsed1) == 1L) {
            inner <- trimws(parsed1)
            if ((startsWith(inner, "{") && grepl("}\\s*$", inner)) ||
                (startsWith(inner, "[") && grepl("\\]\\s*$", inner))) {
                parsed2 <- tryCatch(jsonlite::fromJSON(inner, simplifyVector = TRUE), error = function(e) NULL)
                if (!is.null(parsed2)) {
                    log <- c(log, "unwrapped-json-string")
                    return(list(txt = inner, log = log))
                }
            }
        } else {
            return(list(txt = s, log = log))  # already a bare JSON object/array
        }
    }


    # Pass 1: conservative repairs
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

    parsed <- tryCatch(jsonlite::fromJSON(s1c, simplifyVector = TRUE), error = function(e) NULL)
    if (!is.null(parsed)) return(list(txt = s1c, log = log))

    # Pass 2: aggressive (single->double quotes only when no double quotes present)
    if (aggressive && !grepl('"', s1c, fixed = TRUE) && grepl("'", s1c, fixed = TRUE)) {
        s2 <- gsub("'", '"', s1c, fixed = TRUE)
        if (!identical(s2, s1c)) log <- c(log, "single quotes -> double quotes")
        parsed <- tryCatch(jsonlite::fromJSON(s2, simplifyVector = TRUE), error = function(e) NULL)
        if (!is.null(parsed)) return(list(txt = s2, log = log))
    }

    list(txt = s1c, log = c(log, "unparsed-after-repair"))
}

`%+%` <- function(a,b) paste0(a,b)

# ---- %||% -----
#' @importFrom rlang %||%
NULL


