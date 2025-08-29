# Link to openai platform documentation
browse_openai_documentation <- function(url = "https://platform.openai.com/docs/") {
  browseURL(url)
}


#' Interpret a key spec (type string or allowed set)
#' @param spec Either a type string ("integer","numeric","character","logical")
#'   or a vector of allowed values.
#' @return list(type = <chr|NULL>, allowed = <vector|NULL>)
#' @keywords internal
.parse_key_spec <- function(spec) {
  types <- c("integer", "numeric", "character", "logical")
  if (is.character(spec) && length(spec) == 1L && spec %in% types) {
    list(type = spec, allowed = NULL)
  } else {
    list(type = NULL, allowed = spec)
  }
}


#' normalize a scalar token for comparisons
#' @keywords internal
.normalize_token <- function(x) {
  if (is.null(x)) {
    return(NA_character_)
  }
  if (is.logical(x) && length(x) == 1L) {
    return(tolower(as.character(x)))
  }
  if (is.numeric(x) && length(x) == 1L) {
    return(as.character(x))
  }
  if (is.character(x) && length(x) == 1L) {
    return(trimws(x))
  }
  if (length(x) == 1L) {
    return(as.character(x))
  }
  NA_character_
}

#' coerce a value to a requested type
#' @keywords internal
.coerce_type <- function(value, type) {
  if (type == "integer") {
    return(suppressWarnings(as.integer(value)))
  }
  if (type == "numeric") {
    return(suppressWarnings(as.numeric(value)))
  }
  if (type == "logical") {
    # accept true/false/0/1/"true"/"false"
    if (is.character(value)) {
      v <- tolower(trimws(value))
      if (v %in% c("true", "1")) {
        return(TRUE)
      }
      if (v %in% c("false", "0")) {
        return(FALSE)
      }
      return(NA)
    }
    return(as.logical(value))
  }
  # character
  if (is.null(value)) {
    return(NA_character_)
  }
  as.character(value)
}

#' check if a scalar belongs to an allowed set (case-insensitive for character)
#' @keywords internal
.in_allowed <- function(value, allowed) {
  if (is.null(allowed)) {
    return(TRUE)
  }
  if (length(allowed) == 0L) {
    return(TRUE)
  }
  v <- .normalize_token(value)
  if (is.na(v)) {
    return(FALSE)
  }

  if (is.character(allowed)) {
    any(tolower(v) == tolower(trimws(allowed)))
  } else {
    # numeric/integer/logical sets: compare as characterized scalars
    any(v == vapply(allowed, .normalize_token, character(1)))
  }
}


#' Determine if a scalar should be treated as NA (case-insensitive for characters)
#' @keywords internal
.is_na_like <- function(x, na_vals = c("NA", "null", "", "[]", "{}", "None")) {
  if (is.null(x)) {
    return(TRUE)
  }
  if (length(x) != 1L) {
    return(FALSE)
  }
  if (is.character(x)) {
    xv <- trimws(tolower(x))
    return(xv %in% trimws(tolower(na_vals)))
  }
  FALSE
}

#' Show current gptr package options
#'
#' Prints all options that start with "gpt." along with their current values.
#' This is useful to inspect defaults set in `zzz.R` and any overrides applied
#' in your `.Rprofile` or current session.
#'
#' In R, to change an option, you call options() with a name = value pair:
#' options(gpt.lmstudio_base_url = "http://localhost:5678/v1/chat/completions")

#' @return Invisibly returns a named list of package options.
#' @examples
#' show_gptr_options()
#' @export
show_gptr_options <- function() {
    opts <- options()
    gpt_opts <- opts[grep("^gptr\\.", names(opts))]
    if (length(gpt_opts) == 0) {
        cli::cli_warn("No gpt.* options are currently set.")
        return(invisible(list()))
    }
    cli::cli_inform("Current gptr package options:")
    print(gpt_opts)
    invisible(gpt_opts)
}

#' Coalesce operator (re-export from rlang)
#' See [rlang::%||%].
#' @name %||%
#' @rdname coalesce-operator
#' @export
#' @importFrom rlang %||%
NULL


#' @importFrom stats na.omit runif setNames
#' @importFrom utils adist browseURL head tail
NULL
