#' Coalescing helpers for GPTR
#'
#' Coalesce values to a default under different “emptiness” definitions.
#' These helpers complement the use of [`rlang::%||%`]. Use [`rlang::%||%`] for NULL-only coalescing. Use these helpers when``
#' upstream code (APIs, JSON parsers) sometimes returns length-0 vectors or
#' when you explicitly want to treat the empty string as “unset”.
#'
#' @section Guidelines:
#' - Prefer rlang::`%||%` for options/env and most internals.
#' - Use [coalesce_len0()] when you might get `character(0)` or other length-0 vectors.
#' - Use [coalesce_blank()] **only** for user-configurable strings where "" should mean “use default”.
#'
#' @examples
#' # NULL-only (handled by rlang::%||%):
#' NULL %||% "default"        # "default"
#' "x"  %||% "default"        # "x"
#'
#' # Length-0 coalescing:
#' coalesce_len0(character(0), "fallback")  # "fallback"
#' coalesce_len0(numeric(0),  42)           # 42
#' coalesce_len0("x",         "fallback")   # "x"
#'
#' # Blank-string coalescing:
#' coalesce_blank("",      "fallback")      # "fallback"
#' coalesce_blank("  ",    "fallback")      # "  " (not trimmed!)
#' coalesce_blank("text",  "fallback")      # "text"
#' coalesce_blank(NULL,    "fallback")      # "fallback"
#' coalesce_blank(character(0), "fallback") # "fallback"
#' @name gptr-coalesce
NULL

#' Coalesce NULL or length-0 vectors to a default
#' @param x Any object.
#' @param default Value to return when x is NULL or length(x) == 0.
#' @return Either x or default.
#' @keywords internal
coalesce_len0 <- function(x, default) {
    if (is.null(x) || length(x) == 0L) default else x
}

#' Coalesce “blank” strings (NULL, character(0), or "")
#'
#' Treats NULL, character(0), or a single empty string "" as empty.
#' Does **not** trim whitespace; "  " is considered present.
#'
#' @param x A value (often a character scalar).
#' @param default Value to return when x is considered blank.
#' @return Either x or default.
#' @keywords internal
coalesce_blank <- function(x, default) {
    if (is.null(x)) return(default)
    if (is.character(x) && length(x) == 0L) return(default)
    if (is.character(x) && length(x) == 1L && !nzchar(x)) return(default)
    x
}


