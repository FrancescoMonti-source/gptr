#' Align parsed JSON keys to the expected schema
#'
#' Fuzzy-correct unexpected names (unique match only), optionally drop unexpected keys,
#' and fill missing expected keys with NA.
#'
#' @param x Named list of parsed values from the LLM.
#' @param expected_keys Character vector of expected key names.
#' @param auto_correct Logical; fuzzy-correct unexpected names (default TRUE).
#' @param max_distance Numeric; max distance for agrep() (default 0.2).
#' @param keep_unexpected Logical; keep keys not in expected_keys.
#' @return Named list over expected_keys (plus extras if kept).
#' @export
json_keys_align <- function(x,
                            expected_keys,
                            auto_correct = TRUE,
                            max_distance = 0.2,
                            keep_unexpected = FALSE) {
    if (is.null(expected_keys)) return(x)

    # Fuzzy autocorrect (unique match only)
    if (isTRUE(auto_correct) && !setequal(names(x), expected_keys)) {
        new_names <- vapply(names(x), function(k) {
            if (!is.na(k) && k %in% expected_keys) return(k)
            m <- agrep(k, expected_keys, value = TRUE,
                       ignore.case = TRUE, max.distance = max_distance)
            if (length(m) == 1L) m else NA_character_
        }, character(1))
        names(x)[!is.na(new_names)] <- new_names[!is.na(new_names)]
    }

    # Drop unexpected, if requested
    if (!isTRUE(keep_unexpected)) {
        x <- x[intersect(expected_keys, names(x))]
    }

    # Fill missing expected keys with NA
    out <- setNames(vector("list", length(expected_keys)), expected_keys)
    for (k in expected_keys) {
        out[[k]] <- if (k %in% names(x)) x[[k]] else NA
    }

    # Optionally append extras (stable order)
    if (isTRUE(keep_unexpected)) {
        extra <- setdiff(names(x), expected_keys)
        for (e in extra) out[[e]] <- x[[e]]
    }

    out
}
