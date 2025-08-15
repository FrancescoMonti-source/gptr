#' Align parsed JSON keys to the expected schema (configurable fuzzy match)
#'
#' @param x Named list of parsed values from the LLM.
#' @param expected_keys Character vector of expected key names.
#' @param auto_correct Logical; fuzzy-correct unexpected names (default TRUE).
#' @param fuzzy_model One of:
#'   - "lev_ratio": compare edit distance as a proportion of target length.
#'   - "lev": compare raw edit distance (absolute number of edits).
#' @param fuzzy_threshold Numeric; interpretation depends on `fuzzy_model`:
#'   - if "lev_ratio": a proportion in [0,1], e.g. 0.3.
#'   - if "lev": an integer number of edits, e.g. 2.
#' @param keep_unexpected Logical; keep keys not in `expected_keys`.
#' @return Named list over expected_keys (plus extras if kept).
#' @export
json_keys_align <- function(x,
                            expected_keys,
                            auto_correct    = TRUE,
                            fuzzy_model     = c("lev_ratio", "lev"),
                            fuzzy_threshold = 0.3,
                            keep_unexpected = FALSE) {
    if (is.null(expected_keys)) return(x)
    fuzzy_model <- match.arg(fuzzy_model)

    exp_keys_norm <- trimws(tolower(expected_keys))

    unique_best_match <- function(key, choices, choices_norm, fuzzy_model, fuzzy_threshold) {
        if (is.na(key)) return(NA_character_)
        key_norm <- trimws(tolower(key))

        pos <- match(key_norm, choices_norm)
        if (!is.na(pos)) return(choices[pos])

        d <- adist(key_norm, choices_norm)
        best <- which.min(d)
        if (length(best) == 0L) return(NA_character_)
        dist_best <- d[best]

        ok <- if (fuzzy_model == "lev_ratio") {
            # Allow up to ceil(ratio * target_length) edits
            dist_best <= ceiling(fuzzy_threshold * nchar(choices_norm[best]))
        } else { # "lev"
            dist_best <= as.integer(fuzzy_threshold)
        }

        if (sum(d == dist_best) == 1L && ok) choices[best] else NA_character_
    }

    if (isTRUE(auto_correct) && !setequal(names(x), expected_keys)) {
        new_names <- vapply(
            names(x),
            unique_best_match,
            character(1),
            choices         = expected_keys,
            choices_norm    = exp_keys_norm,
            fuzzy_model     = fuzzy_model,
            fuzzy_threshold = fuzzy_threshold
        )
        names(x)[!is.na(new_names)] <- new_names[!is.na(new_names)]
    }

    if (!isTRUE(keep_unexpected)) {
        x <- x[intersect(expected_keys, names(x))]
    }

    out <- setNames(vector("list", length(expected_keys)), expected_keys)
    for (k in expected_keys) out[[k]] <- if (k %in% names(x)) x[[k]] else NA

    if (isTRUE(keep_unexpected)) {
        extra <- setdiff(names(x), expected_keys)
        for (e in extra) out[[e]] <- x[[e]]
    }
    out
}
