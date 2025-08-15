#' Align parsed JSON keys to the expected schema
#'
#' Fuzzy-correct unexpected names (unique match only), optionally drop unexpected keys,
#' and fill missing expected keys with NA.
#'
#' @export
json_keys_align <- function(x,
                            expected_keys,
                            auto_correct = TRUE,
                            max_distance = 0.2,
                            keep_unexpected = FALSE) {
    if (is.null(expected_keys)) return(x)

    if (isTRUE(auto_correct) && !setequal(names(x), expected_keys)) {
        new_names <- vapply(names(x), function(k) {
            if (!is.na(k) && k %in% expected_keys) return(k)
            m <- agrep(k, expected_keys, value = TRUE,
                       ignore.case = TRUE, max.distance = max_distance)
            if (length(m) == 1L) m else NA_character_
        }, character(1))
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
