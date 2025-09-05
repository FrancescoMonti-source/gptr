#' list_models() helprer
#'  internal
#'  base_url_normalized Logical; set TRUE when base_url is already normalized
.as_models_df <- function(x) {
    if (is.null(x)) {
        return(data.frame(id = character(0), created = numeric(0), stringsAsFactors = FALSE))
    }
    if (is.data.frame(x)) {
        if (!"id" %in% names(x)) {
            return(data.frame(id = character(0), created = numeric(0), stringsAsFactors = FALSE))
        }
        if (!"created" %in% names(x)) x$created <- NA_real_
        x$created <- suppressWarnings(as.numeric(x$created))
        return(x[, c("id", "created"), drop = FALSE])
    }
    if (is.atomic(x) && !is.null(names(x)) && length(x)) {
        ids <- names(x)
        cr <- suppressWarnings(as.numeric(unname(x)))
        return(data.frame(id = as.character(ids), created = cr, stringsAsFactors = FALSE))
    }
    if (is.list(x) && length(x)) {
        if (!is.null(names(x)) && all(vapply(x, function(e) length(e) == 1, logical(1)))) {
            ids <- names(x)
            cr <- suppressWarnings(as.numeric(unlist(x, use.names = FALSE)))
            return(data.frame(id = as.character(ids), created = cr, stringsAsFactors = FALSE))
        }
        rows <- lapply(x, function(e) {
            if (is.list(e)) {
                id <- tryCatch(e$id, error = function(...) NULL)
                cr <- suppressWarnings(tryCatch(as.numeric(e$created), error = function(...) NA_real_))
                if (!is.null(id)) {
                    return(data.frame(id = as.character(id), created = cr, stringsAsFactors = FALSE))
                }
            }
            NULL
        })
        rows <- Filter(Negate(is.null), rows)
        if (length(rows)) {
            return(do.call(rbind, rows))
        }
    }
    if (is.character(x)) {
        n <- length(x)
        return(data.frame(
            id = if (n) x else character(0),
            created = rep(NA_real_, n),
            stringsAsFactors = FALSE
        ))
    }
    data.frame(id = character(0), created = numeric(0), stringsAsFactors = FALSE)
}

#' list_models() helper
#' @keywords internal
#' @param base_url_normalized Logical; set TRUE when base_url is already normalized
.row_df <- function(provider, base_url, models_df, availability, source, timestamp, status = NA_character_, base_url_normalized = FALSE) {
    base_url <- if (base_url_normalized) base_url else .api_root(base_url)
    models_df <- .as_models_df(models_df)
    timestamp <- if (length(timestamp)) timestamp else NA_real_  # coalesce: legacy cache entries may omit timestamp

    if (nrow(models_df) == 0) {
        df <- data.frame(
            provider = character(),
            base_url = character(),
            model_id = character(),
            created = numeric(),
            availability = character(),
            cached_at = as.POSIXct(numeric(), origin = "1970-01-01", tz = "UTC"),
            source = character(),
            status = character(),
            stringsAsFactors = FALSE
        )
        attr(df, "diagnostic") <- list(provider = provider, base_url = base_url, status = status)
        return(df)
    }

    df <- data.frame(
        provider = provider,
        base_url = base_url,
        model_id = models_df$id,
        created = models_df$created,
        availability = availability,
        cached_at = as.POSIXct(timestamp, origin = "1970-01-01", tz = "UTC"),
        source = source,
        status = status,
        stringsAsFactors = FALSE
    )
    attr(df, "diagnostic") <- list(provider = provider, base_url = base_url, status = status)
    df
}
