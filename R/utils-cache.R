#' Convert objects to a models data frame
#'
#' Normalizes various representations of model metadata into a data frame
#' with `id` and `created` columns.
#'
#' @param x Object to convert. Can be `NULL`, a data frame, character vector,
#'   named vector, or list describing model IDs and creation times.
#'
#' @return A data frame with columns `id` and `created`.
#' @keywords internal
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

#' Create a row for the models cache
#'
#' Builds a data frame representing model information returned by a provider.
#'
#' @param provider Character identifier of the model provider.
#' @param base_url Base URL of the provider's API.
#' @param models_df Data frame of models to expand into rows.
#' @param availability Character vector indicating model availability.
#' @param src Source of the model information.
#' @param ts Numeric timestamp when the data were cached.
#' @param status Character string describing the request status.
#' @param base_url_normalized Logical; set `TRUE` when `base_url` is already normalized.
#'
#' @return A data frame with one row per model and a diagnostic attribute.
#' @keywords internal
.row_df <- function(provider, base_url, models_df, availability, src, ts, status = NA_character_, base_url_normalized = FALSE) {
    base_url <- if (base_url_normalized) base_url else .api_root(base_url)
    models_df <- .as_models_df(models_df)
    ts <- if (length(ts)) ts else NA_real_  # coalesce: legacy cache entries may omit ts

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
        cached_at = as.POSIXct(ts, origin = "1970-01-01", tz = "UTC"),
        source = src,
        status = status,
        stringsAsFactors = FALSE
    )
    attr(df, "diagnostic") <- list(provider = provider, base_url = base_url, status = status)
    df
}
