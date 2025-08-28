#' Clear gptr internal caches (backend detection, model list, ping)
#'
#' Useful in tests to ensure a clean state.
#' @export
gpt_clear_caches <- function() {
  .gptr_state$detected_backend <- NULL
  .gptr_state$models_cache <- NULL
  .gptr_state$models_base <- NULL
  .gptr_state$warned_missing <- FALSE
  .gptr_state$ping_cache <- new.env(parent = emptyenv())
  invisible(TRUE)
}


#' @export
gpt_refresh_models <- function(base_url = getOption("gptr.local_base_url", NULL)) {
  if (is.null(base_url) || !nzchar(base_url)) stop("Provide base_url or set options(gptr.local_base_url).", call. = FALSE)
  invisible(.get_model_ids(base_url, force = TRUE))
}


#' list_models() helprer
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

#' list_models() helper
#' @keywords internal
.row_df <- function(provider, base_url, models_df, availability, src, ts, status = NA_character_) {
    base_url <- .api_root(base_url)
    models_df <- .as_models_df(models_df)
    if (nrow(models_df) == 0) {
        return(data.frame(
            provider = provider,
            base_url = base_url,
            model_id = NA_character_,
            created = NA_real_,
            availability = availability,
            cached_at = as.POSIXct(ts, origin = "1970-01-01", tz = "Europe/Paris"),
            source = src,
            status = status,
            stringsAsFactors = FALSE
        ))
    }
    data.frame(
        provider = provider,
        base_url = base_url,
        model_id = models_df$id,
        created = models_df$created,
        availability = availability,
        cached_at = as.POSIXct(ts, origin = "1970-01-01", tz = "Europe/Paris"),
        source = src,
        status = status,
        stringsAsFactors = FALSE
    )
}
