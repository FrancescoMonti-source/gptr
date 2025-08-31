#' list_models() helper
#' @keywords internal
#' @keywords internal
.as_models_df <- function(x) {
  # Idempotent: if already good, return it
  if (is.data.frame(x) && all(c("id","created") %in% names(x))) {
    out <- x[, c("id","created"), drop = FALSE]
    out$created <- suppressWarnings(as.numeric(out$created))
    return(out)
  }

  # From character vector of IDs
    if (is.character(x)) {
        n <- length(x)
        return(data.frame(
            id      = unname(x),
            created = if (n) rep(NA_real_, n) else numeric(0),
            stringsAsFactors = FALSE
        ))
    }

  # From list of records (possibly nested)
  if (is.list(x) && !is.data.frame(x)) {
    # common shapes: list(ids=...), list(data=list(list(id=..., created=...)))
    # flatten once; tolerate unknown fields
    flat <- tryCatch(
      { do.call(rbind, lapply(x, function(e) as.data.frame(e, stringsAsFactors = FALSE))) },
      error = function(...) NULL
    )

    if (is.null(flat)) {
      # last resort: pull any $id and $created we can see
      ids <- unlist(lapply(x, `[[`, "id"), use.names = FALSE)
      crs <- suppressWarnings(as.numeric(unlist(lapply(x, `[[`, "created"), use.names = FALSE)))
      return(data.frame(id = ids %||% character(0),
                        created = crs %||% numeric(0),
                        stringsAsFactors = FALSE))
    } else {
      if (!"id" %in% names(flat)) flat$id <- NA_character_
      if (!"created" %in% names(flat)) flat$created <- NA_real_
      flat$created <- suppressWarnings(as.numeric(flat$created))
      return(flat[, c("id","created"), drop = FALSE])
    }
  }

  # Already a data.frame but missing cols
  if (is.data.frame(x)) {
    if (!"id" %in% names(x)) x$id <- NA_character_
    if (!"created" %in% names(x)) x$created <- NA_real_
    x$created <- suppressWarnings(as.numeric(x$created))
    return(x[, c("id","created"), drop = FALSE])
  }

  # Fallback: empty
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
