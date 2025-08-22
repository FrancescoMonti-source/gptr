#' @keywords internal, likely obsolete
.get_model_ids <- function(base_url, force = FALSE) {
    root <- sub("/chat/completions$", "", base_url)
    if (!force && identical(.gptr_state$models_base, root) && !is.null(.gptr_state$models_cache)) {
        return(.gptr_state$models_cache)
    }
    resp <- httr2::request(paste0(root, "/models")) |>
        httr2::req_timeout(3) |>
        httr2::req_perform() |>
        httr2::resp_body_json()
    ids <- vapply(resp$data, function(x) x$id, character(1))
    .gptr_state$models_cache <- ids
    .gptr_state$models_base  <- root
    ids
}
