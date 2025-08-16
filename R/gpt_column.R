#' Extract Structured Data from Free Text via LLM Completion (Orchestrator)
#'
#' Sends each row of a text column to an LLM with a templated prompt, repairs/parses JSON,
#' validates and aligns it to a schema, then returns the structured columns bound to the input.
#'
#' @param data A data frame or tibble containing the text column.
#' @param col Unquoted name of the text column to send to the LLM.
#' @param prompt Character template with {text}/{json_format} or function(text, keys) -> string.
#' @param keys Optional named list defining expected JSON keys and their type or allowed set.
#' @param auto_correct_keys Logical; fuzzy-correct unexpected key names (unique match only).
#'   Fuzzy key correction is handled by json_keys_align(); control with `fuzzy_model`
#'   ("lev_ratio" or "lev") and `fuzzy_threshold`.
#' @param keep_unexpected_keys Keep keys not listed in `keys`.
#' @param na_values Values treated as NA at multiple stages.
#' @param file_path,image_path Optional file paths passed to the model call.
#' @param temperature Sampling temperature for the model.
#' @param relaxed If TRUE and `keys` is NULL, allow non-JSON / raw outputs.
#' @param verbose Print repair/validation messages.
#' @param return_debug If TRUE, add `.raw_output`, `.invalid_rows`, and `.invalid_detail`. Default TRUE.
#' @param coerce_types Row-level coercion toggle (default TRUE).
#' @param coerce_when Optional named list of per-key target types used for row-level coercion.
#' @param infer_types Logical; when no schema is provided, infer column types (default FALSE).
#'   If a schema (`keys`) is provided, it is always used for final typing.
#' @param ... Extra args passed to `gpt()` (e.g., `model`, `response_format`).
#' @export
gpt_column <- function(
        data,
        col,
        prompt,
        keys                 = NULL,
        provider             = c("local", "openai"),
        temperature          = 0,
        file_path            = NULL,
        image_path           = NULL,
        coerce_types         = TRUE,
        coerce_when          = NULL,
        infer_types          = FALSE,   # <— replaces final_types
        na_values            = c("NA", "N/A", "null", "None", ""),
        auto_correct_keys    = getOption("gptr.auto_correct_keys", TRUE),
        keep_unexpected_keys = getOption("gptr.keep_unexpected_keys", FALSE),
        fuzzy_model          = getOption("gptr.fuzzy_model", "lev_ratio"),
        fuzzy_threshold      = getOption("gptr.fuzzy_threshold", 0.3),
        relaxed              = FALSE,
        return_debug         = TRUE,    # keep debug cols by default per your preference
        verbose              = FALSE,
        ...
) {
    provider <- match.arg(provider)

    # Resolve backend (respects options(gptr.gpt_fun))
    gpt_fun <- .gptr_resolve_backend(provider)

    # Safety: ungroup & validate column exists
    if (dplyr::is_grouped_df(data)) data <- dplyr::ungroup(data)
    col_quo  <- rlang::enquo(col)
    col_name <- rlang::as_name(col_quo)
    if (!col_name %in% names(data)) {
        stop("Column '", col_name, "' not found in `data`.", call. = FALSE)
    }

    # Capture texts
    texts <- dplyr::pull(data, !!col_quo)
    if (!is.character(texts)) texts <- as.character(texts)
    n <- length(texts)

    # Parse key spec
    key_specs <- NULL
    expected_keys <- NULL
    if (!is.null(keys)) {
        key_specs <- parse_key_spec(keys)
        expected_keys <- names(key_specs)
    }

    # Prompt builder
    make_prompt_for <- function(.x_text) {
        .x_trim <- trimws(.x_text)
        if (is.function(prompt)) prompt(.x_trim, keys) else build_prompt(prompt, text = .x_trim, keys = keys)
    }

    # Call model row-wise (skip NA/empty)
    raw_outputs <- vapply(seq_len(n), function(i) {
        txt <- texts[[i]]
        if (is.na(txt) || !nzchar(trimws(txt))) return(NA_character_)
        input_prompt <- make_prompt_for(txt)
        gpt_fun(
            prompt      = input_prompt,
            temperature = temperature,
            file_path   = file_path,
            image_path  = image_path,
            ...
        )
    }, FUN.VALUE = character(1))

    # Simple scalar caster (row-level)
    norm_type <- function(t) {
        t <- tolower(trimws(as.character(t)))
        if (t %in% c("integer","int","long","whole")) return("integer")
        if (t %in% c("numeric","double","float","number","real")) return("numeric")
        if (t %in% c("logical","bool","boolean")) return("logical")
        if (t %in% c("character","string","text")) return("character")
        t
    }
    cast_one <- function(val, type) {
        type <- norm_type(type)
        if (type == "integer")  return(suppressWarnings(as.integer(val)))
        if (type == "numeric")  return(suppressWarnings(as.numeric(val)))
        if (type == "logical") {
            if (is.logical(val)) return(val)
            if (is.numeric(val)) return(val != 0)
            if (is.character(val)) {
                v <- tolower(trimws(val))
                return(ifelse(v %in% c("true","t","yes","y","1"), TRUE,
                              ifelse(v %in% c("false","f","no","n","0"), FALSE, NA)))
            }
            return(suppressWarnings(as.logical(val)))
        }
        as.character(val)
    }

    # Parse + align + row coercion
    parsed_results <- vector("list", n)
    invalid_flags  <- logical(n)
    invalid_detail <- vector("list", n)

    for (i in seq_len(n)) {
        out <- raw_outputs[[i]]

        # Missing input or missing output → invalid
        if (is.na(out) || !nzchar(out)) {
            invalid_flags[i]   <- TRUE
            invalid_detail[[i]] <- NULL
            if (!is.null(expected_keys)) {
                parsed_results[[i]] <- setNames(rep(NA, length(expected_keys)), expected_keys)
            } else {
                parsed_results[[i]] <- NA_character_
            }
            next
        }

        rp <- tryCatch(
            json_fix_parse_validate(
                out,
                key_specs    = key_specs,
                na_values    = na_values,
                verbose      = verbose,
                i            = i,
                coerce_types = FALSE,
                coerce_when  = NULL
            ),
            error = function(e) {
                if (verbose) message("Row ", i, ": parse error --> ", conditionMessage(e))
                list(ok = FALSE, value = NULL, meta = NULL)
            }
        )

        # invalid if parse failed OR meta reports any failure
        invalid <- !isTRUE(rp$ok)
        meta_df <- if (is.data.frame(rp$meta)) rp$meta else NULL
        if (is.data.frame(meta_df)) {
            bad <- meta_df[(meta_df$type_ok %in% FALSE) | (meta_df$allowed %in% FALSE), , drop = FALSE]
            if (nrow(bad)) invalid <- TRUE
        }

        if (!isTRUE(rp$ok)) {
            if (relaxed && is.null(expected_keys)) {
                parsed_results[[i]] <- out
            } else if (!is.null(expected_keys)) {
                parsed_results[[i]] <- setNames(rep(NA, length(expected_keys)), expected_keys)
            } else {
                parsed_results[[i]] <- out
            }
            invalid_flags[i]   <- TRUE
            invalid_detail[[i]] <- meta_df
            next
        }

        x <- rp$value

        # Align keys if needed
        if (!is.null(expected_keys) && !setequal(names(x), expected_keys)) {
            x <- json_keys_align(
                x,
                expected_keys   = expected_keys,
                auto_correct    = auto_correct_keys,
                keep_unexpected = keep_unexpected_keys,
                fuzzy_model     = fuzzy_model,
                fuzzy_threshold = fuzzy_threshold
            )
        }

        # Row-level coercion using schema types or coerce_when
        if (isTRUE(coerce_types) && !is.null(key_specs)) {
            for (k in intersect(names(x), names(key_specs))) {
                tt <- key_specs[[k]]$type
                if (!is.null(tt)) x[[k]] <- cast_one(x[[k]], tt)
            }
        } else if (!is.null(coerce_when) && isTRUE(coerce_types)) {
            for (k in intersect(names(x), names(coerce_when))) {
                tt <- coerce_when[[k]]
                if (!is.null(tt)) x[[k]] <- cast_one(x[[k]], tt)
            }
        }

        parsed_results[[i]] <- x
        invalid_flags[i]    <- invalid
        invalid_detail[[i]] <- meta_df
    }

    # Bind rows
    parsed_df <- purrr::map_dfr(
        parsed_results,
        row_to_tibble,
        expected_keys = expected_keys,
        raw_col_name  = ".parsed"
    )

    # Decide finalization mode automatically:
    # - schema present  -> "schema"
    # - no schema:
    #     - infer_types TRUE  -> "infer"
    #     - else              -> "as_is"
    mode <- if (!is.null(key_specs)) "schema" else if (isTRUE(infer_types)) "infer" else "as_is"

    parsed_df <- finalize_columns(
        parsed_df,
        expected_keys = expected_keys,
        key_specs     = key_specs,
        mode          = mode
    )

    # Drop helper col if still present
    if (".parsed" %in% names(parsed_df)) parsed_df$.parsed <- NULL

    # Bind back to original data
    result <- dplyr::bind_cols(data, parsed_df)

    # Keep debug cols
    if (isTRUE(return_debug)) {
        result <- tibble::add_column(result, .raw_output = raw_outputs, .after = col_name)
        result$.invalid_rows   <- as.logical(invalid_flags)
        result$.invalid_detail <- invalid_detail
    }

    # Drop noisy internal meta if any slipped through, while preserving debug cols
    keep_debug <- c(".raw_output", ".invalid_rows", ".invalid_detail")
    junk <- c("type","allowed","valid",".valid",".raw",".error","..raw_json","..parse_error",".path")
    to_drop <- setdiff(junk, keep_debug)
    result <- result[, setdiff(names(result), to_drop), drop = FALSE]

    # Also expose failing row indices as an attr (back-compat)
    attr(result, "invalid_rows") <- which(invalid_flags)
    result
}
