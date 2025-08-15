#' Extract Structured Data from Free Text via LLM Completion (Orchestrator)
#'
#' Sends each row of a text column to an LLM with a templated prompt, repairs/parses JSON,
#' validates and aligns it to a schema, then returns the structured columns bound to the input.
#'
#' @param data A data frame or tibble containing the text column.
#' @param col Unquoted name of the text column to send to the LLM.
#' @param prompt Character template with {text}/{json_format} or function(text, keys) -> string.
#' @param keys Optional named list defining expected JSON keys and their type or allowed set.
#' @param auto_correct_keys Logical; fuzzy-correct unexpected key names (unique match only). Fuzzy key correction is handled by json_keys_align(). You can control it by passing fuzzy_model ("lev_ratio" or "lev") and fuzzy_threshold via .... Defaults: fuzzy_model = "lev_ratio", fuzzy_threshold = 0.3.
#' @param max.distance Max distance for fuzzy key matching (agrep).
#' @param keep_unexpected_keys Keep keys not listed in `keys`.
#' @param na_values Values treated as NA at multiple stages.
#' @param file_path,image_path Optional file paths passed to the model call.
#' @param temperature Sampling temperature for the model.
#' @param relaxed If TRUE and `keys` is NULL, allow non-JSON / raw outputs.
#' @param verbose Print repair/validation messages.
#' @param show_invalid_rows Print offending inputs (by index) if validation fails.
#' @param return_debug If TRUE, add `.raw_output` and `.invalid_rows`.
#' @param parallel If TRUE, use `furrr` to parallelize.
#' @param coerce_types Row-level coercion toggle (default TRUE).
#' @param coerce_when Optional predicate function(key, value, spec, row_index) -> TRUE/FALSE for per-value control.
#' @param final_types Final column typing: "schema" (default), "infer", or "as_is".
#' @param max_tokens Token limit for trim_text().
#' @param token_mode "words", "chars", or "custom" for trim_text().
#' @param custom_tokenizer Optional custom tokenizer for trim_text().
#' @param ... Extra args passed to `gpt()`.
#' @export

gpt_column <- function(
        data,
        col,
        prompt,
        keys                = NULL,
        provider            = c("local", "openai"),
        temperature         = 0,
        file_path           = NULL,
        image_path          = NULL,
        coerce_types        = TRUE,
        coerce_when         = NULL,
        final_types         = c("schema", "infer", "as_is"),
        na_values           = c("NA", "N/A", "null", "None", ""),
        auto_correct_keys   = getOption("gptr.auto_correct_keys", TRUE),
        keep_unexpected_keys= getOption("gptr.keep_unexpected_keys", FALSE),
        fuzzy_model         = getOption("gptr.fuzzy_model", "lev_ratio"),
        fuzzy_threshold     = getOption("gptr.fuzzy_threshold", 0.3),
        relaxed             = FALSE,
        return_debug        = FALSE,
        verbose             = FALSE,
        ...
) {
    provider    <- match.arg(provider)
    final_types <- match.arg(final_types)

    # ---- resolve backend; allows swap-in via options(gptr.gpt_fun = ...)
    gpt_fun <- .gptr_resolve_backend(provider)

    # ---- capture column and text values
    col_quo <- rlang::enquo(col)
    stopifnot(rlang::quo_is_symbolic(col_quo) || rlang::quo_is_symbol(col_quo))
    texts <- dplyr::pull(data, !!col_quo)
    if (!is.character(texts)) texts <- as.character(texts)
    n <- length(texts)

    # ---- key spec parsing
    key_specs <- NULL
    expected_keys <- NULL
    if (!is.null(keys)) {
        # parse_key_spec() in your package turns list/char specs into a named list with $type, etc.
        key_specs <- parse_key_spec(keys)
        expected_keys <- names(key_specs)
    }

    # ---- helper: prompt per row
    make_prompt_for <- function(.x_text) {
        .x_trim <- preprocess_text(.x_text)
        if (is.function(prompt)) {
            prompt(.x_trim, keys)
        } else {
            build_prompt(prompt, text = .x_trim, keys = keys)
        }
    }

    # ---- Step 1: call model once per row
    raw_outputs <- purrr::map_chr(texts, function(.x) {
        input_prompt <- make_prompt_for(.x)
        gpt_fun(
            prompt      = input_prompt,
            temperature = temperature,
            file_path   = file_path,
            image_path  = image_path,
            ...
        )
    })

    # ---- Step 2: parse + per-row coercion (json_fix_parse_validate now does row-wise casting)
    ok_flags <- integer(n)
    parsed_results <- vector("list", n)

    for (i in seq_len(n)) {
        out <- raw_outputs[[i]]
        rp <- tryCatch(
            json_fix_parse_validate(
                out,
                key_specs    = key_specs,
                na_values    = na_values,
                verbose      = verbose,
                i            = i,
                coerce_types = coerce_types,
                coerce_when  = coerce_when
            ),
            error = function(e) {
                if (verbose) message("Row ", i, ": parse error --> ", conditionMessage(e))
                list(ok = FALSE, value = NULL)
            }
        )

        if (!isTRUE(rp$ok)) {
            ok_flags[i] <- 0L
            if (relaxed && is.null(expected_keys)) {
                parsed_results[[i]] <- out
            } else if (!is.null(expected_keys)) {
                parsed_results[[i]] <- setNames(rep(NA, length(expected_keys)), expected_keys)
            } else {
                parsed_results[[i]] <- out
            }
        } else {
            ok_flags[i] <- 1L
            x <- rp$value

            # Ensure key alignment again if caller turned off key_specs but passed expected_keys
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

            parsed_results[[i]] <- x
        }
    }

    # ---- Step 3: bind rows to tibble without upcasting (row_to_tibble handles a single list row)
    parsed_df <- purrr::map_dfr(
        parsed_results,
        row_to_tibble,
        expected_keys = expected_keys,
        raw_col_name  = ".parsed"
    )

    # ---- Step 4: final column typing (usually a no-op now; keep for safety)
    parsed_df <- finalize_columns(
        parsed_df,
        expected_keys = expected_keys,
        key_specs     = key_specs,
        mode          = final_types
    )

    # ---- Assemble result
    result <- dplyr::bind_cols(data, parsed_df)

    # invalid rows = those whose parse/repair failed (not "all NA")
    invalid_rows <- if (!is.null(expected_keys)) which(ok_flags == 0L) else integer(0)

    if (isTRUE(return_debug)) {
        result <- tibble::add_column(result, .raw_output = raw_outputs, .after = {{col}})
        result$.invalid_rows <- as.integer(seq_len(nrow(result)) %in% invalid_rows)
    }

    attr(result, "invalid_rows") <- invalid_rows
    result
}
