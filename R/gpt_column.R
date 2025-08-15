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

gpt_column <- function(data,
                       col,
                       prompt,
                       keys = NULL,
                       auto_correct_keys = TRUE,
                       keep_unexpected_keys = FALSE,
                       na_values = c("NA", "null", "", "[]", "{}", "None"),
                       file_path = NULL,
                       image_path = NULL,
                       temperature = .2,
                       provider = getOption("gptr.provider", NULL),  # <-- RESTORED
                       relaxed = FALSE,
                       verbose = TRUE,
                       show_invalid_rows = FALSE,
                       return_debug = TRUE,
                       parallel = FALSE,
                       coerce_types = TRUE,
                       coerce_when  = NULL,
                       final_types  = c("schema","infer","as_is"),
                       max_tokens = Inf,
                       token_mode = c("words", "chars", "custom"),
                       custom_tokenizer = NULL,
                       ...) {

    require(dplyr); require(purrr); require(tibble); require(jsonlite)
    require(stringr); require(rlang); require(vctrs); require(progressr)
    if (parallel) require(furrr)

    final_types <- match.arg(final_types)
    token_mode  <- match.arg(token_mode)

    col_name <- rlang::as_string(rlang::ensym(col))
    texts    <- data[[col_name]]

    expected_keys <- if (!is.null(keys)) names(keys) else NULL
    key_specs     <- if (!is.null(keys)) purrr::map(keys, parse_key_spec) else NULL

    preprocess_text <- function(x) {
        trim_text(x, max_tokens = max_tokens, token_mode = token_mode, custom_tokenizer = custom_tokenizer)
    }

    # ---- backend: provider-aware, but test-overridable
    gpt_fun <- .gptr_resolve_backend(provider)

    call_model_once <- function(.x) {
        .x_trim <- preprocess_text(.x)
        input_prompt <- if (is.function(prompt)) {
            prompt(.x_trim, keys)
        } else {
            build_prompt(prompt, text = .x_trim, keys = keys)
        }
        gpt_fun(prompt = input_prompt,
                temperature = temperature,
                file_path   = file_path,
                image_path  = image_path,
                ...)
    }

    # --- optional deprecation shim for old arg name
    dots <- list(...)
    if ("max.distance" %in% names(dots)) {
        warning("`max.distance` is deprecated; use `fuzzy_model` + `fuzzy_threshold`. ",
                "Interpreting as fuzzy_model = 'lev_ratio', fuzzy_threshold = max.distance.")
        dots$fuzzy_model     <- dots$fuzzy_model     %||% "lev_ratio"
        dots$fuzzy_threshold <- dots$fuzzy_threshold %||% dots$max.distance
        dots$max.distance <- NULL
    }

    n <- length(texts); start_time <- Sys.time()

    result <- progressr::with_progress({
        p <- progressr::progressor(steps = n)
        gpt_safe <- function(.x, i) {
            res <- call_model_once(.x)
            elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
            avg <- elapsed / i
            remaining <- avg * (n - i)
            p(message = sprintf("ETA: ~%s | Avg/call: %.2fs",
                                format(Sys.time() + remaining, "%H:%M:%S"), avg))
            res
        }

        raw_outputs <- if (parallel) {
            furrr::future_map2_chr(texts, seq_along(texts), gpt_safe, .options = furrr::furrr_options(seed = NULL))
        } else {
            purrr::map2_chr(texts, seq_along(texts), gpt_safe)
        }

        parsed_results <- purrr::imap(raw_outputs, function(out, i) {
            tryCatch({
                rp <- json_fix_parse_validate(
                    out,
                    key_specs    = key_specs,
                    na_values    = na_values,
                    verbose      = verbose,
                    i            = i,
                    coerce_types = coerce_types,
                    coerce_when  = coerce_when
                )

                if (!rp$ok) {
                    if (relaxed && is.null(expected_keys)) return(out)
                    if (!is.null(expected_keys)) return(setNames(rep(NA, length(expected_keys)), expected_keys))
                    return(out)
                }

                x <- rp$value
                if (!is.null(expected_keys)) {
                    x <- do.call(
                        json_keys_align,
                        c(list(x,
                               expected_keys   = expected_keys,
                               auto_correct    = auto_correct_keys,
                               keep_unexpected = keep_unexpected_keys),
                          dots)  # forwards fuzzy_model/fuzzy_threshold
                    )
                }
                x
            }, error = function(e) {
                if (verbose) message("Row ", i, ": post-processing error → ", conditionMessage(e))
                setNames(rep(NA, length(expected_keys %||% 0)), expected_keys)
            })
        })

        # Step 3: bind rows (no lossy coercion here)
        parsed_df <- purrr::map_dfr(parsed_results, row_to_tibble,
                                    expected_keys = expected_keys,
                                    raw_col_name  = ".parsed")

        # Step 4: final column typing
        parsed_df <- finalize_columns(parsed_df,
                                      expected_keys = expected_keys,
                                      key_specs     = key_specs,
                                      mode          = final_types)

        result <- dplyr::bind_cols(data, parsed_df)

        # FINAL invalid rows (all expected cols NA)
        if (!is.null(expected_keys)) {
            present <- intersect(expected_keys, names(result))
            if (length(present)) {
                invalid_rows <- which(rowSums(is.na(result[, present, drop = FALSE])) == length(present))
            } else invalid_rows <- integer(0)
        } else invalid_rows <- integer(0)

        if (return_debug) {
            result <- tibble::add_column(result, .raw_output = raw_outputs, .after = col_name)
            result$.invalid_rows <- as.integer(seq_len(nrow(result)) %in% invalid_rows)
        }
        attr(result, "invalid_rows") <- invalid_rows
        result
    })

    return(result)
}

