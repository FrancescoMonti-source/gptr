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
                       max.distance = .2,
                       keep_unexpected_keys = FALSE,
                       na_values = c("NA", "null", "", "[]", "{}", "None"),
                       file_path = NULL,
                       image_path = NULL,
                       temperature = .2,
                       relaxed = FALSE,
                       verbose = TRUE,
                       show_invalid_rows = FALSE,
                       return_debug = TRUE,
                       parallel = FALSE,
                       # NEW knobs:
                       coerce_types = TRUE,
                       coerce_when  = NULL,
                       final_types  = c("schema","infer","as_is"),
                       max_tokens = Inf,
                       token_mode = c("words", "chars", "custom"),
                       custom_tokenizer = NULL,
                       ...) {

    require(dplyr); require(purrr); require(tibble);  require(jsonlite)
    require(stringr); require(rlang);  require(vctrs); require(progressr)
    if (parallel) require(furrr)

    final_types <- match.arg(final_types)
    token_mode  <- match.arg(token_mode)

    col_name <- rlang::as_string(rlang::ensym(col))
    texts    <- data[[col_name]]

    expected_keys <- if (!is.null(keys)) names(keys) else NULL
    key_specs <- if (!is.null(keys)) purrr::map(keys, parse_key_spec) else NULL

    preprocess_text <- function(x) {
        trim_text(
            x,
            max_tokens = max_tokens,
            token_mode = token_mode,
            custom_tokenizer = custom_tokenizer
        )
    }

    call_model_once <- function(.x) {
        .x_trim <- preprocess_text(.x)
        input_prompt <- if (is.function(prompt)) {
            prompt(.x_trim, keys)
        } else {
            build_prompt(prompt, text = .x_trim, keys = keys)
        }
        gpt(prompt      = input_prompt,
            temperature = temperature,
            file_path   = file_path,
            image_path  = image_path,
            ...)
    }

    if (parallel) {
        n_workers <- future::nbrOfWorkers()
        message(glue::glue(
            "Running in parallel mode using {n_workers} worker(s).\n",
            "To change this, call plan(multisession, workers = N)."))
    }

    n <- length(texts);  start_time <- Sys.time()

    progressr::with_progress({
        p <- progressr::progressor(steps = n)

        gpt_safe <- function(.x, i) {
            res <- call_model_once(.x)
            # progress/ETA
            elapsed  <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
            avg      <- elapsed / i
            remaining<- avg * (n - i)
            p(message = sprintf("ETA: ~%s | Avg/call: %.2fs",
                                format(Sys.time() + remaining, "%H:%M:%S"), avg))
            res
        }

        raw_outputs <- if (parallel) {
            furrr::future_map2_chr(texts, seq_along(texts), gpt_safe,
                                   .options = furrr::furrr_options(seed = NULL))
        } else {
            purrr::map2_chr(texts, seq_along(texts), gpt_safe)
        }

        parsed_results <- purrr::imap(raw_outputs, function(out, i) {
            tryCatch({
                # Step 1: fix/parse/validate JSON (row-level coercion optional)
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
                    # Not JSON:
                    if (relaxed && is.null(expected_keys)) return(out)   # passthrough in relaxed mode without schema
                    if (!is.null(expected_keys)) {
                        return(setNames(rep(NA, length(expected_keys)), expected_keys))
                    }
                    return(out)
                }

                # Step 2: align keys to schema (names + fill)
                x <- rp$value
                if (!is.null(expected_keys)) {
                    x <- json_keys_align(
                        x,
                        expected_keys   = expected_keys,
                        auto_correct    = auto_correct_keys,
                        max_distance    = max.distance,
                        keep_unexpected = keep_unexpected_keys
                    )
                }

                x
            }, error = function(e) {
                if (verbose) message("Row ", i, ": post-processing error \u2192 ",
                                     conditionMessage(e))
                setNames(rep(NA, length(expected_keys %||% 0)), expected_keys)
            })
        })

        # Step 3: bind rows without forcing final types
        parsed_df <- purrr::map_dfr(parsed_results, row_to_tibble,
                                    expected_keys = expected_keys)

        # Step 4: final column typing (schema / infer / as_is)
        parsed_df <- finalize_columns(parsed_df,
                                      expected_keys = expected_keys,
                                      key_specs     = key_specs,
                                      mode          = final_types)

        result <- dplyr::bind_cols(data, parsed_df)

        # invalid rows (strict mode): where all expected keys are NA
        invalid_rows <- if (!is.null(expected_keys)) {
            which(purrr::map_lgl(parsed_results, function(x) {
                is.list(x) && all(names(x) %in% expected_keys) &&
                    all(vapply(expected_keys, function(k) isTRUE(is.na(x[[k]])), logical(1)))
            }))
        } else integer(0)

        if (return_debug) {
            result <- tibble::add_column(result, .raw_output = raw_outputs, .after = col_name)
            result$.invalid_rows <- as.integer(seq_len(nrow(result)) %in% invalid_rows)
        }
        if (show_invalid_rows && length(invalid_rows) > 0) {
            cat(glue::glue("{length(invalid_rows)} invalid row(s) detected:\n"))
            print(data[invalid_rows, , drop = FALSE])
        }

        attr(result, "invalid_rows") <- invalid_rows
        result
    })
}
