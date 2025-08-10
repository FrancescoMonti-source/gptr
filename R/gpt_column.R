gpt_column
function(data,
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
         ...) {
    # ---- pkgs ------------------------------------------------------------
    require(dplyr); require(purrr); require(tibble);  require(jsonlite)
    require(stringr); require(rlang);  require(vctrs); require(progressr)
    if (parallel) require(furrr)

    # ---------------------------------------------------------------------

    col_name <- rlang::as_string(rlang::ensym(col))
    texts    <- data[[col_name]]
    expected_keys <- if (!is.null(keys)) names(keys) else NULL
    key_specs <- if (!is.null(keys)) {
        purrr::map(keys, parse_key_spec)
    } else NULL


    call_gpt <- function(.x) {
        input_prompt <- if (is.function(prompt)) {
            prompt(.x, keys)
        } else {
            build_prompt(prompt, text = .x, keys = keys)
        }
        gpt(prompt      = input_prompt,
            temperature = temperature,
            file_path   = file_path,
            image_path  = image_path,
            ...)
    }

    # progress (parallel-safe) --------------------------------------------
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
            res <- call_gpt(.x)
            elapsed  <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
            avg      <- elapsed / i
            remaining<- avg * (n - i)
            p(message = sprintf("ETA: ~%s | Avg/call: %.2fs",
                                format(Sys.time() + remaining, "%H:%M:%S"), avg))
            res
        }
        raw_outputs <- if (parallel) {
            future_map2_chr(texts, seq_along(texts), gpt_safe,
                            .options = furrr_options(seed = NULL))
        } else {
            map2_chr(texts, seq_along(texts), gpt_safe)
        }
    })

    # -----------------------  parsing  ------------------------------------
    parsed_results <- purrr::imap(raw_outputs, function(out, i) {
        tryCatch({
            if (any(trimws(tolower(out)) %in% trimws(tolower(na_values))))
                return(rep(NA, length(expected_keys %||% 0)))

            rep_pair <- tidy_json(out, na_values, aggressive = FALSE)
            cleaned  <- rep_pair$txt
            if (verbose && length(rep_pair$log))
                message(glue::glue("Row {i}: Repaired JSON -> {paste(rep_pair$log, collapse = ', ')}"))

            parsed <- tryCatch(jsonlite::fromJSON(cleaned, simplifyVector = TRUE),
                               error = function(e) NULL)

            if (!is.list(parsed) || is.null(names(parsed)) || any(names(parsed) == "")) {
                if (!relaxed && !is.null(expected_keys)) {
                    if (verbose) message(glue::glue("Row {i}: Output not valid JSON: {out}"))
                    return(setNames(rep(NA, length(expected_keys)), expected_keys))
                } else return(out)
            }

            # field-level NA handling + type/allowed-set validation
            parsed <- purrr::imap(parsed, function(value, key) {
                if (is_na_like(value, na_values)) return(NA)

                if (!is.null(key_specs) && key %in% names(key_specs)) {
                    spec <- key_specs[[key]]

                    # 1) type-first coercion (if a type was declared)
                    if (!is.null(spec$type)) {
                        value <- coerce_type(value, spec$type)
                    }

                    # 2) allowed-set validation (if a set was declared)
                    if (!is.null(spec$allowed)) {
                        # keep as scalar; if model returned vector/array, take first
                        if (length(value) > 1L) value <- value[[1L]]
                        if (!in_allowed(value, spec$allowed)) {
                            if (verbose) message(glue::glue(
                                "Value '{normalize_token(value)}' not in allowed set for key '{key}'. Setting NA."))
                            return(NA)
                        }
                    }
                    return(value)
                }

                # key not in schema
                value
            })


            # unexpected keys
            if (!is.null(expected_keys)) {
                unexpected <- setdiff(names(parsed), expected_keys)
                if (length(unexpected) > 0 && verbose)
                    message(glue::glue("Row {i} - Unexpected key(s): ",
                                       paste(unexpected, collapse = ", ")))
            }

            # auto-correct near-miss keys
            if (auto_correct_keys && !is.null(expected_keys) &&
                !setequal(names(parsed), expected_keys)) {
                corrected <- match_arg_tol(names(parsed), expected_keys,
                                           several.ok = TRUE, max.distance = max.distance)
                names(parsed)[!is.na(corrected)] <- corrected[!is.na(corrected)]
                if (!keep_unexpected_keys)
                    parsed <- parsed[expected_keys[expected_keys %in% names(parsed)]]
            }

            if (relaxed && is.null(expected_keys)) return(parsed)

            out_vec <- setNames(rep(NA, length(expected_keys)), expected_keys)
            for (k in expected_keys) if (k %in% names(parsed)) out_vec[k] <- parsed[[k]]
            out_vec

        }, error = function(e) {
            if (verbose) message("Row ", i, ": post-processing error \u2192 ",
                                 conditionMessage(e))
            setNames(rep(NA, length(expected_keys %||% 0)), expected_keys)
        })
    })

    # bind results
    parsed_df <- map_dfr(parsed_results, function(x) {
        if (is.null(names(x)) || any(names(x) == ""))
            x <- setNames(rep(NA, length(expected_keys)), expected_keys)
        tibble::as_tibble_row(x)
    })
    result <- bind_cols(data, parsed_df)

    invalid_rows <- which(map_lgl(parsed_results, ~ all(is.na(.x))))
    if (return_debug) {
        result <- tibble::add_column(result, .raw_output = raw_outputs,
                                     .after = col_name)
        result$.invalid_rows <- as.integer(seq_len(nrow(result)) %in% invalid_rows)
    }
    if (show_invalid_rows && length(invalid_rows) > 0) {
        cat(glue::glue("{length(invalid_rows)} invalid row(s) detected:\n"))
        print(data[invalid_rows, , drop = FALSE])
    }

    attr(result, "invalid_rows") <- invalid_rows
    result
}
