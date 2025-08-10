#' Extract Structured Data from Free Text with an LLM (column-wise)
#'
#' `gpt_column()` sends each row of a text column to a language model (local or OpenAI),
#' using a prompt template or a prompt function, and parses the model's output into
#' typed columns. It supports schema-based validation (`keys`), fuzzy key correction,
#' NA normalization, progress/ETA, optional parallelization, and debug capture of raw outputs.
#'
#' @section Schema (`keys`):
#' Each element of `keys` can be either:
#' - a **type string**: `"integer"`, `"numeric"`, `"character"`, or `"logical"`; the value is coerced.
#' - a **vector of allowed values** (e.g., `c("oui","non","NA")`, `c(0,1)`, `c(TRUE,FALSE)`).
#'   If present, outputs not in the allowed set are set to `NA`.
#'
#' The same `keys` are typically passed to your prompt builder (e.g., via `build_prompt()`)
#' so the JSON skeleton shown to the model matches what the parser expects.
#'
#' @section Prompting:
#' - `prompt` may be a **character template** (with `{text}` and optionally `{json_format}` placeholders,
#'   typically filled by `build_prompt()`), **or** a **function** with signature `function(text, keys)`.
#' - If you pass a function, it will be called for each row to build the final prompt string.
#'
#' @param data A data frame or tibble containing the text column to process.
#' @param col Unquoted column name containing the input text for the LLM.
#' @param prompt Either a character template (used by `build_prompt()`), or a function `function(text, keys)` that returns a prompt string per row.
#' @param keys `NULL` (relaxed mode) or a **named list** defining the schema.
#'   Each name is an expected output key. Each value is either a type string
#'   (`"integer"`, `"numeric"`, `"character"`, `"logical"`) or a vector of allowed values.
#' @param auto_correct_keys Logical. If `TRUE`, attempt fuzzy correction of near-miss key names to the expected names.
#' @param max.distance Numeric in \[0,1]. Maximum distance for fuzzy key matching (passed to `match_arg_tol()`).
#' @param keep_unexpected_keys Logical. If `FALSE` (default), unexpected keys are dropped after autocorrection; if `TRUE`, they are kept.
#' @param na_values Character vector of NA-like tokens to normalize (case-insensitive), e.g. `"NA"`, `"null"`, `""`, `"[]"`, `"{}"`, `"None"`.
#' @param file_path Optional path to a file whose textual contents should be appended to the prompt (handled inside `gpt()`).
#' @param image_path Optional path to an image to include in the prompt (handled inside `gpt()`).
#' @param temperature Sampling temperature passed to `gpt()`.
#' @param relaxed Logical. If `TRUE` **and** `keys = NULL`, returns the parsed list/object as is (no schema mapping).
#'   If `FALSE` and `keys` are provided, rows with invalid/unsuitable JSON yield `NA` for all expected keys.
#' @param verbose Logical. If `TRUE`, prints repair logs (from `tidy_json()`), unexpected keys, coercion notes, and ETA updates.
#' @param show_invalid_rows Logical. If `TRUE`, prints the subset of `data` rows that failed completely after parsing.
#' @param return_debug Logical. If `TRUE`, appends two debug columns: `.raw_output` (model output) and `.invalid_rows` (0/1 flag).
#' @param parallel Logical. If `TRUE`, uses `furrr::future_map2_chr()`; configure workers via `future::plan()`.
#' @param ... Additional arguments forwarded to `gpt()` (e.g., `provider`, `model`, `base_url`, `system`, `seed`,
#'   `response_format`, timeouts, etc.).
#'
#' @return
#' A tibble: the original `data` **plus** one column per expected key (in the order of `names(keys)`).
#' If `return_debug = TRUE`, also includes `.raw_output` and `.invalid_rows`.
#' An integer vector of invalid row indices is attached as an attribute:
#' `attr(result, "invalid_rows")`.
#'
#' @details
#' **Per-row flow**
#' 1. Build prompt (template or function).
#' 2. Call `gpt()` (local or OpenAI backend).
#' 3. Clean/repair JSON via `tidy_json()` (strips fences, extracts the JSON blob, fixes common issues conservatively).
#' 4. Parse with `jsonlite::fromJSON()`.
#' 5. Normalize NA-likes, coerce to declared types, and validate against allowed sets.
#' 6. Autocorrect key names (if enabled) and map to the schema order; drop extras unless `keep_unexpected_keys = TRUE`.
#'
#' **Progress & parallel**
#' - Progress/ETA shown via `progressr`. When `parallel = TRUE`, work is split via `furrr` (respect your `future::plan()`).
#'
#' **Error handling**
#' - Rows that cannot be parsed into valid JSON (and `relaxed = FALSE` with a schema) return all-`NA` for expected keys.
#' - Repair actions taken by `tidy_json()` are logged row-by-row when `verbose = TRUE`.
#'
#' **Interoperability**
#' - Works with local OpenAI-compatible servers (e.g., LM Studio) or hosted providers (set `provider`, `base_url`, `model`, `api_key` inside `gpt()`).
#'
#' @section Troubleshooting:
#' - **All `NA` columns**: the model likely returned non-JSON or keys didn’t match. Inspect `.raw_output` (set `return_debug = TRUE`)
#'   and consider `auto_correct_keys = TRUE`, adjusting `max.distance`, or loosening `na_values`.
#' - **Dropped rows warning**: your prompt may return arrays or multiple objects; ensure the model returns **exactly one** JSON object per row.
#' - **Unexpected keys**: set `keep_unexpected_keys = TRUE` to keep them for inspection, or expand your `keys` schema.
#' - **Timeouts/Rate limits**: pass timeouts or small delays via `...` to `gpt()`; for rate-limited providers, consider batching or `parallel = FALSE`.
#'
#' @examples
#' \dontrun{
#' # 1) Using a template and build_prompt()
#' template <- paste0(
#'   "Tu es un assistant spécialisé.\n\n",
#'   "Texte :\n\"{text}\"\n\n",
#'   "Retourne un JSON sur une seule ligne, format exact :\n",
#'   "{json_format}\n",
#'   "- Clés entre guillemets; valeurs autorisées uniquement.\n",
#'   "- Si absent: \"NA\". Aucune autre sortie."
#' )
#'
#' prompt_fun <- function(text, keys) build_prompt(template, text, keys)
#'
#' res <- gpt_column(
#'   data   = df,
#'   col    = note_medicale,
#'   prompt = prompt_fun,
#'   keys   = list(
#'     isolement_bin      = "integer",
#'     tendance_isolement = c("oui","non","NA")
#'   ),
#'   provider = "openai",
#'   model    = "gpt-4o-mini",
#'   temperature = 0.2,
#'   verbose  = TRUE,
#'   return_debug = TRUE
#' )
#'
#' # Rows that failed:
#' attr(res, "invalid_rows")
#'
#' # 2) Retry failed rows (with audit trail)
#' res2 <- patch_failed_rows(
#'   data   = res,
#'   prompt = prompt_fun,
#'   col    = note_medicale,
#'   id_col = patient_id,
#'   keys   = list(isolement_bin = "integer", tendance_isolement = c("oui","non","NA")),
#'   max_attempts = 2
#' )
#' attr(res2, "invalid_rows")
#' }
#'
#' @seealso
#' [build_prompt()] for creating `{json_format}` blocks from `keys`,
#' [tidy_json()] for robust JSON cleanup,
#' [gpt()] for low-level model calls,
#' and [patch_failed_rows()] to automatically repair failed rows.
#' @export



gpt_column = function(data,
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
