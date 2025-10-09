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
#' @param provider One of "auto", "local", "openai", "lmstudio", "ollama", "localai".
#' @param backend Optional backend to use when `provider = "local"`.
#' @param base_url (now accepted by gpt_column() too): “Optional. Pin a specific local endpoint (…/v1 or …/v1/chat/completions).”
#' @param model Model identifier to request from the provider/backend.
#' @param na_values Values treated as NA at multiple stages.
#' @param file_path,image_path Optional file paths passed to the model call.
#' @param temperature Sampling temperature for the model.
#' @param relaxed If TRUE and `keys` is NULL, allow non-JSON / raw outputs.
#' @param verbose Print repair/validation messages.
#' @param return_debug If TRUE, add `.raw_output`, `.invalid_rows`, and `.invalid_detail`. Default TRUE.
#' @param .coerce_types Row-level coercion toggle (default TRUE).
#' @param coerce_when Optional named list of per-key target types used for row-level coercion.
#' @param infer_types Logical; when no schema is provided, infer column types (default FALSE).
#'   If a schema (`keys`) is provided, it is always used for final typing.
#' @param progress Logical. Progress Bar, defaults to TRUE.
#' @param ... Extra args passed to `gpt()` (e.g., `response_format`).
#' @export

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
#' @param provider One of "auto", "local", "openai", "lmstudio", "ollama", "localai".
#' @param backend Optional backend to use when `provider = "local"`.
#' @param base_url (now accepted by gpt_column() too): “Optional. Pin a specific local endpoint (…/v1 or …/v1/chat/completions).”
#' @param model Model identifier to request from the provider/backend.
#' @param na_values Values treated as NA at multiple stages.
#' @param file_path,image_path Optional file paths passed to the model call.
#' @param temperature Sampling temperature for the model.
#' @param relaxed If TRUE and `keys` is NULL, allow non-JSON / raw outputs.
#' @param verbose Print repair/validation messages.
#' @param return_debug If TRUE, add `.raw_output`, `.invalid_rows`, and `.invalid_detail`. Default TRUE.
#' @param .coerce_types Row-level coercion toggle (default TRUE).
#' @param coerce_when Optional named list of per-key target types used for row-level coercion.
#' @param infer_types Logical; when no schema is provided, infer column types (default FALSE).
#'   If a schema (`keys`) is provided, it is always used for final typing.
#' @param progress Logical. Progress Bar, defaults to TRUE.
#' @param ... Extra args passed to `gpt()` (e.g., `response_format`).
#' @export

gpt_column <- function(data,
                       col,
                       prompt,
                       keys = NULL,
                       provider = c("auto", "local", "openai", "lmstudio", "ollama", "localai"),
                       backend = NULL,
                       base_url = NULL,
                       model = NULL, # <-- new
                       temperature = 0.2,
                       file_path = NULL,
                       image_path = NULL,
                       .coerce_types = TRUE,
                       coerce_when = NULL,
                       infer_types = FALSE,
                       na_values = c("NA", "N/A", "null", "None", ""),
                       auto_correct_keys = getOption("gptr.auto_correct_keys", TRUE),
                       keep_unexpected_keys = getOption("gptr.keep_unexpected_keys", FALSE),
                       fuzzy_model = getOption("gptr.fuzzy_model", "lev_ratio"),
                       fuzzy_threshold = getOption("gptr.fuzzy_threshold", 0.25),
                       relaxed = FALSE,
                       return_debug = TRUE,
                       verbose = FALSE,
                       progress = TRUE,
                       ...) {
    # capture all user extras once
    dots <- rlang::list2(...) # <-- new

    provider <- match.arg(provider)
    if (provider %in% c("lmstudio", "ollama", "localai")) {
        backend <- provider
        provider <- "local"
    }

    # ---------- Helpers ----------------------------------------------------------
    .force_schema_shape <- function(x, expected_keys) {
        # Always return a named list with exactly expected_keys
        if (is.null(expected_keys)) {
            return(x)
        }
        if (is.null(names(x))) x <- list() # unnamed? start from empty
        x <- x[intersect(names(x), expected_keys)] # keep only schema keys
        miss <- setdiff(expected_keys, names(x)) # fill missing as NA
        if (length(miss)) x[miss] <- NA
        x[expected_keys] # order
    }

    .val_to_str <- function(v) {
        if (is.null(v)) {
            return(NA_character_)
        }
        if (is.atomic(v) && length(v) == 1L) {
            return(as.character(v))
        }
        jsonlite::toJSON(v, auto_unbox = TRUE, null = "null")
    }

    norm_type <- function(t) {
        t <- tolower(trimws(as.character(t)))
        if (t %in% c("integer", "int", "long", "whole")) {
            return("integer")
        }
        if (t %in% c("numeric", "double", "float", "number", "real")) {
            return("numeric")
        }
        if (t %in% c("logical", "bool", "boolean")) {
            return("logical")
        }
        if (t %in% c("character", "string", "text")) {
            return("character")
        }
        t
    }
    # Row-level scalar coercion (fast, permissive). Final column coercion/validation happens later.
    cast_one <- function(val, type) {
        type <- norm_type(type)
        if (type == "integer") {
            return(suppressWarnings(as.integer(val)))
        }
        if (type == "numeric") {
            return(suppressWarnings(as.numeric(val)))
        }
        if (type == "logical") {
            if (is.logical(val)) {
                return(val)
            }
            if (is.numeric(val)) {
                return(val != 0)
            }
            if (is.character(val)) {
                v <- tolower(trimws(val))
                return(ifelse(v %in% c("true", "t", "yes", "y", "1", "oui"),
                              TRUE,
                              ifelse(v %in% c("false", "f", "no", "n", "0", "non"), FALSE, NA)
                ))
            }
            return(suppressWarnings(as.logical(val)))
        }
        as.character(val)
    }

    # Ungroup; validate column
    if (dplyr::is_grouped_df(data)) data <- dplyr::ungroup(data)
    col_quo <- rlang::enquo(col)
    col_name <- rlang::as_name(col_quo)
    if (!col_name %in% names(data)) stop("Column '", col_name, "' not found in `data`.", call. = FALSE)

    texts <- dplyr::pull(data, !!col_quo)
    if (!is.character(texts)) texts <- as.character(texts)
    n <- length(texts)

    # ---------- 2) Prepare schema & prompt builder -------------------------------
    # Policies implemented:
    #  - With schema (keys provided):
    #      * keep_unexpected_keys = FALSE --> drop extras; log them in .invalid_detail
    #      * keep_unexpected_keys = TRUE  --> collect extras in a single .extras_json column (no extra cols)
    #  - Without schema (keys = NULL):
    #      * keep_unexpected_keys = FALSE --> debug-only (df + .raw_output + .invalid_rows + .invalid_detail)
    #      * keep_unexpected_keys = TRUE  --> same + .parsed_json (minified JSON of the whole parsed row)

    # WHAT THE USER PROVIDES (example):
    # keys <- list(
    #   impulsivite         = "integer",
    #   hypersexualite      = "integer",
    #   trouble_alimentaire = c("léger","modéré","sévère"),
    #   jeu_pathologique    = "integer"
    # )
    #
    # WHY key_specs: normalize each entry to a consistent shape per key:
    #   .parse_key_spec("integer") -> list(type="integer", allowed=NULL)
    #   .parse_key_spec(c("léger","modéré")) -> list(type=NULL, allowed=c(...))
    #
    # WHY expected_keys: the ordered vector of column names, used to pad/order and
    # drive json_keys_align() / row_to_tibble() / .finalize_columns().
    key_specs <- NULL # named list: key -> list(type=..., allowed=...)
    expected_keys <- NULL # character vector of column names (order matters)
    if (!is.null(keys)) {
        stopifnot(is.list(keys), !is.null(names(keys)))
        key_specs <- purrr::map(keys, .parse_key_spec)
        expected_keys <- names(keys)
    }

    # Build prompt from template or function
    make_prompt_for <- function(.x_text) {
        .x_trim <- trimws(.x_text)
        # Modes:
        #  1) Template string with placeholders:
        #     prompt <- 'Text: "{text}"\nReturn JSON: {json_format}'
        #     build_prompt() substitutes {text} and a schema-based {json_format}
        #  2) Function:
        #     prompt <- function(text, keys) paste("Analyse:\n", text, "\nKeys:", paste(names(keys), collapse=", "))
        if (is.function(prompt)) prompt(.x_trim, keys) else build_prompt(prompt, text = .x_trim, keys = keys)
    }

    # --- build a stable forward-args list for gpt() ---
    forward_args <- Filter(
        Negate(is.null),
        list(
            provider = provider,
            backend  = backend,
            base_url = base_url,
            model    = model
        )
    )

    # --- ------ 3) PER-ROW CALL ----
    call_gpt <- function(i) {
        txt <- texts[[i]]
        if (is.na(txt) || !nzchar(trimws(txt))) {
            return(NA_character_)
        }
        input_prompt <- make_prompt_for(txt)

        do.call(
            gpt,
            c(
                list(
                    prompt      = input_prompt,
                    temperature = temperature,
                    file_path   = file_path,
                    image_path  = image_path
                ),
                forward_args,
                dots
            )
        )
    }

    raw_outputs <- character(n)
    parsed_results <- vector("list", n) # list of named lists (schema) or scalars/lists (no schema)
    invalid_flags <- logical(n) # TRUE when row invalid (parse/validate failures)
    invalid_detail <- vector("list", n) # per-row meta (data.frame or NULL)

    # For schema + keep_unexpected_keys = TRUE we collect extras here
    extras_list <- if (!is.null(expected_keys) && isTRUE(keep_unexpected_keys)) vector("list", n) else NULL

    process_response <- function(i) {
        out <- raw_outputs[[i]]

        # Missing model output
        if (is.na(out) || !nzchar(out)) {
            invalid_flags[i] <<- TRUE
            invalid_detail[[i]] <<- data.frame(stage = "model", note = "empty_output", stringsAsFactors = FALSE)
            parsed_results[[i]] <<- if (!is.null(expected_keys)) setNames(rep(NA, length(expected_keys)), expected_keys) else NA_character_
            return()
        }

        # --- SHIM: unwrap quoted JSON early (even if parser/.tidy_json miss) ---
        if (is.character(out) && length(out) == 1L) {
            val0 <- try(jsonlite::fromJSON(out, simplifyVector = FALSE), silent = TRUE)
            if (!inherits(val0, "try-error") && is.character(val0) && length(val0) == 1L) {
                out <- val0 # now bare JSON: {"impulsivite":1,...}
            } else if (grepl('^\\s*"(\\{|\\[)', out) && grepl('(\\}|\\])"\\s*$', out) && grepl('\\\\\"', out)) {
                # hard fallback: looks like a quoted blob with escaped quotes → strip outer quotes, unescape
                inner <- sub('^\\s*"(.*)"\\s*$', "\\1", out, perl = TRUE)
                out <- gsub('\\\\\\"', '"', inner, perl = TRUE)
            }
        }

        # Parse/repair JSON; no coercion here (column pass handles that)
        rp <- tryCatch(
            json_fix_parse_validate(
                out,
                key_specs    = key_specs,
                na_values    = na_values,
                verbose      = verbose,
                i            = i,
                .coerce_types = FALSE,
                coerce_when  = NULL
            ),
            error = function(e) {
                if (verbose) message("Row ", i, ": parse error --> ", conditionMessage(e))
                list(ok = FALSE, value = NULL, meta = NULL)
            }
        )

        invalid <- !isTRUE(rp$ok)
        meta_df <- if (is.data.frame(rp$meta)) rp$meta else NULL
        if (is.data.frame(meta_df)) {
            bad <- meta_df[(meta_df$type_ok %in% FALSE) | (meta_df$allowed %in% FALSE), , drop = FALSE]
            if (nrow(bad)) invalid <- TRUE
        }

        if (!isTRUE(rp$ok)) {
            parsed_results[[i]] <<- if (relaxed && is.null(expected_keys)) out else if (!is.null(expected_keys)) setNames(rep(NA, length(expected_keys)), expected_keys) else out
            invalid_flags[i] <<- TRUE
            invalid_detail[[i]] <<- meta_df
            return()
        }

        x <- rp$value # named list (usually)

        # Align keys when schema exists -------------------------------------------
        if (!is.null(expected_keys) && !setequal(names(x), expected_keys)) {
            orig_names <- names(x)
            x <- json_keys_align(
                x,
                expected_keys   = expected_keys,
                auto_correct    = auto_correct_keys,
                keep_unexpected = keep_unexpected_keys,
                fuzzy_model     = fuzzy_model,
                fuzzy_threshold = fuzzy_threshold
            )

            # Audit unexpected keys
            if (!isTRUE(keep_unexpected_keys)) {
                dropped <- setdiff(orig_names, names(x))
                if (length(dropped)) {
                    meta_drop <- data.frame(
                        stage = "align_keys",
                        issue = "unexpected_key",
                        key = dropped,
                        value = vapply(dropped, function(k) .val_to_str(rp$value[[k]]), character(1)),
                        action = "dropped",
                        stringsAsFactors = FALSE
                    )
                    meta_df <- if (is.null(meta_df)) {
                        meta_drop
                    } else {
                        tryCatch(
                            dplyr::bind_rows(meta_df, meta_drop),
                            error = function(e) rbind(meta_df, meta_drop)
                        )
                    }
                }
            } else {
                # keep_unexpected_keys = TRUE → collect extras (no extra columns)
                extra_names <- setdiff(names(x), expected_keys)
                if (length(extra_names)) {
                    if (!is.null(extras_list)) extras_list[[i]] <- x[extra_names]
                    # (optional) log as kept extras
                    meta_keep <- data.frame(
                        stage = "align_keys",
                        issue = "unexpected_key",
                        key = extra_names,
                        value = vapply(extra_names, function(k) .val_to_str(x[[k]]), character(1)),
                        action = "kept_in_extras",
                        stringsAsFactors = FALSE
                    )
                    meta_df <- if (is.null(meta_df)) {
                        meta_keep
                    } else {
                        tryCatch(
                            dplyr::bind_rows(meta_df, meta_keep),
                            error = function(e) rbind(meta_df, meta_keep)
                        )
                    }
                    # keep only schema keys in x for the main columns
                    x <- x[intersect(names(x), expected_keys)]
                }
            }
        }

        # Ensure shape now
        x <- .force_schema_shape(x, expected_keys)

        # Row-level coercion for schema (or explicit coerce_when)
        if (isTRUE(.coerce_types) && !is.null(key_specs)) {
            for (k in intersect(names(x), names(key_specs))) {
                tt <- key_specs[[k]]$type
                if (!is.null(tt)) x[[k]] <- cast_one(x[[k]], tt)
            }
        } else if (!is.null(coerce_when) && isTRUE(.coerce_types)) {
            for (k in intersect(names(x), names(coerce_when))) {
                tt <- coerce_when[[k]]
                if (!is.null(tt)) x[[k]] <- cast_one(x[[k]], tt)
            }
        }

        parsed_results[[i]] <<- x
        invalid_flags[i] <<- invalid
        invalid_detail[[i]] <<- meta_df
    }

    use_progressr <- isTRUE(progress) && requireNamespace("progressr", quietly = TRUE)

    # If there are zero rows, bail early to avoid 0-step progressors
    if (n == 0L) return(data)

    if (use_progressr) {
        progressr::with_progress({
            p  <- progressr::progressor(steps = n)
            t0 <- Sys.time()
            for (i in seq_len(n)) {
                raw_outputs[[i]] <- call_gpt(i)
                process_response(i)
                elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
                p(message = sprintf("row %d/%d elapsed %ds", i, n, round(elapsed)))
            }
        })
    } else {
        # progress disabled or progressr not installed: silent execution
        if (isTRUE(progress) && !requireNamespace("progressr", quietly = TRUE)) {
            message("Tip: install.packages('progressr') to see a live progress bar.")
        }
        for (i in seq_len(n)) {
            raw_outputs[[i]] <- call_gpt(i)
            process_response(i)
        }
    }

    # ---------- 3b) Short-circuit for NO-SCHEMA cases ---------------------------
    if (is.null(keys)) {
        result <- data
        n_out <- nrow(result)

        # Normalize debug vectors/lists
        raw_outputs <- as.character(rep_len(raw_outputs, n_out))
        invalid_flags <- as.logical(rep_len(invalid_flags, n_out))
        if (length(invalid_detail) != n_out) length(invalid_detail) <- n_out

        if (isTRUE(return_debug)) {
            if (".raw_output" %in% names(result)) {
                result$.raw_output <- raw_outputs
            } else {
                result <- tibble::add_column(result, .raw_output = raw_outputs, .after = col_name)
            }
            result$.invalid_rows <- invalid_flags
            result$.invalid_detail <- invalid_detail
        }

        # keep_unexpected_keys guides compact payload:
        #  FALSE --> debug-only
        #  TRUE  --> add .parsed_json (minified JSON of whole parsed object)
        if (isTRUE(keep_unexpected_keys)) {
            parsed_json <- vapply(
                parsed_results,
                function(x) {
                    if (is.null(x) || (is.character(x) && length(x) == 1L && is.na(x))) {
                        return(NA_character_)
                    }
                    if (is.character(x) && length(x) == 1L) {
                        return(x)
                    } # relaxed scalar passthrough
                    jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", POSIXt = "ISO8601")
                },
                FUN.VALUE = character(1)
            )
            if (".parsed_json" %in% names(result)) {
                result$.parsed_json <- parsed_json
            } else {
                result <- tibble::add_column(result, .parsed_json = parsed_json, .after = col_name)
            }
        }

        # Clean internal noise (preserve debug cols if present)
        if (isTRUE(return_debug)) {
            keep_debug <- c(".raw_output", ".invalid_rows", ".invalid_detail", ".parsed_json")
            junk <- c("type", "allowed", "valid", ".valid", ".raw", ".error", "..raw_json", "..parse_error", ".path")
            to_drop <- setdiff(junk, keep_debug)
            result <- result[, setdiff(names(result), to_drop), drop = FALSE]
        }

        attr(result, "invalid_rows") <- which(invalid_flags)
        return(result)
    }

    # temporary debug
    if (verbose) {
        bad_idx <- which(vapply(parsed_results, function(z) is.null(names(z)), logical(1)))
        if (length(bad_idx)) {
            message("Found ", length(bad_idx), " rows with no names: indices = ", paste(head(bad_idx, 10), collapse = ", "))
        }
        wrong_names <- which(!vapply(parsed_results, function(z) setequal(names(z), expected_keys), logical(1)))
        if (length(wrong_names)) {
            message("Rows with wrong names: ", paste(head(wrong_names, 10), collapse = ", "))
            # print one example
            print(names(parsed_results[[wrong_names[1]]]))
        }
    }


    # ---------- 4) Bind rows --> tibble of schema columns -------------------------

    #safe_tibble() wraps row_to_tibble() so that NULL/NA values still yield a row
    safe_tibble <- function(x) {
        # if x is NULL or scalar NA, produce a row of NAs for all expected keys
        if (is.null(x) || (is.character(x) && length(x) == 1L && is.na(x))) {
            return(row_to_tibble(setNames(rep(NA, length(expected_keys)), expected_keys),
                                 expected_keys = expected_keys))
        }
        # if x has no names but a schema exists, also return all NAs
        if (is.null(names(x)) && !is.null(expected_keys)) {
            return(row_to_tibble(setNames(rep(NA, length(expected_keys)), expected_keys),
                                 expected_keys = expected_keys))
        }
        row_to_tibble(x, expected_keys = expected_keys)
    }

    parsed_df <- purrr::map_dfr(parsed_results, safe_tibble)


    # ---------- 5) Finalize (schema) --------------------------------------------
    # Column-level enforcement (types/allowed/NA policy). Set mode="as_is" if you
    # want to skip column coercion and rely only on row-level casting.
    mode <- "schema"
    parsed_df <- .finalize_columns(
        parsed_df,
        expected_keys = expected_keys,
        key_specs     = key_specs,
        mode          = mode
    )

    # Drop helper from relaxed paths (shouldn't appear in schema paths, but harmless)
    if (".parsed" %in% names(parsed_df)) parsed_df$.parsed <- NULL

    # ---------- 6) Bind back to input; attach extras/debug ----------------------
    #assign a row id to preserve ordering and avoid recycling mismatches
    data$.row_id <- seq_len(nrow(data))
    parsed_df$.row_id <- seq_len(nrow(parsed_df))
    result <- dplyr::left_join(data, parsed_df, by = ".row_id")
    # drop the helper column after the join
    result$.row_id <- NULL

    n_out <- nrow(result)

    # Normalize debug vectors/lists
    raw_outputs <- as.character(rep_len(raw_outputs, n_out))
    invalid_flags <- as.logical(rep_len(invalid_flags, n_out))
    if (length(invalid_detail) != n_out) length(invalid_detail) <- n_out

    # Schema + keep_unexpected_keys = TRUE --> add single .extras_json column
    if (!is.null(extras_list) && isTRUE(keep_unexpected_keys)) {
        extras_json <- vapply(
            extras_list,
            function(e) if (length(e)) jsonlite::toJSON(e, auto_unbox = TRUE, null = "null") else NA_character_,
            FUN.VALUE = character(1)
        )
        if (".extras_json" %in% names(result)) {
            result$.extras_json <- extras_json
        } else {
            result <- tibble::add_column(result, .extras_json = extras_json, .after = col_name)
        }
    }

    # Debug columns (add/replace, no duplicates)
    if (isTRUE(return_debug)) {
        if (".raw_output" %in% names(result)) {
            result$.raw_output <- raw_outputs
        } else {
            result <- tibble::add_column(result, .raw_output = raw_outputs, .after = col_name)
        }
        result$.invalid_rows <- invalid_flags
        result$.invalid_detail <- invalid_detail
    }

    # Drop noisy internal meta while preserving debug/extras
    keep_debug <- c(".raw_output", ".invalid_rows", ".invalid_detail", ".extras_json")
    junk <- c("type", "allowed", "valid", ".valid", ".raw", ".error", "..raw_json", "..parse_error", ".path")
    to_drop <- setdiff(junk, keep_debug)
    result <- result[, setdiff(names(result), to_drop), drop = FALSE]

    # Back-compat: indices of invalid rows
    attr(result, "invalid_rows") <- which(invalid_flags)
    result
}



