#' Retry Failed Rows in a `gpt_column()` Call
#'
#' Attempts to re-run `gpt_column()` on rows that previously failed validation,
#' updating the original dataset with any successful retries.
#'
#' This is intended for use when `gpt_column()` returns a tibble with an
#' `"invalid_rows"` attribute indicating which rows failed. The function will
#' loop through those rows, retrying each one up to `max_attempts` times, and
#' patch the original data frame in-place for any rows that succeed.
#'
#' @param data A data frame that was processed by `gpt_column()` and has an
#'   `"invalid_rows"` attribute (integer vector of row indices that failed).
#' @param prompt Character scalar. The prompt string to pass back into
#'   `gpt_column()`.
#' @param col Column in `data` containing the text to be processed by
#'   `gpt_column()`. Use tidy-eval notation (unquoted column name).
#' @param id_col Column in `data` that uniquely identifies each row. Used to
#'   join retry results back into the main dataset. Must have unique values.
#' @param keys Optional named list of expected output keys and their R types,
#'   passed to `gpt_column()`.
#' @param max_attempts Integer. Maximum number of attempts per row before
#'   giving up. Default is 3.
#' @param auto_correct_keys Logical. Whether to enable key name autocorrection
#'   in `gpt_column()`. Default is `TRUE`.
#' @param relaxed Logical. Whether to run in relaxed mode in `gpt_column()`.
#'   Default is `TRUE`.
#' @param print_retry Logical. If `TRUE` (default), prints the combined retry
#'   results for inspection (truncated after 50 rows).
#' @param ... Additional arguments passed to `gpt_column()`.
#'
#' @return A data frame with successful retries merged back in, and an updated
#'   `"invalid_rows"` attribute containing the indices of any rows that still
#'   failed after all attempts.
#'
#' @details
#' The function:
#' 1. Checks the `"invalid_rows"` attribute of `data`.
#' 2. Loops through only those rows that failed.
#' 3. Calls `gpt_column()` on each row individually, catching errors.
#' 4. Immediately updates the original data for any rows that pass validation.
#' 5. Retries remaining failures until `max_attempts` is reached or all pass.
#'
#' This sequential version does **not** support parallel retries. It is safer
#' for debugging and avoids concurrency issues.
#'
#' @examples
#' \dontrun{
#' # Suppose 'df' was processed by gpt_column() and has invalid rows
#' attr(df, "invalid_rows")
#'
#' # Retry failed rows up to 2 times
#' df <- patch_failed_rows(
#'   data = df,
#'   prompt = "Extract key medical variables.",
#'   col = note_text,
#'   id_col = patient_id,
#'   keys = list(age = "integer", diagnosis = "character"),
#'   max_attempts = 2
#' )
#'
#' # Inspect which rows still failed
#' attr(df, "invalid_rows")
#' }
#'
#' @seealso
#' [gpt_column()] for the main extraction function.
#'
#' @export


patch_failed_rows <- function(data,
                              prompt,
                              col,
                              id_col,
                              keys = NULL,
                              max_attempts = 3,
                              auto_correct_keys = TRUE,
                              relaxed = TRUE,
                              print_retry = TRUE,
                              ...) {
  # --- deps ---
  for (pkg in c("dplyr", "purrr", "progressr", "rlang")) {
    if (!requireNamespace(pkg, quietly = TRUE)) stop("Missing package: ", pkg, call. = FALSE)
  }

  # --- quosures & names ---
  col_quo <- rlang::enquo(col)
  id_col_quo <- rlang::enquo(id_col)
  col_name <- rlang::as_name(col_quo)
  id_col_name <- rlang::as_name(id_col_quo)

  # --- fetch rows to retry ---
  invalid_rows <- attr(data, "invalid_rows")
  if (is.null(invalid_rows) || length(invalid_rows) == 0) {
    message("No failed rows to retry.")
    return(data)
  }

  # Ensure key uniqueness (rows_update requires unique key)
  if (anyDuplicated(dplyr::pull(data, !!id_col_quo))) {
    stop("`", id_col_name, "` contains duplicates. `rows_update()` requires unique keys.", call. = FALSE)
  }

  rows_to_retry <- data[invalid_rows, c(id_col_name, col_name), drop = FALSE]

  # --- runner (one row) ---
  run_one <- function(row_df) {
    out <- tryCatch(
      gpt_column(
        data = row_df,
        col = !!col_quo,
        prompt = prompt,
        keys = keys,
        auto_correct_keys = auto_correct_keys,
        relaxed = relaxed,
        return_debug = TRUE,
        ...
      ),
      error = function(e) {
        tibble::as_tibble(row_df) %>%
          dplyr::mutate(.error = conditionMessage(e), .invalid_rows = 1L)
      }
    )
    # Ensure id column is present for rows_update join
    if (!id_col_name %in% names(out)) {
      out[[id_col_name]] <- row_df[[id_col_name]]
    }
    out
  }

  # --- progress + retries ---
  n <- nrow(rows_to_retry)
  all_results <- list()
  remaining <- rows_to_retry

  progressr::handlers("progress")
  progressr::with_progress({
    p <- progressr::progressor(steps = n * max_attempts)

    for (attempt in seq_len(max_attempts)) {
      if (nrow(remaining) == 0) break

      batch <- purrr::map(seq_len(nrow(remaining)), function(i) {
        p(sprintf("Attempt %d | row %d/%d", attempt, i, nrow(remaining)))
        run_one(remaining[i, , drop = FALSE])
      })

      batch_df <- dplyr::bind_rows(batch)
      all_results[[attempt]] <- batch_df

      # identify successes (expect .invalid_rows == 0)
      successes <- dplyr::filter(batch_df, .invalid_rows == 0L || .invalid_rows == FALSE)
      if (nrow(successes) > 0) {
        # Apply successful patches immediately
        data <- dplyr::rows_update(data, successes, by = id_col_name)
      }

      # compute who is still failing
      failed_ids <- dplyr::anti_join(
        remaining[, id_col_name, drop = FALSE],
        successes[, id_col_name, drop = FALSE],
        by = id_col_name
      )[[id_col_name]]

      remaining <- if (length(failed_ids)) {
        dplyr::semi_join(rows_to_retry, tibble::tibble(!!id_col_name := failed_ids), by = id_col_name)
      } else {
        remaining[0, , drop = FALSE]
      }
    }
  })

  retry_results <- dplyr::bind_rows(all_results)
  if (isTRUE(print_retry)) {
    print(retry_results, n = min(nrow(retry_results), 50))
    if (nrow(retry_results) > 50) message("... output truncated.")
  }

  # --- update invalid_rows attribute on the returned data ---
  if (nrow(remaining) > 0) {
    idx_still_invalid <- which(dplyr::pull(data, !!id_col_quo) %in% remaining[[id_col_name]])
  } else {
    idx_still_invalid <- integer(0)
  }
  attr(data, "invalid_rows") <- idx_still_invalid

  data
}
