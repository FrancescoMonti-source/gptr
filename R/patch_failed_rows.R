#' Retry and Patch Failed Rows from `gpt_column()` Output
#'
#' This function retries only the rows that previously failed when using `gpt_column()`,
#' identified via the `"invalid_rows"` attribute. It reprocesses these rows in parallel
#' (if enabled) and patches the successful outputs back into the original dataset.
#'
#' The function is useful for recovering from transient parsing or model errors,
#' especially when using local LLMs or fragile prompts.
#'
#' @param data A tibble returned by `gpt_column()`, with the `"invalid_rows"` attribute.
#' @param prompt The same prompt or prompt function used in the original `gpt_column()` call.
#' @param col Unquoted name of the column containing free-text input sent to the model.
#' @param id_col Unquoted name of a column uniquely identifying rows (e.g., `PATID`). Used to patch results.
#' @param keys (Optional) Named list of expected output keys and their types (e.g., `list(foo = "integer")`).
#' @param max_attempts Not currently used in this version. Included for future compatibility.
#' @param auto_correct_keys Logical. Whether to allow fuzzy correction of unexpected keys. Default: `TRUE`.
#' @param relaxed Logical. If `TRUE`, tolerate missing or extra keys (relaxed JSON validation). Default: `TRUE`.
#' @param parallel Logical. If `TRUE`, uses parallel evaluation via `furrr::future_map_dfr()`. Default: `FALSE`.
#' @param print_retry Logical. If `TRUE`, prints the result of retried rows. Useful for debugging. Default: `FALSE`.
#' @param ... Additional arguments passed to `gpt_column()`.
#'
#' @return A tibble of the same structure as the input `data`, with successful retries patched in.
#'         The `"invalid_rows"` attribute is updated to reflect remaining failures.
#'
#' @details
#' - Only rows marked as invalid are retried.
#' - Progress is tracked via `{progressr}`.
#' - Parallelization is controlled by `{furrr}` and requires an active `future::plan()`.
#' - Each row is retried independently; failed retries are left unchanged.
#'
#' @examples
#' \dontrun{
#' result <- gpt_column(data = df, col = note, prompt = prompt, keys = keys)
#'
#' # Retry failed rows by patient ID:
#' result <- patch_failed_rows(
#'   data = result,
#'   prompt = prompt,
#'   col = note,
#'   id_col = PATID,
#'   keys = keys
#' )
#'
#' # Check which rows still failed:
#' attr(result, "invalid_rows")
#' }
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
                              parallel = FALSE,
                              print_retry = TRUE,...) {
    require(dplyr)
    require(purrr)
    require(progressr)
    require(rlang)

    id_col_quo <- enquo(id_col)
    col_name <- as_name(enquo(col))
    id_col_name <- as_name(id_col_quo)

    invalid_rows <- attr(data, "invalid_rows")
    if (is.null(invalid_rows) || length(invalid_rows) == 0) {
        message("No failed rows to retry.")
        return(data)
    }

    rows_to_retry <- data[invalid_rows, c(id_col_name, col_name)]

    progressr::handlers("progress")
    retry_results <- progressr::with_progress({
        p <- progressr::progressor(steps = nrow(rows_to_retry))
        map_dfr(
            1:nrow(rows_to_retry),
            function(i) {
                row <- rows_to_retry[i, , drop = FALSE]
                result <- gpt_column(
                    data = row,
                    col = !!col_quo,
                    prompt = prompt,
                    keys = keys,
                    auto_correct_keys = auto_correct_keys,
                    relaxed = relaxed,
                    parallel = parallel,
                    return_debug = TRUE,
                    ...
                )
                p(sprintf("Row %d/%d", i, nrow(rows_to_retry)))
                result
            }
        )
    })

    if (print_retry) print(retry_results)
    retry_success <- retry_results %>% filter(.invalid_rows == 0)

    resultat <- dplyr::rows_update(data, retry_success, by = id_col_name)
    return(resultat)
}
