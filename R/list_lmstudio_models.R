#' List Available Models from an LM Studio API Server
#'
#' Retrieves and returns the list of models available from a running
#' [LM Studio](https://lmstudio.ai) API endpoint.
#'
#' @param url Character string giving the base URL of the LM Studio models endpoint.
#'   Defaults to `"http://127.0.0.1:1234/v1/models"`, which is the default
#'   local server URL for LM Studio.
#'
#' @return A [tibble][tibble::tibble] where each row represents a model and columns
#'   typically include:
#'   \describe{
#'     \item{`id`}{The model identifier (e.g., `"mistralai/mistral-7b-instruct-v0.3"`).}
#'     \item{`object`}{The type of object, usually `"model"`.}
#'     \item{`owned_by`}{The owner or source of the model (e.g., `"organization_owner"`).}
#'     Additional columns may be returned depending on LM Studio's API version.
#'   }
#'
#' @details
#' This function sends a GET request to the specified LM Studio API endpoint,
#' parses the JSON response, and converts the `data` field into a tidy tibble.
#'
#' It will raise an error if the request fails (e.g., LM Studio server is not running,
#' wrong port, or invalid endpoint).
#'
#' @examples
#' \dontrun{
#' # List models from a locally running LM Studio server:
#' list_lmstudio_models()
#'
#' # List models from a remote LM Studio instance:
#' list_lmstudio_models("http://192.168.1.50:1234/v1/models")
#' }
#'
#' @seealso
#' \itemize{
#'   \item [httr::GET()] for making HTTP requests.
#'   \item [jsonlite::fromJSON()] for parsing JSON.
#'   \item LM Studio API docs: https://lmstudio.ai
#' }
#'
#' @export
list_lmstudio_models <- function(url = "http://127.0.0.1:1234/v1/models") {
    res <- GET(url)
    stop_for_status(res)
    models_df <- content(res, as = "text") |>
        fromJSON(flatten = TRUE) |>
        (\(x) x$data)() |>
        as_tibble()
    return(models_df)
}
