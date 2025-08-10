#' List Available Models from the OpenAI API
#'
#' Retrieves and returns the list of models available to the authenticated user
#' from the [OpenAI API](https://platform.openai.com/docs/api-reference/models/list).
#'
#' @param url Character string giving the OpenAI models endpoint.
#'   Defaults to `"https://api.openai.com/v1/models"`.
#'
#' @return A [tibble][tibble::tibble] where each row represents a model and columns
#'   typically include:
#'   \describe{
#'     \item{`id`}{The model identifier (e.g., `"gpt-4o"`, `"text-embedding-ada-002"`).}
#'     \item{`object`}{The type of object, usually `"model"`.}
#'     \item{`owned_by`}{The owner of the model (e.g., `"openai"`, `"system"`, `"user"`).}
#'     \item{`created`}{The creation date of the model, converted to a `Date` object.}
#'     Additional columns may be present depending on OpenAI's API version.
#'   }
#'
#' @details
#' This function sends a GET request to the specified OpenAI API endpoint,
#' authenticating with the `OPENAI_API_KEY` environment variable.
#' The JSON response is parsed, and the `data` field is converted into a tidy tibble.
#'
#' The `created` field is returned as a UNIX timestamp in seconds by the API
#' and is converted to a `Date` for convenience.
#'
#' An error is raised if:
#' \itemize{
#'   \item The `OPENAI_API_KEY` environment variable is missing or invalid.
#'   \item The request fails (e.g., network issues, unauthorized access).
#' }
#'
#' @examples
#' \dontrun{
#' # Make sure your API key is set in the environment:
#' Sys.setenv(OPENAI_API_KEY = "sk-...")
#'
#' # List available models
#' list_openai_models()
#'
#' # List models from a custom endpoint (rare)
#' list_openai_models("https://api.openai.com/v1/models")
#' }
#'
#' @seealso
#' \itemize{
#'   \item [httr::GET()] for making HTTP requests.
#'   \item [jsonlite::fromJSON()] for parsing JSON.
#'   \item OpenAI Models API: https://platform.openai.com/docs/api-reference/models
#' }
#'
#' @export
list_openai_models <- function(url = "https://api.openai.com/v1/models") {
    api_key <- Sys.getenv("OPENAI_API_KEY")

    res <- GET(
        url,
        add_headers(Authorization = paste("Bearer", api_key))
    )

    stop_for_status(res)

    models_df <- content(res, as = "text") |>
        fromJSON(flatten = TRUE) |>
        (\(x) x$data)() |>
        as_tibble() %>%
        mutate(created = as_date(seconds(created))) %>%
        arrange(desc(created))

    return(models_df)
}
