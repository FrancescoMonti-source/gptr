fake_resp <- function(model = "dummy") {
  body <- list(
    model = model,
    choices = list(list(message = list(content = "ok")))
  )
  resp <- httr2::response(
    status = 200L,
    body = charToRaw(jsonlite::toJSON(body, auto_unbox = TRUE)),
    headers = list("content-type" = "application/json")
  )
  list(body = body, resp = resp)
}
