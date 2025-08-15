gpt_chat_summarise <- function(
        model = "mistralai/mistral-7b-instruct-v0.3",
        base_url = "http://127.0.0.1:1234/v1/chat/completions",
        temperature = 0.3
) {
    if (!exists("gpt_chat")) stop("gpt_chat() must be initialized first.")

    # Grab and clone current chat environment
    env <- environment(gpt_chat)
    if (!exists("history", envir = env)) stop("No chat history to summarize.")

    history <- get("history", envir = env)

    if (length(history) < 2) {
        message("Not enough history to summarize.")
        return(invisible(NULL))
    }

    # Format full history as readable text
    history_text <- paste(
        purrr::map_chr(history, function(x) {
            role <- toupper(x$role)
            if (is.character(x$content)) return(paste0(role, ": ", x$content))
            texts <- purrr::map_chr(x$content, function(b) if (b$type == "text") b$text else "[image]")
            paste0(role, ": ", paste(texts, collapse = "\n"))
        }),
        collapse = "\n\n"
    )

    # Create summarization prompt
    summary_prompt <- list(
        list(role = "user", content = paste0(
            "Voici l'historique de la conversation :\n\n", history_text,
            "\n\nFais un résumé concis de cette conversation en français."
        ))
    )

    response <- httr::POST(
        url = base_url,
        httr::add_headers("Content-Type" = "application/json"),
        body = jsonlite::toJSON(list(
            model = model,
            messages = summary_prompt,
            temperature = temperature
        ), auto_unbox = TRUE),
        encode = "json"
    )

    if (httr::status_code(response) != 200) {
        warning("Summarization failed: ", httr::content(response, "text", encoding = "UTF-8"))
        return(NULL)
    }

    summary_text <- httr::content(response, "parsed")$choices[[1]]$message$content

    # Inject summary as user message
    summary_message <- list(
        role = "user",
        content = paste("Résumé de la conversation précédente :", summary_text)
    )

    assign("history", list(summary_message), envir = env)
    message("Summary injected into chat history.")
}
