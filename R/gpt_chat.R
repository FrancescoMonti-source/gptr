gpt_chat <- local({
    history <- list()

    .require_package <- function(pkg) {
        if (!requireNamespace(pkg, quietly = TRUE)) {
            answer <- readline(paste0("Package '", pkg, "' is required but not installed. Install it now? [y/N]: "))
            if (tolower(answer) %in% c("y", "yes")) {
                install.packages(pkg)
            } else {
                stop("Missing required package: ", pkg)
            }
        }
    }

    # Load required packages
    purrr::walk(c("base64enc", "pdftools", "tibble", "officer", "purrr"), .require_package)

    function(prompt = NULL,image_path = NULL,file_path = NULL,reset = FALSE,
             show_history = FALSE, show_history_as_text = TRUE,
             model = "mistralai/mistral-7b-instruct-v0.3"){
        if (reset) {
            history <<- list()
            cat("🔄 History reset.\n")
            return(invisible(NULL))
        }

        if (show_history) {
            if (length(history) == 0) {
                cat("⛔️ No conversation history yet.\n")
                return(invisible(NULL))
            }

            if (show_history_as_text) {
                lines <- purrr::map_chr(history, function(x) {
                    role <- x$role
                    if (is.character(x$content)) {
                        msg <- x$content
                    } else {
                        texts <- purrr::map_chr(x$content, function(block) {
                            if (!is.null(block$type) && block$type == "text") block$text else "[non-text content]"
                        })
                        msg <- paste(texts, collapse = "\n")
                    }
                    paste0(toupper(role), ": ", msg)
                })

                cat(paste(lines, collapse = "\n\n"), "\n")
                return(invisible(NULL))
            } else {
                return(
                    tibble::tibble(
                        role = purrr::map_chr(history, "role"),
                        message = purrr::map_chr(history, function(x) {
                            if (is.character(x$content)) return(x$content)
                            texts <- purrr::map_chr(x$content, function(block) {
                                if (!is.null(block$type) && block$type == "text") block$text else "[non-text content]"
                            })
                            paste(texts, collapse = "\n")
                        })
                    )
                )
            }
        }



        if (is.null(prompt)) {
            warning("You must provide a prompt unless you're resetting or showing history.")
            return(invisible(NULL))
        }

        # Handle file input
        file_text <- NULL
        if (!is.null(file_path)) {
            if (!file.exists(file_path)) stop("File not found: ", file_path)
            ext <- tolower(tools::file_ext(file_path))
            if (ext %in% c("txt", "md", "csv", "log")) {
                file_text <- readLines(file_path, warn = FALSE) %>% paste(collapse = "\n")
            } else if (ext == "pdf") {
                file_text <- pdftools::pdf_text(file_path) %>% paste(collapse = "\n")
            } else if (ext == "docx") {
                doc <- officer::read_docx(file_path)
                file_text <- officer::docx_summary(doc)$text %>% na.omit() %>% paste(collapse = "\n")
            } else {
                stop("Unsupported file type: ", ext)
            }
        }

        # Handle image input
        image_block <- NULL
        if (!is.null(image_path)) {
            if (!file.exists(image_path)) stop("Image file not found: ", image_path)
            image_b64 <- base64enc::base64encode(image_path)
            image_uri <- paste0("data:image/png;base64,", image_b64)
            image_block <- list(type = "image_url", image_url = list(url = image_uri))
        }

        # Build message content
        content_blocks <- list(list(type = "text", text = prompt))
        if (!is.null(file_text)) {
            content_blocks <- append(content_blocks, list(list(type = "text", text = paste0("\n\nContenu du fichier :\n", file_text))))
        }
        if (!is.null(image_block)) {
            content_blocks <- append(content_blocks, list(image_block))
        }

        # Update history with user message
        history <<- append(history, list(list(role = "user", content = content_blocks)))

        # Call LM Studio
        response <- httr::POST(
            url = "http://127.0.0.1:1234/v1/chat/completions",
            httr::add_headers("Content-Type" = "application/json"),
            body = jsonlite::toJSON(list(
                model = model,
                messages = history,
                temperature = 0.2
            ), auto_unbox = TRUE),
            encode = "json"
        )

        if (httr::status_code(response) != 200) {
            warning("Request failed: ", httr::content(response, "text", encoding = "UTF-8"))
            return(NULL)
        }

        reply_content <- httr::content(response, "parsed")$choices[[1]]$message$content

        # For consistency, convert it to the same content_blocks format
        assistant_content <- if (is.character(reply_content)) {
            list(list(type = "text", text = reply_content))
        } else {
            reply_content
        }
        history <<- append(history, list(list(role = "assistant", content = assistant_content)))

        cat(reply_content, "\n")
        invisible(reply_content)
    }
})

