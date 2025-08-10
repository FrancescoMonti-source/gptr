list_lmstudio_models <- function(path = "C:/Users/franc/.lmstudio/models") {
    if (!dir.exists(path)) {
        stop("📂 The LM Studio models folder does not exist at: ", path)
    }

    models <- list.dirs(path, full.names = FALSE, recursive = TRUE)

    if (length(models) == 0) {
        cat("ℹ️ No models found in:", path, "\n")
        return(invisible(NULL))
    }

    cat("🧠 Available LM Studio models:\n")
    for (m in models) cat("- ", m, "\n")

    invisible(models)
}
