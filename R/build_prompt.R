build_prompt <- function(template, text, keys = NULL) {
    if (is.na(text) || is.null(text) || nchar(text) == 0) {
        return("Texte manquant")
    }

    allowed_values_for_type <- function(type) {
        switch(type,
               "integer" = "0 ou 1 ou NA",
               "numeric" = "un nombre (ex: 3.14)",
               "character" = "\"valeur1\", \"valeur2\"",
               "logical" = "true ou false",
               "unknown"
        )
    }

    json_format <- if (!is.null(keys)) {
        json_lines <- paste(
            names(keys),
            purrr::map_chr(keys, allowed_values_for_type),
            sep = ": ",
            collapse = ", "
        )
        glue::glue("{{ {json_lines} }}")
    } else {
        ""
    }

    glue::glue(template, .envir = rlang::env(text = text, json_format = json_format))
}
