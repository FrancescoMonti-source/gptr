# Generic "list models" for OpenAI-compatible servers ----------------------

# If you already have these helpers elsewhere, keep yours and delete these.
.mk_model_urls <- function(base_url) {
    bu <- sub("/+$", "", base_url)
    unique(c(
        sub("/v1/chat/completions$", "/v1/models", bu, perl = TRUE),
        sub("/chat/completions$",    "/models",    bu, perl = TRUE),
        paste0(bu, "/v1/models"),
        paste0(bu, "/models")
    ))
}

.which_backend_from_base_url <- function(base_url) {
    bu <- tolower(base_url %||% "")
    if (grepl("api\\.openai\\.com|\\.openai\\.azure\\.com", bu)) return("openai")
    if (grepl(":1234",  bu)) return("lmstudio")
    if (grepl(":11434", bu)) return("ollama")
    if (grepl(":8080",  bu)) return("localai")
    if (grepl("^(https?://)?(127\\.0\\.0\\.1|localhost)", bu)) return("local")
    "unknown"
}

# --- Core parser for /v1/models (OpenAI-compatible) -----------------------

.list_models_openai_compat <- function(url, timeout = getOption("gpt.timeout", 5), headers = list(), backend = NA_character_) {
    req <- httr2::request(url) |>
        httr2::req_timeout(timeout) |>
        httr2::req_user_agent("gptr (+list_models)") |>
        httr2::req_retry(
            max_tries = 3,
            backoff = function(i) 0.2 * i,
            is_transient = function(resp) {
                sc <- try(httr2::resp_status(resp), silent = TRUE)
                if (inherits(sc, "try-error")) return(TRUE)
                sc %in% c(408, 429, 500, 502, 503, 504)
            }
        )
    if (length(headers)) req <- httr2::req_headers(req, !!!headers)

    resp <- httr2::req_perform(req)
    httr2::resp_check_status(resp)

    txt <- httr2::resp_body_string(resp)

    # Try a flattened parse first
    j_flat <- tryCatch(jsonlite::fromJSON(txt, flatten = TRUE), error = function(e) NULL)
    if (is.list(j_flat)) {
        if (is.data.frame(j_flat$data))   return(tibble::as_tibble(j_flat$data)   |> tibble::add_column(backend = backend, .before = 1))
        if (is.data.frame(j_flat$models)) return(tibble::as_tibble(j_flat$models) |> tibble::add_column(backend = backend, .before = 1))
    }

    # Lenient fallback
    j <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
    ids <- character(0)
    if (is.list(j) && is.list(j$data)) {
        ids <- vapply(j$data,   function(m) { id <- m$id; if (is.character(id) && length(id)==1L && nzchar(id)) id else "" }, "")
    } else if (is.list(j) && is.list(j$models)) {
        ids <- vapply(j$models, function(m) { id <- m$id; if (is.character(id) && length(id)==1L && nzchar(id)) id else "" }, "")
    } else if (is.character(j)) {
        ids <- j
    }
    tibble::tibble(backend = backend, id = ids[nzchar(ids)])
}

# --- Ollama fallback (/api/tags) ------------------------------------------

.ollama_tags_url_from_base <- function(base_url) {
    # strip path to root host
    proto <- sub("^(https?://).*", "\\1", base_url)
    host  <- sub("^https?://", "", base_url)
    host  <- sub("/.*$", "", host)
    paste0(ifelse(nzchar(proto), proto, "http://"), host, "/api/tags")
}

.list_models_from_ollama_tags <- function(url, timeout = getOption("gpt.timeout", 5), backend = "ollama") {
    resp <- httr2::request(url) |>
        httr2::req_timeout(timeout) |>
        httr2::req_user_agent("gptr (+list_models/ollama)") |>
        httr2::req_retry(
            max_tries = 3,
            backoff = function(i) 0.2 * i,
            is_transient = function(resp) {
                sc <- try(httr2::resp_status(resp), silent = TRUE)
                if (inherits(sc, "try-error")) return(TRUE)
                sc %in% c(408, 429, 500, 502, 503, 504)
            }
        ) |>
        httr2::req_perform()
    httr2::resp_check_status(resp)

    txt <- httr2::resp_body_string(resp)
    j   <- tryCatch(jsonlite::fromJSON(txt, flatten = TRUE), error = function(e) NULL)
    if (is.list(j) && is.data.frame(j$models)) {
        df <- tibble::as_tibble(j$models)
        # common columns: name, modified, size, digest
        if (!"id" %in% names(df)) df$id <- df$name
        return(dplyr::relocate(df, id, .before = 1) |> tibble::add_column(backend = backend, .before = 1))
    }
    tibble::tibble(backend = backend, id = character(0))
}

# --- Public: generic list_models() ----------------------------------------

#' List models from OpenAI, LM Studio, Ollama, or LocalAI.
#'
#' Use either a full `url` (to `/v1/models`) or a `base_url` (we derive candidates),
#' or specify `provider = "lmstudio"|"ollama"|"localai"|"openai"`, or leave as
#' `provider = "auto"` to query whatever is available (OpenAI if key set, plus reachable locals).
#'
#' @param provider "any","lmstudio","ollama","localai","openai".
#' @param url Optional full models URL (e.g. "http://127.0.0.1:11434/v1/models").
#' @param base_url Optional chat-completions base URL; we derive `/v1/models` etc.
#' @param timeout Request timeout, seconds.
#' @param openai_api_key API key for OpenAI (required when provider == "openai" or when url points to api.openai.com).
#' @return A tibble with at least columns `backend` and `id`. Additional columns
#'   are returned when the server exposes richer model metadata.
#' @export

list_models <- function(
        provider = c("any","lmstudio","ollama","localai","openai"),
        url = NULL,
        base_url = NULL,
        timeout = getOption("gpt.timeout", 5),
        openai_api_key = Sys.getenv("OPENAI_API_KEY","")
) {
    provider <- match.arg(provider)

    # 1) If a direct URL is provided, use that.
    if (!is.null(url) && nzchar(url)) {
        b <- .which_backend_from_base_url(url)
        headers <- if (grepl("api\\.openai\\.com", url, ignore.case = TRUE)) list(Authorization = paste("Bearer", openai_api_key)) else list()
        df <- tryCatch(.list_models_openai_compat(url, timeout = timeout, headers = headers, backend = b), error = function(e) NULL)
        if (!is.null(df)) return(df)
        # Ollama fallback if it looks like Ollama host but /v1/models failed
        if (b == "ollama") {
            return(.list_models_from_ollama_tags(.ollama_tags_url_from_base(url), timeout = timeout, backend = "ollama"))
        }
        return(tibble::tibble(backend = b, id = character(0)))
    }

    # 2) If a base_url is provided, derive /models candidates.
    if (!is.null(base_url) && nzchar(base_url)) {
        b <- .which_backend_from_base_url(base_url)
        # OpenAI path (Azure included)
        if (b == "openai") {
            if (!nzchar(openai_api_key)) stop("OPENAI_API_KEY is required for OpenAI.", call. = FALSE)
            return(.list_models_openai_compat("https://api.openai.com/v1/models", timeout = timeout,
                                              headers = list(Authorization = paste("Bearer", openai_api_key)), backend = "openai"))
        }
        # Locals: try standard /models endpoints from base_url
        for (u in .mk_model_urls(base_url)) {
            df <- tryCatch(.list_models_openai_compat(u, timeout = timeout, backend = if (b=="local") "local" else b), error = function(e) NULL)
            if (!is.null(df) && nrow(df)) return(df)
        }
        # Ollama fallback
        if (b == "ollama") {
            return(.list_models_from_ollama_tags(.ollama_tags_url_from_base(base_url), timeout = timeout, backend = "ollama"))
        }
        return(tibble::tibble(backend = b, id = character(0)))
    }

    # 3) Provider explicit (no url/base_url)
    build_default_url <- function(p) {
        switch(p,
               "lmstudio" = getOption("gpt.lmstudio_models_url", "http://127.0.0.1:1234/v1/models"),
               "ollama"   = getOption("gpt.ollama_models_url",   "http://127.0.0.1:11434/v1/models"),
               "localai"  = getOption("gpt.localai_models_url",  "http://127.0.0.1:8080/v1/models"),
               "openai"   = "https://api.openai.com/v1/models",
               NA_character_
        )
    }

    if (provider != "any") {
        if (provider == "openai" && !nzchar(openai_api_key)) stop("OPENAI_API_KEY is required for OpenAI.", call. = FALSE)
        u <- build_default_url(provider)
        headers <- if (provider == "openai") list(Authorization = paste("Bearer", openai_api_key)) else list()
        df <- tryCatch(.list_models_openai_compat(u, timeout = timeout, headers = headers, backend = provider), error = function(e) NULL)
        if (!is.null(df) && nrow(df)) return(df)
        if (provider == "ollama") {
            return(.list_models_from_ollama_tags(.ollama_tags_url_from_base(u), timeout = timeout, backend = "ollama"))
        }
        return(tibble::tibble(backend = provider, id = character(0)))
    }

    # 4) provider == "auto" : combine OpenAI (if key) + any reachable locals
    out <- list()

    # OpenAI (optional)
    if (nzchar(openai_api_key)) {
        out[["openai"]] <- tryCatch(
            .list_models_openai_compat("https://api.openai.com/v1/models",
                                       timeout = timeout,
                                       headers = list(Authorization = paste("Bearer", openai_api_key)),
                                       backend = "openai"),
            error = function(e) tibble::tibble(backend = "openai", id = character(0))
        )
    }

    # Locals from configured candidates (you already have .local_candidates())
    cands <- .local_candidates()
    for (k in names(cands)) {
        # prefer plural probes if present
        probes <- cands[[k]]$probes %||% c(cands[[k]]$probe)
        got <- NULL
        for (p in probes) {
            df <- tryCatch(.list_models_openai_compat(p, timeout = timeout, backend = k), error = function(e) NULL)
            if (!is.null(df) && nrow(df)) { got <- df; break }
        }
        # Ollama fallback if nothing via /v1/models
        if (is.null(got) && k == "ollama") {
            got <- tryCatch(.list_models_from_ollama_tags(.ollama_tags_url_from_base(cands[[k]]$base_url), timeout = timeout, backend = "ollama"),
                            error = function(e) NULL)
        }
        if (!is.null(got)) out[[k]] <- got
    }

    if (!length(out)) return(tibble::tibble(backend = character(0), id = character(0)))
    dplyr::bind_rows(out) %>% arrange(backend,desc(created))
}
