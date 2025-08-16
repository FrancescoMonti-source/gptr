# R/providers_local.R (or R/local_detect.R)
# Discovery helpers ---------------------------------------------------------

#' @keywords internal
#' @keywords internal
.local_candidates <- function() {
    lmstudio_base <- getOption("gpt.lmstudio_base_url",
                               "http://127.0.0.1:1234/v1/chat/completions")
    ollama_base   <- getOption("gpt.ollama_base_url",
                               "http://127.0.0.1:11434/v1/chat/completions")
    localai_base  <- getOption("gpt.localai_base_url",
                               "http://127.0.0.1:8080/v1/chat/completions")

    mk_probes <- function(base_url) {
        bu <- sub("/+$", "", base_url)
        unique(c(
            sub("/v1/chat/completions$", "/v1/models", bu, perl = TRUE),
            sub("/chat/completions$",    "/models",    bu, perl = TRUE),
            paste0(bu, "/v1/models"),
            paste0(bu, "/models")
        ))
    }

    builtins <- list(
        lmstudio = list(key = "lmstudio", name = "LM Studio",
                        base_url = lmstudio_base, probes = mk_probes(lmstudio_base)),
        ollama   = list(key = "ollama",   name = "Ollama",
                        base_url = ollama_base,   probes = mk_probes(ollama_base)),
        localai  = list(key = "localai",  name = "LocalAI",
                        base_url = localai_base,  probes = mk_probes(localai_base))  # <- fixed "="
    )

    extras <- getOption("gpt.extra_local_backends", NULL)
    if (is.list(extras) && length(extras)) {
        for (nm in names(extras)) builtins[[nm]] <- extras[[nm]]
    }
    builtins
}

#' Detect a single running local backend (compat wrapper)
#' @keywords internal
#' @param timeout seconds
#' @param backend optional backend key to force (e.g., "lmstudio","ollama","localai")
#' @param require_model optional model id to prefer when choosing among multiple
#' @return NULL or list(backend, name, base_url)
.detect_local_backend <- function(timeout = getOption("gpt.timeout", 5),
                                  backend = NULL,
                                  require_model = NULL) {
    df <- .detect_local_backends(timeout = timeout)
    if (!nrow(df)) return(NULL)

    if (!is.null(backend) && nzchar(backend)) {
        row <- df[df$backend == backend, , drop = FALSE]
        if (!nrow(row)) return(NULL)
    } else {
        row <- .pick_local_backend(df, require_model = require_model)
    }

    list(
        backend  = row$backend[[1]],
        name     = row$name[[1]],
        base_url = row$base_url[[1]]
        # (Optional) you can add models = row$models[[1]] if useful to callers
    )
}

# Return all running local backends with model lists (data.frame with a list-col)
.detect_local_backends <- function(timeout = getOption("gpt.timeout", 5)) {
    cand <- .local_candidates()  # uses your configured endpoints
    out  <- list()

    test_ok <- function(url) {
        ok <- tryCatch({
            resp <- httr2::req_perform(
                httr2::request(url) |> httr2::req_timeout(timeout)
            )
            httr2::resp_status(resp) < 400
        }, error = function(e) FALSE)
        ok
    }

    for (key in names(cand)) {
        probes <- cand[[key]]$probes %||% c(cand[[key]]$probe)  # old/new field compat
        # try each probe pattern; take the first that works
        good <- NULL
        for (pr in probes) if (isTRUE(test_ok(pr))) { good <- pr; break }
        if (!is.null(good)) {
            models <- .probe_models(good, timeout = timeout)
            out[[length(out) + 1L]] <- list(
                backend  = cand[[key]]$key %||% key,
                name     = cand[[key]]$name,
                base_url = cand[[key]]$base_url,
                models   = models
            )
        }
    }

    if (!length(out)) {
        return(data.frame(
            backend = character(0),
            name = character(0),
            base_url = character(0),
            models = I(list()),
            stringsAsFactors = FALSE
        ))
    }

    data.frame(
        backend  = vapply(out, `[[`, "", "backend"),
        name     = vapply(out, `[[`, "", "name"),
        base_url = vapply(out, `[[`, "", "base_url"),
        models   = I(lapply(out, `[[`, "models")),
        stringsAsFactors = FALSE
    )
}

# Pick a backend from those running, by preference and required model (if any)
.pick_local_backend <- function(available_df,
                                prefer = getOption("gpt.local_prefer",
                                                   c("lmstudio", "ollama", "localai")),
                                require_model = NULL) {
    if (!nrow(available_df)) return(NULL)

    ord <- order(match(available_df$backend, prefer, nomatch = length(prefer) + 1L))
    df  <- available_df[ord, , drop = FALSE]

    if (!is.null(require_model) && nzchar(require_model)) {
        req <- tolower(require_model)
        has_model <- vapply(df$models, function(vec) {
            is.character(vec) && any(tolower(vec) == req)
        }, FALSE)
        if (any(has_model)) {
            return(df[which(has_model)[1L], , drop = FALSE])
        }
    }

    df[1L, , drop = FALSE]
}


# Probe /v1/models and return a character vector of model ids
.probe_models <- function(models_url, timeout = getOption("gpt.timeout", 5)) {
    if (!requireNamespace("httr2", quietly = TRUE)) return(character(0))
    resp <- tryCatch(
        httr2::req_perform(
            httr2::request(models_url) |>
                httr2::req_timeout(timeout)
        ),
        error = function(e) NULL
    )
    if (is.null(resp) || httr2::resp_status(resp) >= 400) return(character(0))

    txt <- httr2::resp_body_string(resp)
    j <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(j)) return(character(0))

    get_ids <- function(x) {
        if (!length(x)) return(character(0))
        vapply(x, function(m) {
            id <- tryCatch(m$id, error = function(e) NULL)
            if (is.character(id) && length(id) == 1L && nzchar(id)) id else ""
        }, "", USE.NAMES = FALSE)
    }

    if (!is.null(j$data)   && is.list(j$data))   return(get_ids(j$data)[nzchar(get_ids(j$data))])
    if (!is.null(j$models) && is.list(j$models)) return(get_ids(j$models)[nzchar(get_ids(j$models))])
    if (is.list(j) && all(vapply(j, function(m) is.list(m) && !is.null(m$id), TRUE))) {
        ids <- get_ids(j); return(ids[nzchar(ids)])
    }
    if (is.character(j)) return(j[nzchar(j)])

    character(0)
}


# Public helper -------------------------------------------------------------

#' List configured local backend endpoints
#'
#' Returns the configured local backend endpoints from package defaults or
#' user options, without checking whether they are actually running.
#'
#' To change defaults globally, set them via `options()` in your `.Rprofile`
#' or session, e.g.:
#' \preformatted{
#' options(
#'   gpt.lmstudio_base_url = "http://localhost:1234/v1/chat/completions",
#'   gpt.ollama_base_url   = "http://localhost:11434/v1/chat/completions"
#' )
#' }
#'
#' Add custom backends:
#' \preformatted{
#' options(
#'   gpt.extra_local_backends = list(
#'     myserv = list(
#'       key = "myserv", name = "MyServer",
#'       base_url = "http://127.0.0.1:9999/v1/chat/completions",
#'       probe    = "http://127.0.0.1:9999/v1/models"
#'     )
#'   )
#' )
#' }
#'
#' @return A data frame with columns: backend, name, base_url, probe.
#' @examples
#' list_local_backends()
#' @export
#' @return A data frame with columns: backend, name, base_url, probe.
#' @export
list_local_backends <- function() {
    x <- .local_candidates()
    probe1 <- vapply(x, function(e) {
        p <- e$probes %||% e$probe
        if (is.null(p)) "" else as.character(p[[1]])
    }, "")
    data.frame(
        backend  = vapply(x, function(e) e$key %||% "", ""),
        name     = vapply(x, `[[`, "", "name"),
        base_url = vapply(x, `[[`, "", "base_url"),
        probe    = probe1,
        row.names = NULL, stringsAsFactors = FALSE
    )
}




#' Perform a Chat Completions request to a local OpenAI-compatible server (httr2)
#' @keywords internal
#' @param payload list as created by openai_compose_payload()
#' @param base_url e.g. http://127.0.0.1:1234/v1/chat/completions
#' @param timeout seconds
#' @return list(body, resp)
#' Perform a Chat Completions request to a local OpenAI-compatible server (httr2)
#' @keywords internal
request_local <- function(payload,
                          base_url = NULL,
                          timeout = getOption("gpt.timeout", 30)) {
    if (is.null(base_url) || !nzchar(base_url)) {
        stop("`base_url` must be provided for local requests.", call. = FALSE)
    }
    ua <- sprintf("gptr/%s (+local)", tryCatch(as.character(utils::packageVersion("gptr")), error = function(e) "0.0.0"))

    flatten_message_content <- function(msgs) {
        lapply(msgs, function(m) {
            if (is.list(m$content)) {
                types <- vapply(m$content, function(p) if (is.list(p)) (p$type %||% "text") else "text", "")
                if (length(types) && all(types == "text")) {
                    texts <- vapply(
                        m$content,
                        function(p) if (is.list(p) && identical(p$type, "text")) p$text else "",
                        "", USE.NAMES = FALSE
                    )
                    m$content <- paste(texts[nzchar(texts)], collapse = "\n\n")
                }
            }
            m
        })
    }
    if (is.list(payload$messages)) {
        payload$messages <- flatten_message_content(payload$messages)
    }

    req <- httr2::request(base_url) |>
        httr2::req_user_agent(ua) |>
        httr2::req_timeout(seconds = timeout) |>
        httr2::req_body_json(payload, auto_unbox = TRUE) |>
        httr2::req_retry(
            max_tries = 4,
            backoff = function(attempt) runif(1, 0.3, 1.2),
            is_transient = function(resp) {
                sc <- try(httr2::resp_status(resp), silent = TRUE)
                if (inherits(sc, "try-error")) return(TRUE)
                sc %in% c(408, 409, 429, 500, 502, 503, 504)
            }
        )

    resp <- httr2::req_perform(req)
    try(httr2::resp_check_status(resp), silent = TRUE)

    body_txt <- httr2::resp_body_string(resp)
    body <- tryCatch(jsonlite::fromJSON(body_txt, simplifyVector = FALSE), error = function(e) NULL)

    if (httr2::resp_status(resp) >= 400) {
        msg <- "Local provider request failed"
        if (is.list(body) && !is.null(body$error)) {
            em <- body$error$message
            if (is.character(em) && nzchar(em)) msg <- em
        }
        stop(msg, call. = FALSE)
    }

    list(body = body, resp = resp)
}

