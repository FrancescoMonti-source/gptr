# Run from package root:
# Rscript dev/tools/rename_wrappers.R
# Add dry_run = TRUE to preview changes.

dry_run <- FALSE

# 1) Collect files (R/ and tests/)
files <- c(
    list.files("R",     pattern = "\\.[Rr]$", full.names = TRUE, recursive = FALSE),
    list.files("tests", pattern = "\\.[Rr]$", full.names = TRUE, recursive = TRUE)
)
# Don't rewrite the wrappers file itself (you'll edit that manually)
files <- setdiff(files, c("R/http_wrappers.R"))

# 2) Patterns: capture the leading dot and keep it via \\1 in replacement
#    Order matters: the *_req variant first to avoid partial matches.
renames <- list(
    list(pattern = "(\\.)http_body_json_req\\b", replacement = "\\1http_req_body_json"),
    list(pattern = "(\\.)http_body_json\\b",     replacement = "\\1http_resp_body_json"),
    list(pattern = "(\\.)http_headers\\b",       replacement = "\\1http_req_headers"),
    list(pattern = "(\\.)http_timeout\\b",       replacement = "\\1http_req_timeout"),
    list(pattern = "(\\.)http_retry\\b",         replacement = "\\1http_req_retry"),
    list(pattern = "(\\.)http_perform\\b",       replacement = "\\1http_req_perform"),
    list(pattern = "(\\.)http_status\\b",        replacement = "\\1http_resp_status")
)

# helper: count matches in a character vector
count_matches <- function(x, pat) {
    sum(sapply(gregexpr(pat, x, perl = TRUE), function(m) sum(m > 0)))
}

changed <- character()
report  <- list()

for (f in files) {
    txt  <- readLines(f, warn = FALSE)
    orig <- txt

    per_file_hits <- integer(length(renames))

    for (i in seq_along(renames)) {
        pat <- renames[[i]]$pattern
        repl <- renames[[i]]$replacement
        per_file_hits[i] <- count_matches(txt, pat)
        if (per_file_hits[i] > 0L) {
            txt <- gsub(pat, repl, txt, perl = TRUE)
        }
    }

    if (!identical(txt, orig)) {
        changed <- c(changed, f)
        report[[f]] <- setNames(per_file_hits, vapply(renames, `[[`, "", "pattern"))
        if (!dry_run) writeLines(txt, f)
    }
}

cat("Updated files:\n",
    if (length(changed)) paste0(" - ", paste(changed, collapse = "\n - "), "\n") else "(none)\n",
    sep = "")

if (length(report)) {
    cat("\nPer-file hit counts (pattern -> hits):\n")
    for (f in names(report)) {
        cat("\n", f, ":\n", sep = "")
        for (nm in names(report[[f]])) {
            cat("  ", nm, " -> ", report[[f]][[nm]], "\n", sep = "")
        }
    }
}
