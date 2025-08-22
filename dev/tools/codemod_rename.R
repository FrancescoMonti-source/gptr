#!/usr/bin/env Rscript
# dev/tools/codemod_rename.R
# Project-wide regex-based renames with preview, backups, and per-file hit counts.
# Run: Rscript dev/tools/codemod_rename.R --apply
# Preview (no write): Rscript dev/tools/codemod_rename.R

suppressWarnings(suppressMessages({
    # no heavy deps; base R only
}))

args <- commandArgs(trailingOnly = TRUE)
apply_changes <- any(args %in% c("--apply", "-y"))

# --------- CONFIGURE HERE (or pass via a wrapper) ---------
paths_include <- c("R", "tests")                 # where to search
paths_exclude <- c("R/http_wrappers.R",          # don’t rewrite the source of truth
                   "NAMESPACE", "man", "inst",
                   "DESCRIPTION", "dev/structure",
                   ".git", ".github")            # common ignores

# Order matters: more specific first to avoid partial matches.
# TIP: capture the leading dot `(\.)` and keep it with `\\1` in replacement
renames <- list(
    list(pattern = "(\\.)http_body_json_req\\b", replacement = "\\1http_req_body_json"),
    list(pattern = "(\\.)http_body_json\\b",     replacement = "\\1http_resp_body_json"),
    list(pattern = "(\\.)http_headers\\b",       replacement = "\\1http_req_headers"),
    list(pattern = "(\\.)http_timeout\\b",       replacement = "\\1http_req_timeout"),
    list(pattern = "(\\.)http_retry\\b",         replacement = "\\1http_req_retry"),
    list(pattern = "(\\.)http_perform\\b",       replacement = "\\1http_req_perform"),
    list(pattern = "(\\.)http_status\\b",        replacement = "\\1http_resp_status")
)
# ----------------------------------------------------------

is_excluded <- function(path, excludes) {
    any(vapply(excludes, function(ex) grepl(paste0("^", gsub("\\.", "\\\\.", ex)), path), logical(1)))
}

collect_files <- function(roots) {
    files <- unlist(lapply(roots, function(dir) {
        if (!dir.exists(dir)) return(character())
        list.files(dir, pattern = "\\.[Rr]$", full.names = TRUE, recursive = TRUE)
    }))
    files[!vapply(files, is_excluded, logical(1), excludes = paths_exclude)]
}

count_matches <- function(x, pat) {
    sum(sapply(gregexpr(pat, x, perl = TRUE), function(m) sum(m > 0)))
}

apply_renames_to_text <- function(txt, renames) {
    hits <- integer(length(renames))
    for (i in seq_along(renames)) {
        pat  <- renames[[i]]$pattern
        repl <- renames[[i]]$replacement
        hits[i] <- count_matches(txt, pat)
        if (hits[i] > 0L) {
            txt <- gsub(pat, repl, txt, perl = TRUE)
        }
    }
    list(text = txt, hits = hits)
}

files <- collect_files(paths_include)
if (!length(files)) {
    cat("No files found under: ", paste(paths_include, collapse = ", "), "\n", sep = "")
    quit(save = "no")
}

total_hits <- integer(length(renames))
changed <- list()

for (f in files) {
    txt  <- readLines(f, warn = FALSE, encoding = "UTF-8")
    res  <- apply_renames_to_text(txt, renames)
    if (!identical(res$text, txt)) {
        changed[[f]] <- res$hits
        total_hits <- total_hits + res$hits
        if (apply_changes) {
            # backup once per run (simple .bak overwrite)
            bak <- paste0(f, ".bak")
            if (!file.exists(bak)) try(writeLines(txt, bak), silent = TRUE)
            writeLines(res$text, f, useBytes = TRUE)
        }
    }
}

# ------- Report -------
if (length(changed)) {
    cat(if (apply_changes) "Updated files:\n" else "Would update files (preview):\n")
    for (f in names(changed)) {
        cat(" - ", f, "\n", sep = "")
        per <- changed[[f]]
        for (i in seq_along(renames)) {
            if (per[i] > 0L) {
                cat(sprintf("     %s  -> %d\n", renames[[i]]$pattern, per[i]))
            }
        }
    }
} else {
    cat(if (apply_changes) "No changes written.\n" else "No changes would be made.\n")
}

cat("\nTotal hits:\n")
for (i in seq_along(renames)) {
    cat(sprintf("  %s  -> %d\n", renames[[i]]$pattern, total_hits[i]))
}

if (!apply_changes) {
    cat("\nRun with --apply to write changes. Backups (*.bak) will be created once per file.\n")
}
