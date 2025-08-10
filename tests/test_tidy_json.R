# tests/test_tidy_json.R
# Sanity & regression tests for tidy_json()

# run source("tests/test_tidy_json.R")

# 1) load helpers -----------------------------------------------------------
source("utils.R")   # make sure this file defines tidy_json() and is_na_like()

# 2) messy examples ---------------------------------------------------------
examples <- list(
    good_json       = '{ "isolement_bin": 1, "comment": "ok" }',
    fenced          = "Voici le résultat :\n```json\n{ \"isolement_bin\": 1 }\n```\nMerci !",
    bare_na         = '{ "isolement_bin": NA, "comment": "ok" }',
    trailing_comma  = '{ "isolement_bin": 1, "comment": "ok", }',
    bare_key        = '{ isolement_bin: 1, "comment": "ok" }',
    single_quotes   = "{ 'isolement_bin': '1', 'comment': 'ok' }",
    py_tokens       = '{ "flag": True, "other": False, "maybe": None }'
)

# 3) expected logs (exact order) -------------------------------------------
expected_logs <- list(
    good_json       = c("cleaned"),
    fenced          = c("cleaned"),
    bare_na         = c("cleaned", "quoted bare NA"),
    trailing_comma  = c("cleaned", "removed trailing comma"),
    bare_key        = c("cleaned", "quoted bare keys"),
    single_quotes   = c("cleaned", "single quotes -> double quotes"),
    py_tokens       = c("cleaned")
)

# 4) runner ----------------------------------------------------------------
pass_count <- 0L
fail_msgs  <- character(0)

for (nm in names(examples)) {
    out <- tidy_json(examples[[nm]], aggressive = TRUE)

    # check logs
    ok_log <- identical(out$log, expected_logs[[nm]])
    if (!ok_log) {
        fail_msgs <- c(
            fail_msgs,
            sprintf("[LOG] %s\n  expected: %s\n  got:      %s",
                    nm,
                    paste(expected_logs[[nm]], collapse = ", "),
                    paste(out$log,           collapse = ", "))
        )
    }

    # check JSON is actually parseable
    parsed <- tryCatch(jsonlite::fromJSON(out$txt, simplifyVector = TRUE), error = identity)
    ok_parse <- !inherits(parsed, "error")
    if (!ok_parse) {
        fail_msgs <- c(
            fail_msgs,
            sprintf("[PARSE] %s\n  fromJSON() error: %s", nm, conditionMessage(parsed))
        )
    }

    if (ok_log && ok_parse) pass_count <- pass_count + 1L
}

# 5) summary ----------------------------------------------------------------
cat(sprintf("\n✅ tidy_json tests: %d/%d passed\n", pass_count, length(examples)))
if (length(fail_msgs)) {
    cat("\n❌ Failures:\n")
    cat(paste0(" - ", fail_msgs, collapse = "\n"), "\n")
    stop("Some tidy_json tests failed (see messages above).", call. = FALSE)
} else {
    cat("All tests green. ✨\n")
}
