# Helper: minimal NA set used across tests
.na_vals <- c("NA", "null", "", "[]", "{}", "None")

# Mock: pretend LLM call. Switches behavior by special tokens in prompt.
# Returns RAW STRING (possibly JSON-looking) just like a real model.
mock_gpt <- function(prompt, ...) {
    if (grepl("MOCK_RET=NA_LIKE", prompt))  return("NA")
    if (grepl("MOCK_RET=NOT_JSON", prompt)) return("totally not json")
    if (grepl("MOCK_RET=BASIC_JSON", prompt)) return('{"a": "64", "b": "1"}')   # <- adjust for the selective-coercion test
    if (grepl("MOCK_RET=BROKEN_JSON", prompt)) return('{"age": 64, "smoker": true, }')
    if (grepl("MOCK_RET=EXTRA_KEYS", prompt)) return('{"age": "64", "smokre": "1", "extra": "zzz"}')
    if (grepl("MOCK_RET=ALLOWED_FAIL", prompt)) return('{"severity":"extreme"}')
    if (grepl("MOCK_RET=LIST_VAL", prompt)) return('{"codes":[1,2,3]}')
    '{"ok": true}'
}

# Use mock backend for all tests
set_gpt_backend(mock_gpt)
testthat::teardown(set_gpt_backend(NULL))


# Minimal prompt builder used in tests (injects a control tag)
mock_prompt <- function(text, keys, tag = "BASIC_JSON") {
    paste0("Text: ", text, "\nMOCK_RET=", tag)
}

# Quick spec helpers
specs_from_keys <- function(keys) {
    if (is.null(keys)) return(NULL)
    purrr::map(keys, parse_key_spec)
}
