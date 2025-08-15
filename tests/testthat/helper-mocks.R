.na_vals <- c("NA", "null", "", "[]", "{}", "None")

mock_gpt <- function(prompt, ...) {
    if (grepl("MOCK_RET=NA_LIKE",    prompt)) return("NA")
    if (grepl("MOCK_RET=NOT_JSON",   prompt)) return("totally not json")
    if (grepl("MOCK_RET=BASIC_JSON", prompt)) return('{"age": "64", "smoker": "1"}')
    if (grepl("MOCK_RET=BASIC_JSON_AB", prompt)) return('{"a": "64", "b": "1"}')
    if (grepl("MOCK_RET=BROKEN_JSON", prompt)) return('{"age": 64, "smoker": true, }')
    if (grepl("MOCK_RET=EXTRA_KEYS",  prompt)) return('{"age": "64", "smokre": "1", "extra": "zzz"}')
    if (grepl("MOCK_RET=ALLOWED_FAIL", prompt)) return('{"severity":"extreme"}')
    if (grepl("MOCK_RET=LIST_VAL",    prompt)) return('{"codes":[1,2,3]}')
    '{"ok": true}'
}

# Force the mock for the whole test run (no HTTP)
options(gptr.gpt_fun = mock_gpt)
testthat::teardown(options(gptr.gpt_fun = NULL))

mock_prompt <- function(text, keys, tag = "BASIC_JSON") {
    paste0("Text: ", text, "\nMOCK_RET=", tag)
}

specs_from_keys <- function(keys) {
    if (is.null(keys)) return(NULL)
    purrr::map(keys, parse_key_spec)
}
