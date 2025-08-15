template <- paste0(
    "You are a data extraction assistant.\n",
    "Given the following text, extract the specified fields and return them as a single‑line JSON object.\n",
    "Schema: {json_format}\n\n",
    "Text:\n\"{text}\"\n\n",
    "Return only a valid JSON object with these keys. No other output."
)

test_that("gpt_column happy path (schema, coerce, schema final types)", {
    df   <- tibble::tibble(id = 1:2, note = c("foo", "bar"))
    keys <- list(age = "integer", smoker = "integer")

    res <- gpt_column(
        data         = df,
        col          = note,
        prompt       = template,
        keys         = keys,
        provider     = "local",
        temperature  = 0,
        return_debug = TRUE,
        coerce_types = TRUE,
        final_types  = "schema"
    )

    expect_s3_class(res, "tbl_df")
    expect_type(res$age, "integer")
    expect_type(res$smoker, "integer")
    expect_true(all(res$.invalid_rows == 0))
})

test_that("gpt_column handles broken JSON via tidy_json repair", {
    df   <- tibble::tibble(id = 1, note = "x")
    keys <- list(age = "integer", smoker = "logical")

    res <- gpt_column(
        data         = df,
        col          = note,
        prompt       = template,
        keys         = keys,
        provider     = "local",
        temperature  = 0,
        return_debug = TRUE,
        final_types  = "schema"
    )

    expect_s3_class(res, "tbl_df")
    expect_true(is.integer(res$age))
    expect_true(is.logical(res$smoker))
    # Any row that can be parsed (even after a trailing‑comma fix) should not be marked invalid
    expect_equal(attr(res, "invalid_rows"), integer(0))
})


