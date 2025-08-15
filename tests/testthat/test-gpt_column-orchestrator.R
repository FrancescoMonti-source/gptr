template <- paste0(
    "You are a data extraction assistant.\n",
    "Given the following text, extract the specified fields and return them as a single-line JSON object.\n",
    "Schema: {json_format}\n\n",
    "Text:\n\"{text}\"\n\n",
    "Respond with RAW JSON only (no backticks, no markdown, no explanations). ",
    "Always include ALL schema keys. If unknown, use null."
)

test_that("gpt_column happy path (schema, coerce, schema final types)", {
    df <- tibble::tibble(
        id   = 1:2,
        note = c(
            "The patient is a 64-year-old male with hypertension. He currently smokes about one pack of cigarettes per day.",
            "The patient is a 32-year-old female with seasonal allergies. She has never smoked and denies any tobacco use."
        )
    )
    keys = list(age = "numeric", smoker = "integer")

    res <- gpt_column(
        data         = df,
        col          = note,
        prompt       = template,
        keys         = list(age = "numeric", smoker = "integer"),
        provider     = "local",
        temperature  = 0,
        return_debug = TRUE,
        coerce_types = TRUE,
        final_types  = "schema"
    )

    # Debug: print raw LLM output
    print(res$.raw_output)

    expect_s3_class(res, "tbl_df")
    expect_type(res$age, "integer")
    expect_type(res$smoker, "integer")
    expect_true(all(res$.invalid_rows == 0))
})
