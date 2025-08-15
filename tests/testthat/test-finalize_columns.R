test_that("finalize_columns schema mode uses declared types", {
    df <- tibble::tibble(age = c("64","65"), smoker = c("1","0"), sev = c("mild","severe"))
    ks <- specs_from_keys(list(age="integer", smoker="integer", sev=c("mild","moderate","severe")))

    out <- finalize_columns(df, expected_keys = names(df), key_specs = ks, mode = "schema")
    expect_type(out$age,   "integer")
    expect_type(out$smoker,"integer")
    expect_type(out$sev,   "character")
})

test_that("finalize_columns infer mode follows logical -> integer -> numeric -> character order", {
    df <- tibble::tibble(
        a = c("TRUE","FALSE",NA),    # logical
        b = c("1","0","1"),          # integer
        c = c("3.14","2.72","NA"),   # numeric
        d = c("mild","severe","NA")  # character
    )
    out <- finalize_columns(df, expected_keys = names(df), key_specs = NULL, mode = "infer")
    expect_type(out$a, "logical")
    expect_type(out$b, "integer")
    expect_type(out$c, "double")
    expect_type(out$d, "character")
})

test_that("finalize_columns as_is leaves columns untouched", {
    df <- tibble::tibble(a = I(list(1,2)), b = c("x","y"))
    out <- finalize_columns(df, expected_keys = names(df), key_specs = NULL, mode = "as_is")
    expect_s3_class(out$a, "AsIs")   # still a list-col wrapped by I()
    expect_identical(out$b, df$b)
})
