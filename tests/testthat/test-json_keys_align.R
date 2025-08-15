test_that("json_keys_align exact pass-through and fill missing", {
    exp <- c("age","smoker")
    x <- list(age = 64L)
    res <- json_keys_align(x, expected_keys = exp, auto_correct = FALSE)
    expect_named(res, exp)
    expect_identical(res$age, 64L)
    expect_true(is.na(res$smoker))
})

test_that("json_keys_align fuzzy-corrects unique and leaves ambiguous as NA", {
    exp <- c("smoker","smoker_status")
    x1  <- list(smokre = 1)      # misspelled 'smoker'
    r1  <- json_keys_align(x1, exp,
                           auto_correct = TRUE,
                           fuzzy_model = "lev_ratio", fuzzy_threshold = 0.5)
    r1_abs <- json_keys_align(x1, exp,
                              auto_correct = TRUE,
                              fuzzy_model = "lev", fuzzy_threshold = 2)
    expect_false(is.na(r1_abs$smoker))
    expect_false(is.na(r1$smoker))
    expect_true(is.na(r1$smoker_status))

    # ambiguous: "smok" matches both -> remain NA
    x2 <- list(smok = 1)
    r2 <- json_keys_align(x2, exp, auto_correct = TRUE,fuzzy_model = "lev_ratio", fuzzy_threshold = 0.3)
    expect_true(is.na(r2$smoker))
    expect_true(is.na(r2$smoker_status))
})

test_that("json_keys_align keep_unexpected controls extra keys", {
    exp <- c("age","smoker")
    x   <- list(age = 64, smokre = 1, extra = "zzz")

    r_drop <- json_keys_align(x, exp, auto_correct = TRUE, keep_unexpected = FALSE)
    expect_named(r_drop, exp)

    r_keep <- json_keys_align(x, exp, auto_correct = TRUE, keep_unexpected = TRUE)
    expect_true(all(c(exp, "extra") %in% names(r_keep)))
})
