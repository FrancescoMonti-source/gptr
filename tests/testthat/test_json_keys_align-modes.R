test_that("json_keys_align supports lev_ratio and lev modes", {
    exp <- c("smoker", "smoker_status")

    # single typo: "smokre" -> "smoker"
    x1 <- list(smokre = 1)

    r_ratio <- json_keys_align(x1, exp,
                               auto_correct    = TRUE,
                               fuzzy_model     = "lev_ratio",
                               fuzzy_threshold = 0.3)
    expect_false(is.na(r_ratio$smoker))
    expect_true(is.na(r_ratio$smoker_status))

    r_abs <- json_keys_align(x1, exp,
                             auto_correct    = TRUE,
                             fuzzy_model     = "lev",
                             fuzzy_threshold = 2)
    expect_false(is.na(r_abs$smoker))
    expect_true(is.na(r_abs$smoker_status))

    # ambiguous short key: "smok" should remain NA (not unique best)
    x2 <- list(smok = 1)
    r2 <- json_keys_align(x2, exp,
                          auto_correct    = TRUE,
                          fuzzy_model     = "lev_ratio",
                          fuzzy_threshold = 0.3)
    expect_true(is.na(r2$smoker))
    expect_true(is.na(r2$smoker_status))
})
