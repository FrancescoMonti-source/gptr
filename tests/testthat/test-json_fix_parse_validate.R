test_that("json_fix_parse_validate handles NA-like, not-json, and basic parse", {
    ks <- specs_from_keys(list(age = "integer", smoker = "integer"))

    # NA-like → ok + empty list
    out <- json_fix_parse_validate("NA", ks, .na_vals, verbose = FALSE)
    expect_true(out$ok)
    expect_true(is.list(out$value))
    expect_length(out$value, 0)

    # not JSON → ok=FALSE
    out <- json_fix_parse_validate("not a json", ks, .na_vals, verbose = FALSE)
    expect_false(out$ok)

    # valid JSON, no coercion
    out <- json_fix_parse_validate('{"age":"64","smoker":"1"}', ks, .na_vals, verbose = FALSE,
                                   coerce_types = FALSE)
    expect_true(out$ok)
    expect_identical(out$value$age, "64")       # still character
    expect_identical(out$value$smoker, "1")
})

test_that("json_fix_parse_validate repairs trailing comma and coerces/in_allowed", {
    ks <- specs_from_keys(list(age = "integer", smoker = "integer",
                               severity = c("mild","moderate","severe")))

    # trailing comma repaired
    broken <- '{"age": 64, "smoker": true, }'
    out <- json_fix_parse_validate(broken, ks, .na_vals, verbose = FALSE)
    expect_true(out$ok)
    expect_identical(out$value$age, 64L)          # coerced to integer
    expect_identical(out$value$smoker, 1L)      # coerced logical→TRUE

    # allowed set rejection -> NA
    bad <- '{"severity":"extreme"}'
    out <- json_fix_parse_validate(bad, ks, .na_vals, verbose = FALSE)
    expect_true(out$ok)
    expect_true(is.na(out$value$severity))
})

test_that("json_fix_parse_validate coerce_when predicate works per key/value", {
    ks <- specs_from_keys(list(a = "integer", b = "integer"))

    # Coerce only key 'a', not 'b'
    pred <- function(key, value, spec, i) key == "a"

    out <- json_fix_parse_validate('{"a":"5","b":"6"}', ks, .na_vals,
                                   coerce_types = FALSE, coerce_when = pred)
    expect_identical(out$value$a, 5L)  # coerced
    expect_identical(out$value$b, "6") # left as character
})
