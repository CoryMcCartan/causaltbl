test_that("causal_tbl creation", {
    x = causal_tbl(milk_first = c(0, 1, 0, 1, 1, 0, 0, 1),
                   guess = c(0, 1, 0, 1, 1, 0, 0, 1))

    expect_s3_class(x, c("causal_tbl", "tbl_df", "tbl", "data.frame"), exact=TRUE)
    # addl attribute checks
    expect_type(attr(x, "causal_cols"), "list")
    expect_type(causal_cols(x), "list")
    expect_null(get_outcome(x))
    expect_null(get_treatment(x))
})

test_that("causal_tbl attributes", {
    x <- causal_tbl(milk_first = c(0, 1, 0, 1, 1, 0, 0, 1),
                   guess = c(0, 1, 0, 1, 1, 0, 0, 1),
                   .outcome = "guess")

    expect_s3_class(x, "causal_tbl")

    expect_equal(get_outcome(x), "guess")
    expect_null(get_treatment(x))

    x <- causal_tbl(milk_first = c(0, 1, 0, 1, 1, 0, 0, 1),
                    guess = c(0, 1, 0, 1, 1, 0, 0, 1),
                    .treatment = "milk_first")

    expect_s3_class(x, "causal_tbl")
    expect_equal(get_treatment(x), "milk_first")
    expect_null(get_outcome(x))

    x <- causal_tbl(milk_first = c(0, 1, 0, 1, 1, 0, 0, 1),
                    guess = c(0, 1, 0, 1, 1, 0, 0, 1),
                    .outcome = "guess",
                    .treatment = "milk_first")

    expect_s3_class(x, "causal_tbl")

    expect_equal(get_outcome(x), "guess")
    expect_equal(get_treatment(x), "milk_first")
})
