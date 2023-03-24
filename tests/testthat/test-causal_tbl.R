test_that("causal_tbl creation", {
    x = causal_tbl(milk_first = c(0, 1, 0, 1, 1, 0, 0, 1),
                   guess = c(0, 1, 0, 1, 1, 0, 0, 1))

    expect_s3_class(x, c("causal_tbl", "tbl_df", "tbl", "data.frame"), exact=TRUE)
    # addl attribute checks
    expect_true(is.null(attr(x, 'causal_cols')$outcome))
    expect_true(is.null(attr(x, 'causal_cols')$treatment))
    expect_true(length(names(attr(x, 'causal_cols'))) == 2L)
})

test_that("causal_tbl attribute", {
    x <- causal_tbl(milk_first = c(0, 1, 0, 1, 1, 0, 0, 1),
                   guess = c(0, 1, 0, 1, 1, 0, 0, 1),
                   correct = c(0, 1, 0, 1, 1, 0, 0, 1) == c(0, 1, 0, 1, 1, 0, 0, 1),
                   .outcome = 'correct')

    expect_s3_class(x, c("causal_tbl", "tbl_df", "tbl", "data.frame"), exact=TRUE)

    expect_true(attr(x, 'causal_cols')$outcome == 'correct')
    expect_true(is.null(attr(x, 'causal_cols')$treatment))

    x <- causal_tbl(milk_first = c(0, 1, 0, 1, 1, 0, 0, 1),
                    guess = c(0, 1, 0, 1, 1, 0, 0, 1),
                    correct = c(0, 1, 0, 1, 1, 0, 0, 1) == c(0, 1, 0, 1, 1, 0, 0, 1),
                    .outcome = 'correct', .treatment = 'milk_first')

    expect_s3_class(x, c("causal_tbl", "tbl_df", "tbl", "data.frame"), exact=TRUE)

    expect_true(attr(x, 'causal_cols')$outcome == 'correct')
    expect_true(attr(x, 'causal_cols')$treatment == 'milk_first')
})
