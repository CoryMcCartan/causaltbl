test_that("causal_tbl creation", {
    x = causal_tbl(milk_first = c(0, 1, 0, 1, 1, 0, 0, 1),
                   guess = c(0, 1, 0, 1, 1, 0, 0, 1))

    expect_s3_class(x, c("causal_tbl", "tbl_df", "tbl", "data.frame"), exact=TRUE)
    # addl attribute checks
})
