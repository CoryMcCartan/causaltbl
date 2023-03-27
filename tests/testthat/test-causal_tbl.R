test_that("causal_tbl creation", {
    x = causal_tbl(milk_first = c(0, 1, 0, 1, 1, 0, 0, 1),
                   guess = c(0, 1, 0, 1, 1, 0, 0, 1))

    expect_s3_class(x, c("causal_tbl", "tbl_df", "tbl", "data.frame"), exact=TRUE)
    # addl attribute checks
    expect_type(attr(x, "causal_cols"), "list")
    expect_type(causal_cols(x), "list")
    expect_null(get_outcome(x))
    expect_null(get_treatment(x))

    expect_s3_class(causal_tbl(), "causal_tbl") # test empty
    x = as_causal_tbl(list(y=1))
    expect_s3_class(x, c("causal_tbl", "tbl_df", "tbl", "data.frame"), exact=TRUE)
})

test_that("causal_tbl validation", {
    expect_error(validate_causal_tbl(data.frame()), "must have a")

    x = causal_tbl(y="5")
    causal_cols(x) <- list(outcomes="y", treatments=NULL)
    expect_error(validate_causal_tbl(x), "must be numeric")
    causal_cols(x) <- list(outcomes=5L, treatments=NULL)
    expect_error(validate_causal_tbl(x), "as a string")

    x = causal_tbl(t="5")
    causal_cols(x) <- list(outcomes=NULL, treatments="y")
    expect_error(validate_causal_tbl(x), "must be numeric")
    causal_cols(x) <- list(outcomes=NULL, treatments=5L)
    expect_error(validate_causal_tbl(x), "as a string")

    x = data.frame()
    expect_no_error(assert_df(x))
    expect_error(assert_df(5L), "data frame")
    expect_error(assert_causal_tbl(x), "causal_tbl")

    x <- causal_tbl(milk_first = c(0, 1, 0, 1, 1, 0, 0, 1),
                    guess = c(0, 1, 0, 1, 1, 0, 0, 1),
                    .outcome = "guess")
    y <- data.frame(x)
    expect_identical(reconstruct.causal_tbl(y, x), x)
    expect_error(reconstruct.causal_tbl(5L, x), "data frame")
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
                    .outcome = guess,
                    .treatment = milk_first)

    expect_s3_class(x, "causal_tbl")

    expect_equal(get_outcome(x), "guess")
    expect_equal(get_treatment(x), c(guess="milk_first"))
})

test_that("causal_tbl slicing and renaming", {
    x <- causal_tbl(milk_first = c(0, 1, 0, 1, 1, 0, 0, 1),
                    guess = c(0, 1, 0, 1, 1, 0, 0, 1),
                    .outcome = guess,
                    .treatment = milk_first)

    names(x) <- c("trt", "y")
    expect_equal(get_treatment(x), c(y="trt"))
    expect_equal(get_outcome(x), "y")

    x_df = as.data.frame(x)
    expect_equal(x[1:2, ], causal_tbl(x_df[1:2, ], .outcome=y, .treatment=trt))

    expect_identical(x, x[])

    y <- x[1:4, ]
    expect_equal(get_treatment(y), c(y="trt"))
    expect_equal(get_outcome(y), "y")

    y <- x[2]
    expect_null(get_treatment(y))
    expect_equal(get_outcome(y), "y")
    y <- x[, 1]
    expect_null(get_outcome(y))
    expect_equal(get_treatment(y), "trt") # no y= !

    x <- causal_tbl(milk_first = c(0, 1, 0, 1, 1, 0, 0, 1),
                    guess = c(0, 1, 0, 1, 1, 0, 0, 1),
                    .outcome = guess)
    names(x) <- c("trt", "y")
    x <- x[2]
    expect_null(get_treatment(x))
    expect_equal(get_outcome(x), "y")
})

test_that("causal_tbl printing", {
    skip_on_cran()

    x <- causal_tbl(milk_first = c(0, 1, 0, 1, 1, 0, 0, 1),
                    guess = c(0, 1, 0, 1, 1, 0, 0, 1),
                    .outcome = guess,
                    .treatment = milk_first)

    expect_snapshot(print(x))
})

test_that("causal_tbl + dplyr", {
    x <- causal_tbl(milk_first = c(0, 1, 0, 1, 1, 0, 0, 1),
                    guess = c(0, 1, 0, 1, 1, 0, 0, 1),
                    .outcome = guess)

    expect_s3_class(dplyr::group_by(x, guess), "causal_tbl")
    expect_equal(get_outcome(dplyr::group_by(x, guess)), "guess")
    expect_s3_class(dplyr::ungroup(x), "causal_tbl")
    expect_s3_class(dplyr::rowwise(x), "causal_tbl")

    y <- data.frame(milk_first=c(0, 0, 1, 1),
                    guess=c(0, 1, 0, 1),
                    correct=c(1, 0, 0, 1))
    out <- dplyr::left_join(x, y, by=c("milk_first", "guess"))
    expect_s3_class(out, "causal_tbl")
    expect_equal(get_outcome(out), "guess")
})
