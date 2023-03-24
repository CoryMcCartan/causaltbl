test_that("getting and setting treatment works", {
    x = causal_tbl(milk_first = c(0, 1, 0, 1, 1, 0, 0, 1),
                   guess = c(0, 1, 0, 1, 1, 0, 0, 1))

    x_trt = set_treatment(x, milk_first)
    expect_equal(get_treatment(x_trt), "milk_first")

    x = data.frame(milk_first = c(0, 1, 0, 1, 1, 0, 0, 1),
                   guess = c(0, 1, 0, 1, 1, 0, 0, 1))
    x_trt = set_treatment(x, milk_first)
    expect_no_error(validate_causal_tbl(x_trt))
    expect_equal(get_treatment(x_trt), "milk_first")
})

test_that("getting and setting outcome works", {
    x = causal_tbl(milk_first = c(0, 1, 0, 1, 1, 0, 0, 1),
                   guess = c(0, 1, 0, 1, 1, 0, 0, 1))

    x_out = set_outcome(x, guess)
    expect_equal(get_outcome(x_out), "guess")

    x_out_trt = set_treatment(x_out, milk_first)
    expect_equal(get_outcome(x_out_trt), "guess")
    expect_equal(get_treatment(x_out_trt), "milk_first")

    x = data.frame(milk_first = c(0, 1, 0, 1, 1, 0, 0, 1),
                   guess = c(0, 1, 0, 1, 1, 0, 0, 1))
    x_out = set_outcome(x, guess)
    expect_no_error(validate_causal_tbl(x_out))
    expect_equal(get_outcome(x_out), "guess")
})
