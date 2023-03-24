test_that("getting and setting treatment works", {
    x = causal_tbl(milk_first = c(0, 1, 0, 1, 1, 0, 0, 1),
                   guess = c(0, 1, 0, 1, 1, 0, 0, 1))

    x_trt = set_treatment(x, milk_first)
    expect_equal(get_treatment(x_trt), "milk_first")
    # check non-tidy works too
    x_trt = set_treatment(x, "milk_first")
    expect_equal(get_treatment(x_trt), "milk_first")

    x = data.frame(milk_first = c(0, 1, 0, 1, 1, 0, 0, 1),
                   guess = c(0, 1, 0, 1, 1, 0, 0, 1))
    x_trt = set_treatment(x, milk_first)
    expect_no_error(validate_causal_tbl(x_trt))
    expect_equal(get_treatment(x_trt), "milk_first")

    expect_error(set_treatment(x, not_a_column), "doesn't exist")
    expect_error(set_treatment(x, NULL), "Must select")
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

    expect_error(set_outcome(x, not_a_column), "doesn't exist")
    expect_error(set_outcome(x, NULL), "Must select")
    x$guess = as.character(x$guess)
    expect_error(set_outcome(x, "guess"), "convert `guess`")
})


test_that("getting and setting panel data", {
    x <- data.frame(
        id = c("a", "a", "a", "a", "b", "b", "b", "b"),
        year = rep(2015:2018, 2),
        trt = c(0, 0, 0, 0, 0, 0, 1, 1),
        y = c(1, 3, 2, 3, 1, 3, 4, 5)
    )

    x_panel = set_panel(x, unit=id, time=year)
    expect_equal(get_panel(x_panel), list(unit = "id", time = "year"))
    expect_no_error(validate_causal_tbl(x_panel))

    expect_error(set_panel(x, unit=id), "for `time`")
    expect_error(set_panel(x, time=year), "for `unit`")
    expect_error(set_panel(x, unit=year, time=id), "convert `id`")
    expect_error(set_panel(x, unit=id, time=not_a_column), "doesn't exist")
})

