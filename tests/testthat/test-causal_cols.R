test_that("getting and setting treatment works", {
    x = causal_tbl(milk_first = c(0, 1, 0, 1, 1, 0, 0, 1),
                   guess = c(0, 1, 0, 1, 1, 0, 0, 1))

    x_trt = set_treatment(x, milk_first)
    expect_equal(get_treatment(x_trt), "milk_first")
    expect_true(has_treatment(x_trt))
    # check non-tidy works too
    x_trt = set_treatment(x, "milk_first")
    expect_equal(get_treatment(x_trt), "milk_first")

    x = data.frame(milk_first = c(0, 1, 0, 1, 1, 0, 0, 1),
                   guess = c(0, 1, 0, 1, 1, 0, 0, 1))
    x_trt = set_treatment(x, milk_first)
    expect_no_error(validate_causal_tbl(x_trt))
    expect_equal(get_treatment(x_trt), "milk_first")

    x_trt_out = set_outcome(x_trt, guess)
    expect_equal(get_treatment(x_trt_out), c(guess="milk_first"))

    expect_error(set_treatment(x, not_a_column), "doesn't exist")
    expect_error(set_treatment(x, c(milk_first, guess)), "Only one")
    expect_error(set_treatment(x, NULL), "Must select")
})

test_that("getting and setting outcome works", {
    x = causal_tbl(milk_first = c(0, 1, 0, 1, 1, 0, 0, 1),
                   guess = c(0, 1, 0, 1, 1, 0, 0, 1))

    x_out = set_outcome(x, guess)
    expect_equal(get_outcome(x_out), "guess")
    expect_true(has_outcome(x_out))

    x_out_trt = set_treatment(x_out, milk_first)
    expect_equal(get_outcome(x_out_trt), "guess")
    expect_equal(get_treatment(x_out_trt), c(guess="milk_first"))

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
        y = c(1, 3, 2, 3, 2, 4, 4, 5)
    )

    x_panel = set_panel(x, unit=id, time=year)
    expect_equal(get_panel(x_panel), list(unit = "id", time = "year"))
    expect_true(has_panel(x_panel))
    expect_no_error(validate_causal_tbl(x_panel))

    x_panel2 = set_treatment(set_outcome(x_panel, y), trt)
    expect_true(has_panel(x_panel2))
    expect_true(has_treatment(x_panel2))
    expect_true(has_outcome(x_panel2))

    expect_error(set_panel(x, unit=id), "for `time`")
    expect_error(set_panel(x, time=year), "for `unit`")
    expect_error(set_panel(x, unit=year, time=id), "convert `id`")
    expect_error(set_panel(x, unit=id, time=not_a_column), "doesn't exist")
})

test_that("lower-level getting and setting", {
    x <- causal_tbl(
        id = c("a", "a", "a", "a", "b", "b", "b", "b"),
        year = rep(2015:2018, 2),
        trt = c(0, 0, 0, 0, 0, 0, 1, 1),
        y = c(1, 3, 2, 3, 2, 4, 4, 5)
    )

    x2 <- set_causal_col(x, "treatments", y=trt, id=year)
    expect_equal(get_treatment(x2), c(y="trt"))
    expect_length(causal_cols(x2)$treatments, 2)

    x3 <- add_causal_col(x, "treatments", y=trt)
    expect_equal(get_treatment(x3), c(y="trt"))
    expect_length(causal_cols(x3)$treatments, 1)
    expect_error(add_causal_col(x, "treatments", y=trt, y2=trt2), "more than one")

    x3 <- set_causal_col(x3, "treatments", id=year)
    expect_equal(get_treatment(x3), c(id="year"))
    expect_length(causal_cols(x3)$treatments, 1)
    expect_error(set_causal_col(x3, "treatments"), "Must select")

    x4 <- add_causal_col(x, "pscore", trt=id, ptype=factor())
    expect_type(causal_cols(x4)$pscore, "character")
    expect_s3_class(x4[[causal_cols(x4)$pscore]], "factor")
    expect_equal(get_causal_col(x4, "pscore"), c(trt="id"))
})

