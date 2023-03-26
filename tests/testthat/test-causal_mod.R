
test_that("causal_mod constructor", {
    m <- lm(yield ~ block + N*P*K, data=npk)
    x <- causal_mod(m)
    expect_s3_class(x, "causal_mod")
    expect_type(x, "double")
    expect_true(is_causal_mod(x))

    expect_error(causal_mod(5), "atomic")
    expect_error(new_causal_mod(5), "atomic")
    expect_error(causal_mod(list()), "fitted()")

    d = rbind(NA, npk, NA, npk)
    m <- lm(yield ~ block + N*P*K, data=d)
    x <- causal_mod(m)
    expect_equal(which(is.na(x)), unname(c(na.action(m))))
})

test_that("causal_mod conversion", {
    m <- lm(yield ~ block + N*P*K, data=npk)
    x <- causal_mod(m)

    expect_type(as.double(x), "double")
    expect_type(c(x, double()), "double")
    expect_type(c(double(), x), "double")
})

test_that("causal_mod slicing", {
    m <- lm(yield ~ block + N*P*K, data=npk)
    x <- causal_mod(m)

    expect_equal(as.double(x[4:2]), unname(fitted(m)[4:2]))
    expect_equal(attr(x[4:2], "idx"), 4:2)
})


test_that("causal_mod printing", {
    m <- lm(yield ~ block + N*P*K, data=npk)
    x <- causal_mod(m)

    expect_snapshot(print(x))
    expect_snapshot(str(x))
})

