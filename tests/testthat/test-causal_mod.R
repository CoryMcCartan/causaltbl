
test_that("causal_mod constructor", {
    m <- lm(yield ~ block + N, data=npk)
    x <- causal_mod(m)
    expect_s3_class(x, "causal_mod")
    expect_type(x, "double")
    expect_true(is_causal_mod(x))
    expect_equal(length(x), nrow(npk))

    expect_error(causal_mod(5), "atomic")
    expect_error(new_causal_mod(5), "atomic")
    expect_error(causal_mod(list()), "fitted()")
    expect_error(causal_mod(list(fitted="a")), "numeric")

    d = rbind(NA, npk, NA, npk)
    m <- lm(yield ~ block + N, data=d)
    x <- causal_mod(m)
    expect_equal(which(is.na(x)), unname(c(na.action(m))))
})

test_that("causal_mod conversion", {
    m <- lm(yield ~ block + N, data=npk)
    x <- causal_mod(m)

    expect_type(as.double(x), "double")
    expect_type(c(x, double()), "double")
    expect_type(c(double(), x), "double")
})

test_that("causal_mod getters", {
    m <- lm(yield ~ block + N, data=npk)
    x <- causal_mod(m)

    expect_identical(get_model_fit(x), m)
    expect_equal(get_model_idx(x), seq_len(nrow(npk)))

    expect_error(get_model_fit(5), "causal_mod")
    expect_error(get_model_idx(5), "causal_mod")
})

test_that("causal_mod slicing", {
    m <- lm(yield ~ block + N, data=npk)
    x <- causal_mod(m)

    expect_equal(as.double(x[4:2]), unname(fitted(m)[4:2]))
    expect_equal(attr(x[4:2], "idx"), 4:2)
})

test_that("causal_mod generics", {
    m <- lm(yield ~ block + N, data=npk)
    x <- causal_mod(m)

    expect_identical(fitted(x), fitted(m))
    expect_identical(resid(x), residuals(m))
    expect_identical(predict(x), predict(m))
    expect_identical(simulate(x, seed=5118), simulate(m, seed=5118))
    expect_identical(summary(x), summary(m))
    expect_identical(nobs(x), nobs(m))
    expect_identical(vcov(x), vcov(m))
    expect_identical(logLik(x), logLik(m))
    expect_identical(confint(x, "block2"), confint(m, "block2"))
    expect_identical(formula(x), formula(m))
})

test_that("causal_mod arithmetic", {
    m <- lm(yield ~ block + N, data=npk)
    x <- causal_mod(m)

    expect_equal(x - x, rep(0, nrow(npk)))
    expect_equal(x + 1, unname(fitted(m)) + 1)
    expect_equal(1 + x, unname(fitted(m)) + 1)
})

test_that("causal_mod printing", {
    skip_on_cran()

    m <- lm(yield ~ block + N, data=npk)
    x <- causal_mod(m)

    expect_snapshot(print(x))
    expect_snapshot(str(x))
})

