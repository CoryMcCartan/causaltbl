test_that("causal_idx constructor", {
    idx <- causal_idx(list(2, c(1, NA, 3), 2))
    expect_s3_class(idx, "causal_idx")
    expect_type(idx, "list")
    expect_true(is_causal_idx(idx))

    expect_error(causal_idx(list("a")), "character")
    expect_error(causal_idx(5), "must be a list")
})

test_that("causal_idx conversion", {
    idx <- causal_idx(list(2, c(1, NA, 3), 2))

    expect_s3_class(as_causal_idx(as.list(idx)), "causal_idx")
    expect_type(as.list(idx), "list")
    expect_type(c(idx, list()), "list")
    expect_type(c(list(), idx), "list")
})

test_that("causal_idx slicing", {
    idx <- causal_idx(list(2, c(1, NA, 3), 2))

    expect_equal(idx[1:3], idx)
    expect_equal(idx[1:2],
                 causal_idx(list(2, c(1, NA, NA))))
    expect_equal(idx[2:1],
                 causal_idx(list(c(2, NA, NA), 1)))
})


test_that("causal_idx printing", {
    skip_on_ci()
    skip_on_cran()

    idx <- causal_idx(list(2, c(1, NA, 3), 2))

    expect_snapshot(print(idx))
    expect_snapshot(str(idx))
})
