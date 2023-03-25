test_that("indices constructor", {
    idx <- indices(list(2, c(1, NA, 3), 2))
    expect_s3_class(idx, "indices")
    expect_type(idx, "list")
    expect_true(is_indices(idx))

    expect_error(indices(list("a")), "character")
    expect_error(indices(5), "must be a list")
})

test_that("indices conversion", {
    idx <- indices(list(2, c(1, NA, 3), 2))

    expect_s3_class(as_indices(as.list(idx)), "indices")
    expect_type(as.list(idx), "list")
    expect_type(c(idx, list()), "list")
    expect_type(c(list(), idx), "list")
})

test_that("indices slicing", {
    idx <- indices(list(2, c(1, NA, 3), 2))

    expect_equal(idx[1:3], idx)
    expect_equal(idx[1:2],
                 indices(list(2, c(1, NA, NA))))
    expect_equal(idx[2:1],
                 indices(list(c(2, NA, NA), 1)))
})


test_that("indices printing", {
    idx <- indices(list(2, c(1, NA, 3), 2))

    expect_snapshot(print(idx))
    expect_snapshot(str(idx))
})
