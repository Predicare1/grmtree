
library(testthat)
library(hlt)
library(mirt)

## Test that grmtree works as expected
test_that("grmtree produces correct object structure", {
  skip_if_not_installed("mirt")
  skip_if_not_installed("hlt")

  ## ASTI data
  data("asti", package = "hlt")
  ## Create response as a matrix
  asti$resp <- data.matrix(asti[, 1:4])

  # Basic functionality
  expect_silent({
    tree <- grmtree(resp ~ gender + group, data = asti,
                    control = grmtree.control(minbucket = 30))
  })

  expect_s3_class(tree, "grmtree")
  expect_true("grmtree" %in% class(tree))

  # Check response matrix requirement
  expect_error(
    grmtree(group ~ gender, data = asti),
    "Response variable must be a matrix"
  )
})


## Test that grmtree.control handles invalid inputs
test_that("grmtree.control handles invalid inputs", {
  # Valid inputs
  expect_silent(grmtree.control(minbucket = 10, alpha = 0.01))

  # Invalid minbucket
  expect_error(grmtree.control(minbucket = 0), "must be at least 1")
  expect_error(grmtree.control(minbucket = "a"), "must be numeric")

  # Invalid alpha
  expect_error(grmtree.control(alpha = 0), "must be between 0 and 1")
  expect_error(grmtree.control(alpha = 1.1), "must be between 0 and 1")

  # Invalid p_adjust
  expect_error(grmtree.control(p_adjust = "invalid"),
               "must be one of")
})

