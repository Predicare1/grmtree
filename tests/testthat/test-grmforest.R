library(testthat)
library(hlt)
library(mirt)

## Test for grmforest.control()
test_that("grmforest.control creates valid control object", {
  # Default parameters
  ctrl <- grmforest.control()
  expect_s3_class(ctrl, "grmforest_control")
  expect_equal(ctrl$n_tree, 100L)

  # Custom parameters
  ctrl <- grmforest.control(
    n_tree = 50,
    sampling = "subsample",
    sample_fraction = 0.5,
    mtry = 2,
    seed = 123
  )
  expect_equal(ctrl$n_tree, 50L)
  expect_equal(ctrl$sampling, "subsample")
  expect_equal(ctrl$seed, 123L)

  # Invalid parameters
  expect_error(grmforest.control(n_tree = 0), "must be at least 1")
  expect_error(grmforest.control(sampling = "invalid"), "must be one of")
  expect_error(grmforest.control(sample_fraction = 1.1), "between 0 and 1")
})

## Test for grmforest fitting
test_that("grmforest fits with valid inputs", {
  skip_if_not_installed("mirt")
  skip_if_not_installed("hlt")

  data("asti", package = "hlt")
  asti$resp <- data.matrix(asti[, 1:4])

  # Test basic forest - may or may not have warnings depending on tree success
  suppressWarnings({
    forest <- grmforest(resp ~ gender + group, data = asti,
                        control = grmforest.control(n_tree = 30))
  })

  expect_s3_class(forest, "grmforest")
  expect_true(length(forest$trees) > 0)  # At least some trees succeeded
  expect_equal(length(forest$trees), length(forest$oob_samples))

  # Test with subsampling
  suppressWarnings({
    forest <- grmforest(resp ~ gender + group, data = asti,
                        control = grmforest.control(n_tree = 30, sampling = "subsample"))
  })

  expect_s3_class(forest, "grmforest")
  expect_true(length(forest$trees) > 0)

  # Test invalid formula
  expect_error(
    suppressWarnings(grmforest(group ~ gender, data = asti)),
    "Response variable must be a matrix of item responses"
  )
})

## grmforest with edge cases and tree failures
test_that("grmforest handles errors appropriately", {
  skip_if_not_installed("mirt")
  skip_if_not_installed("hlt")

  data("asti", package = "hlt")
  asti$resp <- data.matrix(asti[, 1:4])

  # Too small dataset
  expect_error(
    grmforest(resp[1:5,] ~ gender + group, data = asti[1:5,]), "Insufficient data")

  # With tree failures allowed - may or may not have warnings
  ctrl <- grmforest.control(n_tree = 15, remove_dead_trees = TRUE)
  suppressWarnings({
    forest <- grmforest(resp ~ gender + group, data = asti, control = ctrl)
  })
  expect_s3_class(forest, "grmforest")
  expect_true(length(forest$trees) > 0)

  # Test invalid formula
  expect_error(
    grmforest(group ~ gender, data = asti),
    "Response variable must be a matrix"
  )

  # Test invalid control object
  bad_ctrl <- grmforest.control()
  class(bad_ctrl) <- "not_control"
  expect_error(
    grmforest(resp ~ gender + group, data = asti, control = bad_ctrl),
    "'control' must be created by grmforest.control()"
  )
})

## Test for print method
test_that("print.grmforest works correctly", {
  skip_if_not_installed("mirt")
  skip_if_not_installed("hlt")

  data("asti", package = "hlt")
  asti$resp <- data.matrix(asti[, 1:4])

  suppressWarnings({
    forest <- grmforest(resp ~ gender + group, data = asti,
                        control = grmforest.control(n_tree = 2, seed = 123))
  })

  expect_output(print(forest), "GRM Forest with")
})
