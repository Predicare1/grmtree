library(testthat)
library(mirt)
library(hlt)

## Test that threshpar_grmtree works with valid inputs
test_that("threshpar_grmtree works with valid inputs", {

  cat("\n=== Starting threshpar test ===\n")
  skip_if_not_installed("mirt")
  skip_if_not_installed("hlt")

  ## ASTI data
  data("asti", package = "hlt")
  asti$resp <- data.matrix(asti[, 1:4])

  ## Create the grmtree object
  tree <- grmtree(resp ~ gender + group, data = asti)

  # Default usage
  expect_silent(thresh <- threshpar_grmtree(tree))
  expect_s3_class(thresh, "data.frame")
  expect_true(nrow(thresh) > 0)
  expect_true(all(c("Node", "Item") %in% names(thresh)))

  # Specific nodes
  nodes <- partykit::nodeids(tree, terminal = TRUE)
  if (length(nodes) > 1) {
    expect_silent(thresh_sub <- threshpar_grmtree(tree, node = nodes[1:2]))
    expect_equal(unique(thresh_sub$Node), nodes[1:2])
  }
  cat("Current objects:", ls(), "\n")
})


## Test that threshpar_grmtree handles errors appropriately
test_that("threshpar_grmtree handles errors appropriately", {
  skip_if_not_installed("mirt")
  skip_if_not_installed("hlt")

  data("asti", package = "hlt")
  asti$resp <- data.matrix(asti[, 1:4])

  tree <- grmtree(resp ~ gender + group, data = asti[1:1000,])

  # Invalid object
  expect_error(threshpar_grmtree(list()), "must be a grmtree object")

  # Invalid nodes
  nodes <- partykit::nodeids(tree, terminal = TRUE)
  expect_error(threshpar_grmtree(tree, node = 999), "Invalid node IDs")

})

## Test that apply_to_models helper works correctly
test_that("apply_to_models helper works correctly", {
  skip_if_not_installed("mirt")
  skip_if_not_installed("hlt")


  data(asti, package = "hlt")
  asti$resp <- data.matrix(asti[, 1:4])

  tree <- grmtree(resp ~ gender + group, data = asti)

  # Default usage
  expect_silent(models <- apply_to_models(tree))
  expect_type(models, "list")
  expect_true(length(models) > 0)

  # With custom function
  expect_silent(coefs <- apply_to_models(tree, FUN = function(x) mirt::coef(x, simplify = TRUE)))
  expect_true(all(sapply(coefs, is.list)))

  # With specific nodes
  nodes <- partykit::nodeids(tree, terminal = TRUE)[1:2]
  expect_silent(sub_models <- apply_to_models(tree, node = nodes))
  expect_equal(length(sub_models), length(nodes))

  # Error cases
  expect_error(apply_to_models(list()), "must be a grmtree object")
  expect_error(apply_to_models(tree, FUN = "not_a_function"), "must be a function")
})


## Test that the internal grm_threshpar works
test_that("internal grm_threshpar works", {
  skip_if_not_installed("mirt")
  skip_if_not_installed("hlt")

  data(asti, package = "hlt")
  asti$resp <- data.matrix(asti[, 1:4])

  tree <- grmtree(resp ~ gender + group, data = asti)

  expect_silent(thresh <- grm_threshpar(tree))
  expect_true(is.matrix(thresh))
  expect_true(nrow(thresh) > 0)

  nodes <- partykit::nodeids(tree, terminal = TRUE)
  if (length(nodes) > 1) {
    expect_silent(thresh_sub <- grm_threshpar(tree, node = nodes[1:2]))
    expect_equal(nrow(thresh_sub), length(nodes[1:2]) * ncol(asti[, 1:4]))
  }
})
