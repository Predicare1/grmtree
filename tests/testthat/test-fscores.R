library(testthat)
library(hlt)
library(mirt)

## Test that fscores_grmtree input validation works
test_that("fscores_grmtree input validation works", {
  # Create a simple GRM tree for testing
  set.seed(123)
  sim_data <- data.frame(
    x1 = rnorm(100),
    x2 = sample(0:1, 100, replace = TRUE),
    item1 = sample(1:5, 100, replace = TRUE),
    item2 = sample(1:5, 100, replace = TRUE)
  )

  # Fit a simple tree (assuming grmtree function exists)
  tree <- grmtree(cbind(item1, item2) ~ x1 + x2, data = sim_data)

  # Test input validation
  expect_error(fscores_grmtree(), "missing with no default")
  expect_error(fscores_grmtree("not_a_tree"), "must be a GRM tree object")
  expect_error(fscores_grmtree(tree, method = "invalid"), "must be one of")
})


## Test that fscores_grmtree returns correct structure
test_that("fscores_grmtree returns correct structure", {
  skip_if_not_installed("mirt")
  skip_if_not_installed("hlt")

  ## Asti data
  data("asti", package = "hlt")
  ## Create response as a matrix
  asti$resp <- data.matrix(asti[, 1:4])

  tree <- grmtree(resp ~ gender + group, data = asti,
                  control = grmtree.control(minbucket = 30))

  # Test output structure - allow messages from mirt
  expect_message(
    result <- fscores_grmtree(tree),
    "Processing node"
  )
  expect_type(result, "list")
  expect_true(length(result) > 0)
  expect_true(all(sapply(result, is.numeric)))

  # Test different methods - expect messages but no warnings
  test_method <- function(method) {
    expect_warning(
      expect_message(
        res <- fscores_grmtree(tree, method),
        "Terminal nodes|Processing node"
      ),
      NA  # Expect no warnings
    )
    expect_true(all(sapply(res, is.numeric)))
  }

  test_method("MAP")
  test_method("ML")
  test_method("WLE")

  # Test invalid inputs
  expect_error(fscores_grmtree(list()), "must be a GRM tree object")
  expect_error(fscores_grmtree(tree, method = "invalid"),
               "must be one of: 'EAP', 'MAP', 'ML', or 'WLE'")
})


## Test that generate_node_scores_dataset works correctly
test_that("generate_node_scores_dataset works correctly", {
  skip_if_not_installed("mirt")
  skip_if_not_installed("hlt")

  # Load and prepare asti data
  data("asti", package = "hlt")
  asti$resp <- data.matrix(asti[, 1:4])

  # Fit GRM tree
  tree <- grmtree(resp ~ gender + group, data = asti,
                  control = grmtree.control(minbucket = 30))

  # Test basic functionality - allow messages from fscores computation
  expect_message(
    result <- generate_node_scores_dataset(tree),
    "Processing node"  # Expect the progress message
  )

  expect_s3_class(result, "data.frame")
  expect_true("factor_score" %in% names(result))
  expect_true("node" %in% names(result))
  expect_equal(nrow(result), nrow(asti))

  # Test that row order is preserved
  expect_equal(result$gender, asti$gender)

  # Test with different methods - expect messages but no warnings
  test_method <- function(method) {
    expect_warning(
      expect_message(
        res <- generate_node_scores_dataset(tree, method),
        "Terminal nodes|Processing node"
      ),
      NA  # Expect no warnings
    )
    expect_s3_class(res, "data.frame")
    expect_true("factor_score" %in% names(res))
  }

  test_method("MAP")
  test_method("ML")
  test_method("WLE")

})


## Test that generate_node_scores_dataset handles errors
test_that("generate_node_scores_dataset handles errors gracefully", {
  skip_if_not_installed("mirt")
  skip_if_not_installed("hlt")

  ## Load the ASTI data
  data("asti", package = "hlt")
  asti$resp <- data.matrix(asti[, 1:4])

  tree <- grmtree(resp ~ gender + group, data = asti,
                  control = grmtree.control(minbucket = 30))

  # Test invalid inputs
  expect_error(generate_node_scores_dataset(list()),
               "must be a GRM tree object")
  expect_error(generate_node_scores_dataset(tree, method = "invalid"),
               "must be one of: 'EAP', 'MAP', 'ML', or 'WLE'")

  # Create a properly structured empty tree
  empty_tree <- structure(
    list(
      node = structure(list(id = 1L), class = "partynode"),
      fitted = data.frame(),
      terms = terms(resp ~ gender + group),
      data = asti,
      info = list()
    ),
    class = c("grmtree", "constparty", "party")
  )

  # Now this should properly error
  expect_error(
    generate_node_scores_dataset(empty_tree),
    "No factor scores available to create dataset"
  )
})
