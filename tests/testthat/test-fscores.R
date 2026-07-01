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

  # Test different methods - expect messages
  test_method <- function(method)  {
    # Just expect the message and check the result structure
    expect_message(
      res <- fscores_grmtree(tree, method),
      "Terminal nodes|Processing node"
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

