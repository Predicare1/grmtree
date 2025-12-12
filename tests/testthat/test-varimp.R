library(testthat)
library(mirt)
library(hlt)

## Test for basic varimp functionality
test_that("varimp works with valid forest", {
  skip_if_not_installed("mirt")
  skip_if_not_installed("hlt")

  data("asti", package = "hlt")
  asti$resp <- data.matrix(asti[, 1:4])

  # Create forest - may or may not have warnings
  suppressWarnings({
    forest <- grmforest(resp ~ gender + group, data = asti,
                        control = grmforest.control(n_tree = 30))
  })

  # Basic functionality
  imp <- varimp(forest)

  expect_type(imp, "double")
  expect_named(imp)
  expect_s3_class(imp, "varimp")

  # With verbose - expect specific message
  expect_message(
    varimp(forest, verbose = TRUE),
    "Processing variable"
  )

  # With seed - test reproducibility
  imp1 <- varimp(forest, seed = 123)
  imp2 <- varimp(forest, seed = 123)
  expect_equal(imp1, imp2)

  # Additional test - check we have some non-zero importance values
  expect_true(any(imp != 0))
})

## Test case for edge cases
test_that("varimp handles edge cases", {
  skip_if_not_installed("mirt")
  skip_if_not_installed("hlt")

  data("asti", package = "hlt")
  asti$resp <- data.matrix(asti[, 1:4])

  # Create forest
  suppressWarnings({
    forest <- grmforest(resp ~ gender + group, data = asti,
                        control = grmforest.control(n_tree = 30))
  })

  # Invalid inputs
  expect_error(varimp(list()), "must be a grmforest object")
  expect_error(varimp(forest, method = "invalid"), "Only permutation importance is implemented")

  # Create a properly structured bad forest with no valid trees
  bad_forest <- structure(
    list(
      trees = list(NULL, NULL),  # All trees NULL
      oob_samples = list(asti[1:10,], asti[11:20,]),
      formula = resp ~ gender + group,
      data = asti,
      call = NULL
    ),
    class = "grmforest"
  )

  # Expect warnings about no valid trees - suppress additional warnings
  suppressWarnings({
    expect_warning(
      result <- varimp(bad_forest),
      "No valid trees"
    )
  })

  # Should return named vector of zeros with proper class
  expect_type(result, "double")
  expect_named(result)
  expect_s3_class(result, "varimp")
  expect_true(all(result == 0))
  expect_length(result, 2)
})

## Test for varimp plot
test_that("plot.varimp works correctly", {
  skip_if_not_installed("mirt")
  skip_if_not_installed("hlt")
  skip_if_not_installed("ggplot2")

  data("asti", package = "hlt")
  asti$resp <- data.matrix(asti[, 1:4])

  # Create the forest object
  suppressWarnings({
    forest <- grmforest(resp ~ gender + group, data = asti,
                        control = grmforest.control(n_tree = 30))
  })

  # Create the variable importance
  imp <- varimp(forest)

  # Base R plot
  expect_silent(plot(imp, use_ggplot = FALSE))

  # ggplot2 plot if available
  expect_silent(plot(imp, use_ggplot = TRUE))

  # Top n variables
  expect_silent(plot(imp, top_n = 1))

  # Invalid inputs
  expect_error(plot.varimp(list()), "must be a varimp object")
  expect_error(plot(imp, top_n = 0), "must be NULL or a positive integer")

  # Additional test for non-varimp objects
  expect_error(
    plot.varimp(structure(list(), class = "not_varimp")),
    "must be a varimp object"
  )
})

## Test for the internal helper functions
test_that("internal helper functions work", {
  skip_if_not_installed("mirt")
  skip_if_not_installed("hlt")

  data("asti", package = "hlt")
  asti$resp <- data.matrix(asti[, 1:4])

  tree <- grmtree(resp ~ gender + group, data = asti,
                  control = grmtree.control(minbucket = 30))

  # get_node_model
  nodes <- partykit::nodeids(tree, terminal = TRUE)
  expect_silent(model <- get_node_model(tree, nodes[1]))
  expect_true(inherits(model, "SingleGroupClass") || is.null(model))

  # calculate_node_loglik
  dat <- asti[1:5, ]
  attr(dat, "response.var") <- "resp"
  expect_type(calculate_node_loglik(model, dat), "double")

  # evaluate_tree
  expect_type(evaluate_tree(tree, asti[1:5, ]), "double")

  # evaluate_forest
  suppressWarnings({
    forest <- grmforest(resp ~ gender + group, data = asti,
                        control = grmforest.control(n_tree = 30))
  })
  expect_type(evaluate_forest(forest), "double")
})
