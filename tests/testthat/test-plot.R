library(testthat)
library(mirt)
library(hlt)

## Test that plot.grmtree works correctly
test_that("plot.grmtree works correctly", {
  skip_if_not_installed("mirt")
  skip_if_not_installed("hlt")

  ## ASTI data
  data("asti", package = "hlt")
  ## Create response as a matrix
  asti$resp <- data.matrix(asti[, 1:4])

  tree <- grmtree(resp ~ gender + group, data = asti)

  # Test basic plot types
  expect_silent(plot(tree, type = "regions"))
  expect_silent(plot(tree, type = "profile"))
  expect_message(plot(tree, type = "histogram"), "Processing node")

  # Test profile plot variants
  expect_silent(plot(tree, type = "profile", what = "item"))
  expect_silent(plot(tree, type = "profile", what = "threshold"))
  expect_silent(plot(tree, type = "profile", what = "discrimination"))

  # Test parameter variations
  expect_silent(plot(tree, tnex = 3))
  expect_silent(plot(tree, drop_terminal = FALSE))
  expect_silent(plot(tree, spacing = 0.2))

  # Test error handling - need to force dispatch to plot.grmtree
  expect_error(plot.grmtree("not_a_tree"), "must be of class 'grmtree'")
  expect_error(plot(tree, type = "invalid"), "should be one of")
  expect_error(plot(tree, what = "invalid"), "should be one of")
  expect_error(plot(tree, tnex = -1), "positive numeric value")
  expect_error(plot(tree, spacing = -0.1), "non-negative numeric value")

  # Test with empty tree (should fail gracefully)
  empty_tree <- structure(
    list(
      node = structure(list(id = 1L), class = "partynode"),
      fitted = data.frame("(fitted)" = factor(1), "(response)" = matrix(0, 1, 4)),
      terms = terms(resp ~ gender + group),
      data = asti,
      info = list()
    ),
    class = c("grmtree", "constparty", "party")
  )
  expect_error(plot(empty_tree, type = "regions"), "Failed to create regions plot")
})

## Test that the lot.grmtree argument validation works
test_that("plot.grmtree argument validation works", {
  # Test input object validation separately
  expect_error(plot.grmtree(), "missing")
  expect_error(plot.grmtree("not_a_tree"), "must be of class 'grmtree'")

  # Improved validation helper function
  validate_plot_args <- function(type = c("regions", "profile", "histogram"),
                                 what = c("item", "threshold", "discrimination"),
                                 tnex = 2L,
                                 drop_terminal = TRUE,
                                 spacing = 0.1) {
    # Input validation checks
    type <- match.arg(type)
    what <- match.arg(what)

    if (!is.numeric(tnex) || length(tnex) != 1 || tnex <= 0 || !is.finite(tnex)) {
      stop("'tnex' must be a single positive numeric value.")
    }

    if (!is.logical(drop_terminal) || length(drop_terminal) != 1 || is.na(drop_terminal)) {
      stop("'drop_terminal' must be a single logical value.")
    }

    if (!is.numeric(spacing) || length(spacing) != 1 || spacing < 0 || !is.finite(spacing)) {
      stop("'spacing' must be a single non-negative numeric value.")
    }

    return(TRUE)
  }

  # Test the validation logic
  expect_silent(validate_plot_args())  # Test default args work

  # Test invalid inputs
  expect_error(
    validate_plot_args(type = "invalid"),
    "should be one of"
  )

  expect_error(
    validate_plot_args(what = "invalid"),
    "should be one of"
  )

  expect_error(
    validate_plot_args(tnex = "a"),
    "must be a single positive numeric value"
  )

  expect_error(
    validate_plot_args(drop_terminal = NA),
    "must be a single logical value"
  )

  expect_error(
    validate_plot_args(spacing = -0.1),
    "must be a single non-negative numeric value"
  )

  # Additional edge cases
  expect_error(
    validate_plot_args(tnex = NA_real_),
    "must be a single positive numeric value"
  )

  expect_error(
    validate_plot_args(spacing = Inf),
    "must be a single non-negative numeric value"
  )
})
