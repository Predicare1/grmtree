library(testthat)
library(mirt)
library(hlt)


## Test that itempar_grmtree works with valid inputs
test_that("itempar_grmtree works with valid inputs", {
  skip_if_not_installed("mirt")
  skip_if_not_installed("hlt")

  ## ASTI data
  data("asti", package = "hlt")
  asti$resp <- data.matrix(asti[, 1:4])

  tree <- grmtree(resp ~ gender + group, data = asti,
                  control = grmtree.control(minbucket = 30))

  # Default usage
  expect_silent(items <- itempar_grmtree(tree))
  expect_s3_class(items, "data.frame")
  expect_true(nrow(items) > 0)
  expect_equal(names(items),
               c("Node", "Item", "Discrimination", "AvgThreshold", "Thresholds"))

  # Check parameter values
  expect_true(all(items$Discrimination > 0))
  expect_true(is.numeric(items$AvgThreshold)) # Thresholds typically can be positive or negative in GRM

  # Check thresholds list column
  expect_true(is.list(items$Thresholds))
  expect_equal(length(items$Thresholds[[1]]), 2) # n_categories - 1 thresholds

  # Specific nodes
  nodes <- partykit::nodeids(tree, terminal = TRUE)
  if (length(nodes) > 1) {
    expect_silent(items_sub <- itempar_grmtree(tree, node = nodes[1:2]))
    expect_equal(unique(items_sub$Node), nodes[1:2])
  }
})


## Test that itempar_grmtree handles errors appropriately
test_that("itempar_grmtree handles errors appropriately", {
  skip_if_not_installed("mirt")
  skip_if_not_installed("hlt")

  data("asti", package = "hlt")
  asti$resp <- data.matrix(asti[, 1:4])

  tree <- grmtree(resp ~ gender + group, data = asti,
                  control = grmtree.control(minbucket = 30))

  # Invalid object
  expect_error(itempar_grmtree(list()), "must be a grmtree object")

  # Invalid nodes
  nodes <- partykit::nodeids(tree, terminal = TRUE)
  expect_error(itempar_grmtree(tree, node = 999), "Invalid node IDs")

  # Create a tree with a single non-terminal node
  bad_tree <- tree
  bad_tree$node <- list(
    id = 1L,
    split = NULL,
    kids = NULL,
    surrogates = NULL,
    info = list()
  )
  class(bad_tree$node) <- "partynode"
  expect_error(itempar_grmtree(bad_tree), "No discrimination parameters found in node")

})

## Test that item parameters are consistent with thresholds
test_that("item parameters are consistent with thresholds", {
  skip_if_not_installed("mirt")
  skip_if_not_installed("hlt")

  data("asti", package = "hlt")
  asti$resp <- data.matrix(asti[, 1:4])

  tree <- grmtree(resp ~ gender + group, data = asti,
                  control = grmtree.control(minbucket = 30))
  items <- itempar_grmtree(tree)
  thresholds <- threshpar_grmtree(tree)

  # Check average thresholds match
  for (i in seq_len(nrow(items))) {
    node <- items$Node[i]
    item <- items$Item[i]
    calc_avg <- mean(unlist(items$Thresholds[i]))
    expect_equal(items$AvgThreshold[i], calc_avg)

    # Cross-check with threshpar output
    thresh_sub <- thresholds[thresholds$Node == node & thresholds$Item == item, ]
    thresh_values <- unlist(thresh_sub[, grep("^d", names(thresh_sub))])
    expect_equal(mean(thresh_values), items$AvgThreshold[i], tolerance = 1e-6)
  }
})
