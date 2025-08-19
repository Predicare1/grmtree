library(testthat)
library(hlt)
library(mirt)


## Test that discrpar_grmtree works as expected
test_that("discrpar_grmtree works with valid inputs", {

  skip_if_not_installed("mirt")
  skip_if_not_installed("hlt")

  ## Asti data
  data("asti", package = "hlt")
  ## Create response as a matrix
  asti$resp <- data.matrix(asti[, 1:4])

  tree <- grmtree(resp ~ gender + group, data = asti,
                  control = grmtree.control(minbucket = 30))

  # Default usage
  expect_silent(discr <- discrpar_grmtree(tree))
  expect_s3_class(discr, "data.frame")
  expect_true(nrow(discr) > 0)
  expect_equal(names(discr), c("Node", "Item", "Discrimination"))

  # Check discrimination values
  expect_true(all(discr$Discrimination > 0))

  # Specific nodes
  nodes <- partykit::nodeids(tree, terminal = TRUE)
  if (length(nodes) > 1) {
    expect_silent(discr_sub <- discrpar_grmtree(tree, node = nodes[1:2]))
    expect_equal(unique(discr_sub$Node), nodes[1:2])
  }
})


## Test that discrpar_grmtree handles errors appropriately
test_that("discrpar_grmtree handles errors appropriately", {
  skip_if_not_installed("mirt")
  skip_if_not_installed("hlt")

  data("asti", package = "hlt")
  asti$resp <- data.matrix(asti[, 1:4])

  tree <- grmtree(resp ~ gender + group, data = asti,
                  control = grmtree.control(minbucket = 30))

  # 1. Test invalid object type
  expect_error(discrpar_grmtree(list()), "must be a grmtree object")

  # 2. Test invalid nodes
  nodes <- partykit::nodeids(tree, terminal = TRUE)
  expect_error(discrpar_grmtree(tree, node = 999), "Invalid node IDs")

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
  expect_error(discrpar_grmtree(bad_tree), "No discrimination parameters found in node")

})


## Test that discrimination parameters are reasonable
test_that("discrimination parameters are reasonable", {
  skip_if_not_installed("mirt")
  skip_if_not_installed("hlt")

  ## Asti data
  data("asti", package = "hlt")
  ## Create response as a matrix
  asti$resp <- data.matrix(asti[, 1:4])

  tree <- grmtree(resp ~ gender + group, data = asti,
                  control = grmtree.control(minbucket = 30))
  discr <- discrpar_grmtree(tree)

  # Typical discrimination values in educational testing
  expect_true(all(discr$Discrimination > 0.2))
  expect_true(all(discr$Discrimination < 4))

  # Check items are correctly labeled
  expect_true(all(discr$Item %in% paste0("resp",colnames(asti[, 1:4]))))
})
