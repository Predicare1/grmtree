################################################################################
##
## Tests for generate_node_scores_dataset() (unified grmtree + longitudinal)
##
## The unified version:
##   - Signature: generate_node_scores_dataset(object, data = NULL, method)
##   - With data = NULL: returns only `node` + score column(s)
##   - With data = <df>: returns all original columns + node + score column(s)
##   - Cross-sectional -> `factor_score`; longitudinal -> `Theta_T1`, `Theta_T2`
##
################################################################################

library(testthat)
library(mirt)

# ==============================================================================
# 1. Cross-sectional grmtree
# ==============================================================================
test_that("generate_node_scores_dataset works for cross-sectional grmtree", {
  skip_if_not_installed("mirt")
  skip_if_not_installed("hlt")

  data("asti", package = "hlt")
  asti$resp <- data.matrix(asti[, 1:4])
  tree <- grmtree(resp ~ gender + group, data = asti,
                  control = grmtree.control(minbucket = 30))

  # With data supplied: all original columns + node + factor_score
  result <- suppressMessages(
    generate_node_scores_dataset(tree, data = asti)
  )
  expect_s3_class(result, "data.frame")
  expect_true(all(c("node", "factor_score") %in% names(result)))
  expect_equal(nrow(result), nrow(asti))
  # Original covariates are preserved and in order
  expect_true("gender" %in% names(result))
  expect_equal(result$gender, asti$gender)

  # With data = NULL: minimal output (node + factor_score only)
  minimal <- suppressMessages(generate_node_scores_dataset(tree))
  expect_true(all(c("node", "factor_score") %in% names(minimal)))
  expect_equal(nrow(minimal), nrow(asti))
})

test_that("generate_node_scores_dataset accepts all scoring methods", {
  skip_if_not_installed("mirt")
  skip_if_not_installed("hlt")

  data("asti", package = "hlt")
  asti$resp <- data.matrix(asti[, 1:4])
  tree <- grmtree(resp ~ gender + group, data = asti,
                  control = grmtree.control(minbucket = 30))

  for (m in c("EAP", "MAP", "ML", "WLE")) {
    res <- suppressMessages(
      generate_node_scores_dataset(tree, data = asti, method = m)
    )
    expect_s3_class(res, "data.frame")
    expect_true("factor_score" %in% names(res))
  }
})

# ==============================================================================
# 2. Input validation
# ==============================================================================
test_that("generate_node_scores_dataset validates its inputs", {
  skip_if_not_installed("mirt")
  skip_if_not_installed("hlt")

  data("asti", package = "hlt")
  asti$resp <- data.matrix(asti[, 1:4])
  tree <- grmtree(resp ~ gender + group, data = asti,
                  control = grmtree.control(minbucket = 30))

  # Wrong object class
  expect_error(generate_node_scores_dataset(list()),
               "must be a grmtree or longitudinal_grmtree object")
  # Invalid scoring method
  expect_error(generate_node_scores_dataset(tree, method = "invalid"),
               "must be one of: 'EAP', 'MAP', 'ML', or 'WLE'")
})

# ==============================================================================
# 3. Longitudinal grmtree (skipped on CRAN for time)
# ==============================================================================
test_that("generate_node_scores_dataset works for longitudinal_grmtree", {
  skip_on_cran()
  skip_if_not_installed("mirt")
  skip_if_not_installed("partykit")

  df  <- make_tiny_long(n = 250, n_items = 4, seed = 7)
  it1 <- paste0("I", 1:4, "_T1")
  it2 <- paste0("I", 1:4, "_T2")
  ld  <- prepare_longitudinal_data(df, items_t1 = it1, items_t2 = it2,
                                   covariates = c("grp", "age"))

  tree <- longitudinal_grmtree(resp_wide ~ grp + age, data = ld, n_items = 4,
                               control = grmtree.control(minbucket = 80))

  # With data supplied: original columns + node + Theta_T1 + Theta_T2
  result <- suppressMessages(
    generate_node_scores_dataset(tree, data = ld)
  )
  expect_s3_class(result, "data.frame")
  expect_true(all(c("node", "Theta_T1", "Theta_T2") %in% names(result)))
  expect_false("factor_score" %in% names(result))  # longitudinal, not x-sectional
  expect_equal(nrow(result), nrow(ld))

  # With data = NULL: minimal output (node + the two thetas)
  minimal <- suppressMessages(generate_node_scores_dataset(tree))
  expect_true(all(c("node", "Theta_T1", "Theta_T2") %in% names(minimal)))
})
