
library(testthat)
library(mirt)

# ==============================================================================
# longitudinal_grmtree()  input validation (fast)
# ==============================================================================

test_that("longitudinal_grmtree validates its inputs", {
  df <- make_tiny_long()
  it1 <- paste0("I", 1:4, "_T1")
  it2 <- paste0("I", 1:4, "_T2")
  ld  <- prepare_longitudinal_data(df, items_t1 = it1, items_t2 = it2,
                                   covariates = c("grp", "age"))

  # Not a formula
  expect_error(
    longitudinal_grmtree("resp_wide ~ grp", data = ld, n_items = 4),
    "must be a formula"
  )
  # Not a data.frame
  expect_error(
    longitudinal_grmtree(resp_wide ~ grp, data = ld$resp_wide, n_items = 4),
    "must be a data.frame"
  )
  # Wrong n_items given the matrix width
  expect_error(
    longitudinal_grmtree(resp_wide ~ grp, data = ld, n_items = 6),
    "Expected 12 columns"
  )
})
