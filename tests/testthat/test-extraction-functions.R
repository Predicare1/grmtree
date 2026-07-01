
library(testthat)
library(mirt)


# ==============================================================================
# Extraction functions  input validation (fast)
# ==============================================================================
test_that("extraction functions reject non-longitudinal objects", {
  expect_error(threshpar_longitudinal_grmtree(list()),
               "must be a longitudinal_grmtree object")
  expect_error(discrpar_longitudinal_grmtree(list()),
               "must be a longitudinal_grmtree object")
  expect_error(itempar_longitudinal_grmtree(list()),
               "must be a longitudinal_grmtree object")
  expect_error(fscores_longitudinal_grmtree(list()),
               "must be a longitudinal_grmtree object")
  expect_error(latentpar_longitudinal_grmtree(list()),
               "must be a longitudinal_grmtree object")
})

test_that("fscores_longitudinal_grmtree rejects bad scoring methods", {
  stub <- structure(list(info = list(n_items = 4)),
                    class = c("longitudinal_grmtree", "grmtree",
                              "modelparty", "party"))
  expect_error(fscores_longitudinal_grmtree(stub, method = "invalid"),
               "method must be one of")
})

test_that("plot.longitudinal_grmtree rejects non-longitudinal objects", {
  expect_error(plot.longitudinal_grmtree(list()),
               "must be of class 'longitudinal_grmtree'")
})
