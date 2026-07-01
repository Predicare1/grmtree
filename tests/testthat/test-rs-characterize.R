library(testthat)
library(mirt)


# ==============================================================================
# rs_characterize()  input validation (fast, no fitting)
# ==============================================================================
test_that("rs_characterize validates its inputs", {
  # Wrong object class
  expect_error(rs_characterize(list()), "must be a longitudinal_grmtree object")

  # Bad p-adjust methods (checked before any fitting) require a real object,
  # so build a stub with the right class and info slot.
  stub <- structure(list(info = list(n_items = 4)),
                    class = c("longitudinal_grmtree", "grmtree",
                              "modelparty", "party"))
  expect_error(rs_characterize(stub, p_adjust = "invalid"),
               "valid p.adjust method")
  expect_error(rs_characterize(stub, global_p_adjust = "invalid"),
               "valid p.adjust method")
})

# ==============================================================================
# Visualization helpers  input validation (fast)
# ==============================================================================
test_that("plot_rs_tree and plot_rs_heatmap validate their inputs", {
  stub_tree <- structure(list(info = list(n_items = 4)),
                         class = c("longitudinal_grmtree", "grmtree",
                                   "modelparty", "party"))
  expect_error(plot_rs_tree(list(), list()),
               "must be a longitudinal_grmtree object")
  expect_error(plot_rs_tree(stub_tree, list()),
               "must be an rs_characterization object")
  expect_error(plot_rs_heatmap(list()),
               "must be an rs_characterization object")
})
