

library(testthat)
library(mirt)

# ==============================================================================
# END-TO-END smoke test (slow-ish; skipped on CRAN)
# ==============================================================================
test_that("longitudinal_grmtree + rs_characterize run end-to-end", {
  skip_on_cran()
  skip_if_not_installed("mirt")
  skip_if_not_installed("partykit")

  df  <- make_tiny_long(n = 250, n_items = 4, seed = 42)
  it1 <- paste0("I", 1:4, "_T1")
  it2 <- paste0("I", 1:4, "_T2")
  ld  <- prepare_longitudinal_data(df, items_t1 = it1, items_t2 = it2,
                                   covariates = c("grp", "age"))

  # Fit tree (no true DIF -> typically no split, which is fine)
  tree <- longitudinal_grmtree(resp_wide ~ grp + age, data = ld, n_items = 4,
                               control = grmtree.control(minbucket = 80))

  expect_s3_class(tree, "longitudinal_grmtree")
  expect_true("longitudinal_grmtree" %in% class(tree))
  expect_equal(tree$info$n_items, 4)

  # Phase 2 runs even on a single (root) node
  rs <- rs_characterize(tree, verbose = FALSE)
  expect_s3_class(rs, "rs_characterization")
  expect_true(is.data.frame(rs$global))
  expect_true(all(c("Node", "LRT_chi2", "LRT_p", "RS_detected") %in%
                    names(rs$global)))

  # Extraction functions return the unique (T1) items only
  th <- threshpar_longitudinal_grmtree(tree)
  expect_s3_class(th, "data.frame")
  n_term <- length(partykit::nodeids(tree, terminal = TRUE))
  expect_equal(nrow(th), n_term * 4)          # n_items per node, not 2*n_items

  dp <- discrpar_longitudinal_grmtree(tree)
  expect_true("Discrimination" %in% names(dp))

  lp <- latentpar_longitudinal_grmtree(tree)
  expect_true(all(c("mu_T2", "sigma2_T2", "cor_T1_T2") %in% names(lp)))

  # Factor scores return two columns (Theta_T1, Theta_T2)
  fs <- fscores_longitudinal_grmtree(tree)
  first <- fs[[1]]
  expect_true(all(c("Theta_T1", "Theta_T2") %in% names(first)))
})
