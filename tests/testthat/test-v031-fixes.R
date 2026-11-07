## Tests for the fixes introduced in grmtree 0.3.1

test_that("item names carry no response prefix; coef() and logLik() work", {
  skip_on_cran()
  skip_if_not_installed("hlt")
  data("asti", package = "hlt")
  asti$resp <- data.matrix(asti[, 1:4])
  tree <- grmtree(resp ~ gender + group, data = asti,
                  control = grmtree.control(minbucket = 30))

  # clean item names in extractor output
  expect_true(all(discrpar_grmtree(tree)$Item %in% colnames(asti[, 1:4])))

  # one coefficient per IRT item parameter in every terminal node
  cf <- coef(tree)
  n_term <- length(partykit::nodeids(tree, terminal = TRUE))
  n_par <- sum(sapply(asti[, 1:4], function(x) length(unique(x))))
  if (n_term == 1L) {
    expect_length(cf, n_par)
  } else {
    expect_equal(dim(cf), c(n_term, n_par))
  }

  # logLik equals the sum of the node objectives; AIC/BIC available
  ll <- logLik(tree)
  expect_s3_class(ll, "logLik")
  obj <- unlist(partykit::nodeapply(tree, ids = partykit::nodeids(tree, terminal = TRUE),
                                    FUN = function(n) partykit::info_node(n)$objfun))
  expect_equal(as.numeric(ll), -sum(obj))
  expect_equal(attr(ll, "df"), n_term * n_par + (n_term - 1L))
  expect_true(is.finite(AIC(tree)) && is.finite(BIC(tree)))
})

test_that("tree-wide adjustment keeps other mob_control settings", {
  skip_on_cran()
  skip_if_not_installed("hlt")
  data("asti", package = "hlt")
  asti$resp <- data.matrix(asti[, 1:4])
  tree <- grmtree(resp ~ gender + group, data = asti,
                  control = grmtree.control(minbucket = 30, p_adjust = "BH",
                                            maxdepth = 1))
  expect_lte(length(partykit::nodeids(tree)), 3L)
})

test_that("rs_characterize uses correct df and estimated follow-up variance", {
  skip_on_cran()
  df <- make_tiny_long()
  ld <- prepare_longitudinal_data(df, items_t1 = paste0("I", 1:4, "_T1"),
                                  items_t2 = paste0("I", 1:4, "_T2"),
                                  covariates = c("grp", "age"))
  tree <- longitudinal_grmtree(resp_wide ~ grp, data = ld, n_items = 4,
                               control = grmtree.control(minbucket = 80))
  rs <- rs_characterize(tree, verbose = FALSE)
  # 4 items x (1 slope + 4 thresholds) = 20 constraints, minus T2 mean and variance
  n_cons <- sum(apply(ld$resp_wide[, 1:4], 2, function(x) length(unique(x))))
  expect_true(all(rs$global$LRT_df == n_cons - 2, na.rm = TRUE))
  # sigma2_T2 is estimated, matching Phase 1
  lp <- latentpar_longitudinal_grmtree(tree)
  expect_equal(unname(rs$global$sigma2_T2), unname(round(lp$sigma2_T2, 3)),
               tolerance = 0.01)

  rs_p <- rs_characterize(tree, verbose = FALSE, purify = TRUE)
  expect_true(isTRUE(rs_p$purify))
  if (!is.null(rs_p$item_level)) {
    expect_true("iterations" %in% names(rs_p$item_level))
  }
})

test_that(".align_categories gives each item pair the same categories", {
  y <- cbind(c(1, 2, 3, 4, 5, 2), c(1, 1, 2, 2, 3, 3),   # T1 items
             c(2, 2, 3, 4, 5, 3), c(1, 2, 2, 3, 3, 1))   # T2 items
  ya <- grmtree:::.align_categories(y, n_items = 2)
  # item 1: category 1 is empty at T2, so it is merged with category 2
  expect_equal(sort(unique(ya[, 1])), sort(unique(ya[, 3])))
  expect_equal(ya[, 1], c(1, 1, 2, 3, 4, 1))
  expect_equal(ya[, 3], c(1, 1, 2, 3, 4, 2))
  # item 2: same categories at both occasions, left unchanged
  expect_equal(ya[, c(2, 4)], y[, c(2, 4)])
})

test_that("Wald type tests separate slope and location shifts", {
  skip_on_cran()
  set.seed(42)
  M <- 4; n <- 1500
  a <- c(2, 1.8, 2.2, 1.6); b <- t(replicate(M, sort(runif(4, -1.8, 1.2))))
  a2 <- a; b2 <- b
  a2[2] <- a[2] * 0.5          # item 2: reprioritization
  b2[3, ] <- b[3, ] + 0.8      # item 3: recalibration
  th1 <- rnorm(n); th2 <- 0.6 * th1 + sqrt(1 - 0.36) * rnorm(n)
  r1 <- mirt::simdata(a = matrix(a, ncol = 1), d = -a * b, Theta = matrix(th1),
                      itemtype = "graded") + 1L
  r2 <- mirt::simdata(a = matrix(a2, ncol = 1), d = -a2 * b2, Theta = matrix(th2),
                      itemtype = "graded") + 1L
  y <- cbind(r1, r2)
  colnames(y) <- c(paste0("I", 1:M, "_T1"), paste0("I", 1:M, "_T2"))
  y <- grmtree:::.align_categories(y, M)
  m <- 'Theta_T1 = 1-4\nTheta_T2 = 5-8\nCOV = Theta_T1*Theta_T2, Theta_T2*Theta_T2\nMEAN = Theta_T2'
  a_all <- sprintf("(%d, a1, %d, a2)", 1:M, 1:M + M)
  nc <- apply(y, 2, function(x) length(unique(x)))
  thr_all <- lapply(1:M, function(i) sprintf("(%d, %d, d%d)", i, i + M,
                                             seq_len(min(nc[i], nc[i + M]) - 1)))
  tt <- grmtree:::.rs_type_tests(y, a_all, thr_all, M, flagged_items = c(2, 3),
                                 model_str = m)
  expect_lt(tt$p_slope[tt$Item == 2], 0.01)
  expect_lt(tt$p_location[tt$Item == 3], 0.01)
  expect_equal(tt$df_location[tt$Item == 3], min(nc[3], nc[3 + M]) - 1)
})
