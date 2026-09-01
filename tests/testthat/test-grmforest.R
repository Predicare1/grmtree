library(testthat)
library(mirt)
library(hlt)

# Small helper: fit a modest forest on the asti data (reused across tests)
make_asti <- function() {
  data("asti", package = "hlt")
  asti$resp <- data.matrix(asti[, 1:4])
  asti
}

## ---- grmforest.control() -------------------------------------------------
test_that("grmforest.control creates a valid control object", {
  skip_on_cran()
  ctrl <- grmforest.control()
  expect_s3_class(ctrl, "grmforest_control")
  expect_equal(ctrl$n_tree, 100L)
  expect_equal(ctrl$sampling, "subsample")           # default

  ctrl <- grmforest.control(n_tree = 5, sampling = "bootstrap",
                            sample_fraction = 0.5, mtry = 2, seed = 123)
  expect_equal(ctrl$n_tree, 5L)
  expect_equal(ctrl$sampling, "bootstrap")
  expect_equal(ctrl$mtry, 2L)
  expect_equal(ctrl$seed, 123L)
})

test_that("grmforest.control rejects invalid arguments", {
  expect_error(grmforest.control(n_tree = 0), "at least 1")
  expect_error(grmforest.control(sampling = "invalid"))         # match.arg
  expect_error(grmforest.control(sample_fraction = 1.1), "between 0 and 1")
  expect_error(grmforest.control(mtry = 0), "positive integer")
  expect_error(grmforest.control(n_cores = 0), "positive integer")
})

## ---- grmforest() ---------------------------------------------------------
test_that("grmforest fits and stores out-of-bag indices", {
  skip_if_not_installed("mirt")
  skip_if_not_installed("hlt")
  asti <- make_asti()

  suppressWarnings({
    forest <- grmforest(resp ~ gender + group, data = asti,
                        control = grmforest.control(n_tree = 3, seed = 1))
  })
  expect_s3_class(forest, "grmforest")
  expect_gt(length(forest$trees), 0)
  # out-of-bag membership is stored as row indices, one vector per tree
  expect_equal(length(forest$oob_indices), length(forest$trees))
  expect_equal(length(forest$in_indices),  length(forest$trees))
  expect_true(all(vapply(forest$oob_indices, is.numeric, logical(1))))
})

test_that("grmforest reproduces across cores (indices identical)", {
  skip_on_cran()
  skip_if_not_installed("mirt")
  skip_if_not_installed("hlt")
  asti <- make_asti()

  suppressWarnings({
    f1 <- grmforest(resp ~ gender + group, data = asti,
                    control = grmforest.control(n_tree = 3, seed = 42, n_cores = 1))
    f2 <- grmforest(resp ~ gender + group, data = asti,
                    control = grmforest.control(n_tree = 3, seed = 42, n_cores = 2))
  })
  expect_identical(f1$in_indices, f2$in_indices)
})

test_that("grmforest validates its inputs", {
  skip_on_cran()
  skip_if_not_installed("mirt")
  skip_if_not_installed("hlt")
  asti <- make_asti()

  # response must be a matrix of item responses
  expect_error(grmforest(group ~ gender, data = asti),
               "Response variable must be a matrix of item responses")
  # too few rows
  expect_error(grmforest(resp ~ gender, data = asti[1:5, ]),
               "Insufficient data")
  # control must come from grmforest.control()
  bad_ctrl <- grmforest.control(); class(bad_ctrl) <- "not_control"
  expect_error(
    grmforest(resp ~ gender + group, data = asti, control = bad_ctrl),
    "must be created by grmforest.control"
  )
})

## ---- print.grmforest -----------------------------------------------------
test_that("print.grmforest works", {
  skip_on_cran()
  skip_if_not_installed("mirt")
  skip_if_not_installed("hlt")
  asti <- make_asti()
  suppressWarnings({
    forest <- grmforest(resp ~ gender + group, data = asti,
                        control = grmforest.control(n_tree = 2, seed = 123))
  })
  expect_output(print(forest), "GRM Forest with")
})

## ---- c.grmforest ---------------------------------------------------------
test_that("c.grmforest combines forests grown on the same data", {
  skip_on_cran()
  skip_if_not_installed("mirt")
  skip_if_not_installed("hlt")
  asti <- make_asti()
  suppressWarnings({
    f1 <- grmforest(resp ~ gender + group, data = asti,
                    control = grmforest.control(n_tree = 2, seed = 1))
    f2 <- grmforest(resp ~ gender + group, data = asti,
                    control = grmforest.control(n_tree = 2, seed = 2))
  })
  big <- c(f1, f2)
  expect_s3_class(big, "grmforest")
  expect_equal(length(big$trees), length(f1$trees) + length(f2$trees))
  expect_equal(length(big$oob_indices), length(big$trees))
})

## ---- longitudinal forest via tree_fun ------------------------------------
test_that("grmforest grows a longitudinal forest via tree_fun", {
  skip_on_cran()
  skip_if_not_installed("mirt")
  skip_if_not_installed("partykit")

  df  <- make_tiny_long(n = 250, n_items = 4, seed = 42)
  it1 <- paste0("I", 1:4, "_T1")
  it2 <- paste0("I", 1:4, "_T2")
  ld  <- prepare_longitudinal_data(df, items_t1 = it1, items_t2 = it2,
                                   covariates = c("grp", "age"))

  suppressWarnings({
    f <- grmforest(resp_wide ~ grp + age, data = ld,
                   control = grmforest.control(n_tree = 2, seed = 1,
                                               control = grmtree.control(minbucket = 80)),
                   tree_fun = longitudinal_grmtree, tree_args = list(n_items = 4))
  })
  expect_s3_class(f, "grmforest")
  expect_gt(length(f$trees), 0)
  expect_s3_class(f$trees[[1]], "longitudinal_grmtree")
  expect_equal(length(f$oob_indices), length(f$trees))
})
