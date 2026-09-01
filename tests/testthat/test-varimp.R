library(testthat)
library(mirt)
library(hlt)

make_forest <- function(n_tree = 4, seed = 1) {
  data("asti", package = "hlt")
  asti$resp <- data.matrix(asti[, 1:4])
  suppressWarnings(
    grmforest(resp ~ gender + group, data = asti,
              control = grmforest.control(n_tree = n_tree, seed = seed))
  )
}

## ---- basic behaviour -----------------------------------------------------
test_that("varimp returns a named varimp vector", {
  skip_on_cran()
  skip_if_not_installed("mirt")
  skip_if_not_installed("hlt")
  forest <- make_forest()

  imp <- varimp(forest, seed = 123)
  expect_s3_class(imp, "varimp")
  expect_type(imp, "double")
  expect_named(imp)
  expect_length(imp, 2L)                       # gender + group
})

test_that("varimp is reproducible for a fixed seed", {
  skip_on_cran()
  skip_if_not_installed("mirt")
  skip_if_not_installed("hlt")
  forest <- make_forest()
  expect_equal(varimp(forest, seed = 123), varimp(forest, seed = 123))
})

test_that("varimp verbose reports progress", {
  skip_on_cran()
  skip_if_not_installed("mirt")
  skip_if_not_installed("hlt")
  forest <- make_forest()
  expect_message(varimp(forest, verbose = TRUE), "Evaluating")
})

## ---- input validation ----------------------------------------------------
test_that("varimp validates its inputs", {
  skip_on_cran()
  skip_if_not_installed("mirt")
  skip_if_not_installed("hlt")
  forest <- make_forest()

  expect_error(varimp(list()), "must be a grmforest object")
  expect_error(varimp(forest, method = "invalid"),
               "Only permutation importance is implemented")
})

test_that("varimp rejects legacy forests without oob_indices", {
  skip_on_cran()
  skip_if_not_installed("mirt")
  skip_if_not_installed("hlt")
  data("asti", package = "hlt")
  asti$resp <- data.matrix(asti[, 1:4])

  legacy <- structure(
    list(trees = list(NULL), oob_samples = list(asti[1:10, ]),
         formula = resp ~ gender + group, data = asti),
    class = "grmforest"
  )
  expect_error(varimp(legacy), "older version")
})

test_that("varimp returns zeros when no tree yields a usable OOB likelihood", {
  skip_on_cran()
  skip_if_not_installed("mirt")
  skip_if_not_installed("hlt")
  data("asti", package = "hlt")
  asti$resp <- data.matrix(asti[, 1:4])

  bad <- structure(
    list(trees = list(NULL, NULL),
         oob_indices = list(1:20, 21:40),
         in_indices  = list(41:60, 61:80),
         formula = resp ~ gender + group, data = asti,
         control = grmforest.control(), call = NULL),
    class = "grmforest"
  )
  suppressWarnings(
    expect_warning(res <- varimp(bad), "No trees produced")
  )
  expect_s3_class(res, "varimp")
  expect_named(res)
  expect_true(all(res == 0))
})

## ---- print / plot --------------------------------------------------------
test_that("print.varimp works", {
  skip_on_cran()
  skip_if_not_installed("mirt")
  skip_if_not_installed("hlt")
  imp <- varimp(make_forest(), seed = 1)
  expect_output(print(imp), "variable importance")
})

test_that("plot.varimp runs", {
  skip_on_cran()
  skip_if_not_installed("mirt")
  skip_if_not_installed("hlt")
  imp <- varimp(make_forest(), seed = 1)

  tmp <- tempfile(fileext = ".pdf"); grDevices::pdf(tmp)
  on.exit({ grDevices::dev.off(); unlink(tmp) }, add = TRUE)

  expect_error(plot(imp, use_ggplot = FALSE), NA)          # no error
  expect_error(plot(imp, use_ggplot = FALSE, top_n = 1), NA)
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    expect_error(print(plot(imp, use_ggplot = TRUE)), NA)
  }
})

## ---- varimp for longitudinal grmforest --------------------------------------------------------
test_that("varimp scores a longitudinal forest", {
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
                   control = grmforest.control(n_tree = 3, seed = 1,
                                               control = grmtree.control(minbucket = 80)),
                   tree_fun = longitudinal_grmtree, tree_args = list(n_items = 4))
  })
  imp <- varimp(f, seed = 1)
  expect_s3_class(imp, "varimp")
  expect_named(imp)
  expect_length(imp, 2L)                      # sex + age + residency
})
