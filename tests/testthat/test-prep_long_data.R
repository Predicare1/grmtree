library(testthat)
library(mirt)


# ==============================================================================
# prepare_longitudinal_data()  (fast, no fitting)
# ==============================================================================

test_that("prepare_longitudinal_data builds the wide matrix correctly", {
  df <- make_tiny_long()
  it1 <- paste0("I", 1:4, "_T1")
  it2 <- paste0("I", 1:4, "_T2")

  ld <- prepare_longitudinal_data(df, items_t1 = it1, items_t2 = it2,
                                  covariates = c("grp", "age"))

  expect_s3_class(ld, "data.frame")
  expect_true("resp_wide" %in% names(ld))
  expect_true(is.matrix(ld$resp_wide))
  expect_equal(ncol(ld$resp_wide), 8)          # 2 * n_items
  expect_true(all(c("grp", "age") %in% names(ld)))
})

test_that("prepare_longitudinal_data validates inputs", {
  df <- make_tiny_long()
  it1 <- paste0("I", 1:4, "_T1")
  it2 <- paste0("I", 1:4, "_T2")

  # Unequal item vector lengths
  expect_error(
    prepare_longitudinal_data(df, items_t1 = it1[1:3], items_t2 = it2),
    "same length"
  )
  # Missing columns
  expect_error(
    prepare_longitudinal_data(df, items_t1 = c(it1, "nope"),
                              items_t2 = c(it2, "nope2")),
    "Columns not found"
  )
})

test_that("prepare_longitudinal_data drops incomplete response rows", {
  df <- make_tiny_long()
  it1 <- paste0("I", 1:4, "_T1")
  it2 <- paste0("I", 1:4, "_T2")
  df$I1_T1[1:5] <- NA

  expect_message(
    ld <- prepare_longitudinal_data(df, items_t1 = it1, items_t2 = it2),
    "incomplete rows"
  )
  expect_equal(nrow(ld), nrow(df) - 5)
})
