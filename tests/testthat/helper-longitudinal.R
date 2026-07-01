# ------------------------------------------------------------------------------
# Helper: tiny synthetic longitudinal dataset (fast; no real data needed)
# ------------------------------------------------------------------------------
make_tiny_long <- function(n = 250, n_items = 4, seed = 1) {
  set.seed(seed)
  a  <- runif(n_items, 1.5, 2.5)
  b  <- t(replicate(n_items, sort(rnorm(4))))
  d  <- -a * b
  th1 <- rnorm(n)
  th2 <- 0.58 * th1 + sqrt(1 - 0.58^2) * rnorm(n)
  r1 <- mirt::simdata(a = matrix(a, ncol = 1), d = d,
                      Theta = matrix(th1, ncol = 1), itemtype = "graded") + 1L
  r2 <- mirt::simdata(a = matrix(a, ncol = 1), d = d,
                      Theta = matrix(th2, ncol = 1), itemtype = "graded") + 1L
  df <- data.frame(r1, r2)
  names(df) <- c(paste0("I", 1:n_items, "_T1"), paste0("I", 1:n_items, "_T2"))
  df$grp <- factor(sample(c("A", "B"), n, replace = TRUE))
  df$age <- round(rnorm(n, 60, 8))
  df
}
