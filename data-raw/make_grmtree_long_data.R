################################################################################
##
## make_grmtree_long_data.R
##
## Generates a SYNTHETIC longitudinal MOS-SS Emotional Domain dataset for the
## grmtree package. The data are simulated from a two-factor longitudinal
## graded response model (GRM) so that:
##
##   (1) There is a clear age-driven subgroup split in Phase 1
##       (longitudinal_grmtree), and
##   (2) The older subgroup exhibits item-level response shift on two items
##       (Info = reprioritization/discrimination change;
##        Fears = recalibration/threshold change),
##
## so that longitudinal_grmtree() + rs_characterize() produce a real,
## interpretable result in examples, tests, and the vignette.
##
## NOTE: This is entirely synthetic. It contains no real patient data.
## Item naming matches the cross-sectional grmtree_data for consistency.
##
## Author: Olayinka Arimoro
## Date: 2026
##
################################################################################

set.seed(20260630)

library(mirt)      # simdata
library(dplyr)

# ------------------------------------------------------------------------------
# 1. Design
# ------------------------------------------------------------------------------
N        <- 1500                 # number of persons
n_items  <- 8                    # MOS-SS emotional items
K        <- 5                    # response categories (1-5)

item_names <- c("MOS_Listen", "MOS_Info", "MOS_Advice_Crisis", "MOS_Confide",
                "MOS_Advice_Want", "MOS_Fears", "MOS_Personal", "MOS_Understand")

# ------------------------------------------------------------------------------
# 2. Covariates
# ------------------------------------------------------------------------------
# Age is the RS-moderating covariate: older patients (> ~61) will experience RS.
age <- round(rnorm(N, mean = 64, sd = 9.5))
age <- pmin(pmax(age, 35), 90)

older <- age > 61                # subgroup that will show RS

sex        <- factor(sample(c("Male", "Female"), N, replace = TRUE, prob = c(0.84, 0.16)))
residency  <- factor(sample(c("Urban", "Rural"), N, replace = TRUE, prob = c(0.8, 0.2)))
job        <- factor(sample(c("Employed", "Unemployed"), N, replace = TRUE, prob = c(0.45, 0.55)))
education  <- factor(sample(c("College/University", "Primary/High school"),
                            N, replace = TRUE, prob = c(0.6, 0.4)))
ever_smoker <- factor(sample(c("Yes", "No"), N, replace = TRUE, prob = c(0.55, 0.45)))
comorbidity_count <- pmin(rpois(N, lambda = 1.8), 5L)
bmi        <- round(rnorm(N, 28.7, 4.7), 1)

# ------------------------------------------------------------------------------
# 3. True item parameters (GRM, slope-intercept internally via simdata)
# ------------------------------------------------------------------------------
# Baseline (T1) discriminations and thresholds, loosely based on real MOS-SS.
a_T1 <- c(2.6, 2.3, 2.7, 3.4, 2.9, 3.0, 3.5, 3.1)

# Threshold matrix (n_items x (K-1)); increasing within each row
b_T1 <- rbind(
  c(-2.0, -1.3, -0.7,  0.2),   # Listen
  c(-1.9, -1.2, -0.5,  0.6),   # Info
  c(-1.8, -1.1, -0.4,  0.6),   # Advice_Crisis
  c(-1.6, -1.0, -0.5,  0.2),   # Confide
  c(-1.5, -1.0, -0.4,  0.5),   # Advice_Want
  c(-1.3, -0.8, -0.4,  0.3),   # Fears
  c(-1.5, -0.9, -0.3,  0.4),   # Personal
  c(-1.6, -0.9, -0.3,  0.4)    # Understand
)

# T2 parameters for the OLDER group: introduce response shift.
#   Item 2 (Info): discrimination change -> reprioritization
#   Item 6 (Fears): threshold shift      -> recalibration
a_T2_old <- a_T1
a_T2_old[2] <- a_T1[2] + 1.4          # Info: discrimination up (reprioritization)

b_T2_old <- b_T1
b_T2_old[6, ] <- b_T1[6, ] + 0.7      # Fears: thresholds shift up (recalibration)

# T2 parameters for the YOUNGER group: no RS (equal to T1).
a_T2_young <- a_T1
b_T2_young <- b_T1

# ------------------------------------------------------------------------------
# 4. Helper: simulate GRM responses for a set of people given params + theta
# ------------------------------------------------------------------------------
sim_grm <- function(theta, a, b) {
  # theta: numeric vector (n persons); a: n_items; b: n_items x (K-1)
  # Convert to mirt's simdata parameterization: d = -a * b
  d <- -a * b
  simdata(
    a = matrix(a, ncol = 1),
    d = d,
    Theta = matrix(theta, ncol = 1),
    itemtype = "graded"
  )
}

# ------------------------------------------------------------------------------
# 5. Latent traits with test-retest correlation
# ------------------------------------------------------------------------------
# theta_T1 ~ N(0,1); theta_T2 correlated with T1, small positive mean shift.
rho <- 0.58
mu_T2 <- 0.08

theta_T1 <- rnorm(N, 0, 1)
theta_T2 <- mu_T2 + rho * theta_T1 + sqrt(1 - rho^2) * rnorm(N, 0, 1)

# ------------------------------------------------------------------------------
# 6. Generate item responses
# ------------------------------------------------------------------------------
resp_T1 <- matrix(NA_integer_, N, n_items)
resp_T2 <- matrix(NA_integer_, N, n_items)

# T1: same params for everyone
resp_T1 <- sim_grm(theta_T1, a_T1, b_T1)

# T2: params depend on subgroup
resp_T2[older, ]  <- sim_grm(theta_T2[older],  a_T2_old,   b_T2_old)
resp_T2[!older, ] <- sim_grm(theta_T2[!older], a_T2_young, b_T2_young)

# simdata returns categories 0..K-1; shift to 1..K to match MOS coding
resp_T1 <- resp_T1 + 1L
resp_T2 <- resp_T2 + 1L

colnames(resp_T1) <- item_names
colnames(resp_T2) <- paste0(item_names, "_year1")

# ------------------------------------------------------------------------------
# 7. Assemble the data frame
# ------------------------------------------------------------------------------
grmtree_long_data <- data.frame(
  resp_T1,
  resp_T2,
  sex = sex,
  age = age,
  residency = residency,
  job = job,
  education = education,
  ever_smoker = ever_smoker,
  comorbidity_count = comorbidity_count,
  bmi = bmi,
  stringsAsFactors = FALSE
)

# reorder: all T1 items, all T2 items, then covariates
grmtree_long_data <- grmtree_long_data %>%
  dplyr::select(
    all_of(item_names),
    all_of(paste0(item_names, "_year1")),
    sex, age, residency, job, education, ever_smoker,
    comorbidity_count, bmi
  )

grmtree_long_data <- tibble::as_tibble(grmtree_long_data)

cat("Synthetic longitudinal MOS-SS data created:\n")
cat("  N =", nrow(grmtree_long_data), "rows,",
    ncol(grmtree_long_data), "columns\n")
cat("  Older (>61) subgroup n =", sum(older), "\n")
str(grmtree_long_data)

# ------------------------------------------------------------------------------
# 8. QUICK VERIFICATION (optional; requires the longitudinal functions sourced)
# ------------------------------------------------------------------------------
# library(grmtree)
# source("longitudinal_grmtree.R"); source("rs_characterize.R")
#
# ld <- prepare_longitudinal_data(
#   data = grmtree_long_data,
#   items_t1 = item_names,
#   items_t2 = paste0(item_names, "_year1"),
#   covariates = c("sex","age","residency","job","education",
#                  "ever_smoker","comorbidity_count")
# )
# tree <- longitudinal_grmtree(resp_wide ~ sex + age + residency + job +
#                                education + comorbidity_count + ever_smoker,
#                              data = ld, n_items = 8,
#                              control = grmtree.control(minbucket = 200))
# print(tree)                     # expect a split on age near 61
# rs <- rs_characterize(tree, p_adjust = "fdr", global_p_adjust = "bonferroni")
# print(rs)                       # expect RS in older node; Info / Fears flagged

# ------------------------------------------------------------------------------
# 9. Save to the package (run from the package root)
# ------------------------------------------------------------------------------
#usethis::use_data(grmtree_long_data, overwrite = TRUE)
