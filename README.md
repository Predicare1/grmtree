# grmtree: Recursive Partitioning for Graded Response Models

<!-- badges: start -->

[![R-CMD-check](https://github.com/Predicare1/grmtree/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Predicare1/grmtree/actions/workflows/R-CMD-check.yaml) [![CRAN version](https://www.r-pkg.org/badges/version/grmtree)](https://cran.r-project.org/package=grmtree) [![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/grmtree)](https://cran.r-project.org/package=grmtree) [![License: GPL-3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://cran.r-project.org/web/licenses/GPL-3) [![Code size](https://img.shields.io/github/languages/code-size/Predicare1/grmtree.svg)](https://github.com/Predicare1/grmtree)

<!-- badges: end -->

## Overview

The `grmtree` package implements advanced tree-based recursive partitioning methods for Graded Response Models (GRM), providing a powerful framework for detecting and analyzing differential item functioning (DIF) in polytomous items from patient-reported outcome measures (PROMs) and other psychological assessments. DIF occurs when individuals with the same underlying latent trait (e.g., health status, quality of life, or psychological attribute) respond differently to assessment items based on extraneous characteristics such as age, gender, education level, or clinical subgroups. This measurement bias can compromise the validity and fairness of assessments across diverse populations.

The GRMTree methodology combines the psychometric rigor of item response theory with the interpretability of decision trees to:

-   Identify specific covariates associated with DIF
-   Detect complex interaction effects between patient characteristics
-   Provide visual representations of how item functioning varies across population subgroups
-   Support the development of equitable assessment instruments

The package also implements the **Longitudinal GRMTree** for detecting **response shift** in PROMs measured at two time points, embedding a constrained two-factor longitudinal GRM within recursive partitioning and characterizing recalibration and reprioritization within data-driven subgroups.

## Key Features

-   **DIF Detection**: Advanced algorithms for detecting differential item functioning in polytomous items using model-based recursive partitioning
-   **Response Shift Detection**: Longitudinal GRMTree for detecting and characterizing response shift (recalibration, reprioritization) in PROMs measured at two time points
-   **Visual Analytics**: Specialized plotting functions for visualizing threshold parameters, discrimination parameters, factor score distributions, and response shift across terminal nodes
-   **Parameter Extraction**: Comprehensive methods for extracting and analyzing item parameters (thresholds, discrimination), factor scores, and node-specific characteristics
-   **Ensemble Methods**: GRM Forests for both cross-sectional and longitudinal trees, with permutation variable importance
-   **Multiple Testing Corrections**: Flexible p-value adjustment methods (Bonferroni, Holm, Benjamini-Hochberg, etc.) to control Type I error inflation
-   **Seamless Integration**: Full compatibility with the `mirt` package for GRM estimation and the `partykit` ecosystem for tree visualization

This package is particularly valuable for researchers, psychometricians, and health outcomes specialists who require robust methods for ensuring measurement invariance and equity in their assessment instruments across diverse populations.

## Installation

Install from CRAN (when available):

```{r}
install.packages("grmtree")
```

Install the development version from GitHub

```{r}
# Install devtools if not already installed
install.packages("devtools")

# Install the grmtree
devtools::install_github("Predicare1/grmtree")

# Install with vignettes (takes longer but includes vignettes)
devtools::install_github("Predicare1/grmtree", build_vignettes = TRUE)
```

## Quick Start

```{r}
# Load the package
library(grmtree)
library(dplyr)

# Load the data
data("grmtree_data") # Sample dataset included with package

# Prepare the data
resp.data <- grmtree_data %>% 
  mutate_at(vars(starts_with("MOS")), as.ordered) %>% 
  mutate_at(vars(c(sex, Education)), as.factor) 

# Create response as outcomes
resp.data$resp <- data.matrix(resp.data[, 1:8])

## GRMTree control parameters with Benjamini-Hochberg
grm_control <- grmtree.control(
  minbucket = 350,
  p_adjust = "BH", alpha = 0.05)

# Fit a GRM tree
tree <- grmtree(resp ~ sex + age + Education,
                       data = resp.data,
                       control = grm_control)

# Print the GRMTree model
print(tree)

# Visualize the tree
plot(tree) # Default regions plot
plot(tree, type = "profile") # Parameter profiles
plot(tree, type = "histogram") # Factor score distributions

# Extract parameters
threshpar_grmtree(tree) # Threshold parameters
discrpar_grmtree(tree) # Discrimination parameters
```

## Longitudinal GRMTree: Response Shift Detection

The Longitudinal GRMTree detects response shift in PROMs measured at two time
points, using a two-phase approach: Phase 1 identifies patient subgroups whose
longitudinal measurement model differs, and Phase 2 tests for response shift
within each subgroup and classifies the affected items.

```{r}
# Load the synthetic longitudinal dataset
data("grmtree_long_data")

# Prepare the wide-format response matrix
items_t1 <- c("MOS_Listen", "MOS_Info", "MOS_Advice_Crisis", "MOS_Confide",
              "MOS_Advice_Want", "MOS_Fears", "MOS_Personal", "MOS_Understand")

ld <- prepare_longitudinal_data(
  data       = grmtree_long_data,
  items_t1   = items_t1,
  items_t2   = paste0(items_t1, "_year1"),
  covariates = c("sex", "age", "residency", "job",
                 "education", "comorbidity_count", "ever_smoker")
)

# Phase 1: fit the tree to identify subgroups
tree <- longitudinal_grmtree(
  resp_wide ~ sex + age + residency + job +
    education + comorbidity_count + ever_smoker,
  data = ld, n_items = 8,
  control = grmtree.control(minbucket = 200)
)
print(tree)

# Phase 2: characterize response shift within each subgroup
rs <- rs_characterize(tree, p_adjust = "fdr", global_p_adjust = "bonferroni")
print(rs)

# Visualize response shift
plot_rs_tree(tree, rs)
plot_rs_heatmap(rs)
```

## Vignettes

Detailed interactive tutorials are available:

```{r}
browseVignettes("grmtree")
```

1.  Getting Started with the grmtree Package: Basic GRM tree implementation
2.  GRM Forests for Robust DIF Detection: Ensemble methods for robust DIF detection
3.  Response Shift Detection with the Longitudinal GRMTree: Two-phase detection and characterization of response shift in longitudinal PROMs

## GRM Forests Example

```{r}
# Fit a forest with 50 trees
forest <- grmforest(resp ~ sex + age + Education,
                       data = resp.data,
                   control = grmforest.control(control = grm_control, 
                   n_tree = 50, 
                   n_cores = 2))

# Variable importance
vim <- varimp(forest)
print(vim)
plot(vim)

# Examine individual trees
plot(forest$trees[[1]]) # First tree
```

## Longitudinal GRM Forest

The same forest engine screens many covariates for a longitudinal tree — just
pass `tree_fun = longitudinal_grmtree`:

```{r}
forest <- grmforest(
  resp_wide ~ sex + age + residency + job +
    education + comorbidity_count + ever_smoker,
  data      = ld,
  control   = grmforest.control(n_tree = 50, n_cores = 2,
                control = grmtree.control(minbucket = 200)),
  tree_fun  = longitudinal_grmtree,
  tree_args = list(n_items = 8)
)
varimp(forest)
```

# References

## Methodological Foundations

-   Samejima, F. (1969). Estimation of latent ability using a response pattern of graded scores. Psychometrika Monograph Supplement, 34, 100-114.
-   Strobl, C., Kopf, J., & Zeileis, A. (2015). Rasch trees: A new method for detecting differential item functioning in the Rasch model. Psychometrika, 80(2), 289-316.
-   Komboz, B., Strobl, C., & Zeileis, A. (2018). Tree-based global model tests for polytomous Rasch models. Educational and psychological measurement, 78(1), 128–166. doi.org/10.1177/0013164416664394.
-   Oort, F. J. (2005). Using structural equation modeling to detect response shifts and true change. Quality of Life Research, 14(3), 587–598. <https://doi.org/10.1007/s11136-004-0830-y>.
-   Arimoro, O. I., Lix, L. M., Patten, S. B., Sawatzky, R., Sebille, V., Liu, J., Wiebe, S., Josephson, C. B., & Sajobi, T. T. (2025). Tree-based latent variable model for assessing differential item functioning in patient-reported outcome measures: a simulation study. Quality of life research : an international journal of quality of life aspects of treatment, care and rehabilitation, 10.1007/s11136-025-04018-6. Advance online publication. <https://doi.org/10.1007/s11136-025-04018-6>.

## Applied Examples

-   Arimoro, O. I., Josephson, C. B., James, M. T., Patten, S. B., Wiebe, S., Lix, L. M., & Sajobi, T. T. (2024). Screening for depression in patients with epilepsy: same questions but different meaning to different patients. Quality of life research : an international journal of quality of life aspects of treatment, care and rehabilitation, 33(12), 3409–3419. <https://doi.org/10.1007/s11136-024-03782-1>.

# Authors

Olayinka Imisioluwa Arimoro ([olayinka.arimoro\@ucalgary.ca](mailto:olayinka.arimoro@ucalgary.ca){.email}), Lisa M. Lix, Tolulope T. Sajobi

# Contributing

Contributions are welcome! Please submit issues and pull requests via GitHub: <https://github.com/Predicare1/grmtree/issues>
