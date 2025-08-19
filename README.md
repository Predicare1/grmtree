
# grmtree: Recursive Partitioning for Graded Response Models

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/grmtree)](https://CRAN.R-project.org/package=grmtree)
[![R-CMD-check](https://github.com/Predicare1/grmtree/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Predicare1/grmtree/actions/workflows/R-CMD-check.yaml)
[![License:
GPL-3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Code
size](https://img.shields.io/github/languages/code-size/Predicare1/grmtree.svg)](https://github.com/Predicare1/grmtree)

<!-- badges: end -->

## Overview

The `grmtree` package implements recursive partitioning methods for
Graded Response Models (GRM), enabling:

-   Detection of differential item functioning (DIF) in polytomous items
-   Identification of heterogeneous subgroups based on item response
    patterns
-   Visualization of threshold parameters across terminal nodes
-   Ensemble methods via GRM Forests for robust variable importance

Key features: - Specialized plotting functions for GRM trees - Methods
for extracting threshold and discrimination parameters - Integration
with `mirt` package for GRM estimation - Parallel computation support
for forests

## Installation

Install from CRAN (when available):

```{r}
install.packages("grmtree")
```

Install the development version from GitHub

```{r}
# install.packages("devtools")
devtools::install_github("Predicare1/grmtree")
```

## Quick Start

```{r}
library(grmtree)

# Fit a GRM tree
data(mos_data) # Sample dataset included with package
tree <- grmtree(items ~ age + sex + education, data = mos_data)

# Visualize the tree
plot(tree) # Default regions plot
plot(tree, type = "profile") # Parameter profiles
plot(tree, type = "histogram") # Factor score distributions

# Extract parameters
threshpar_grmtree(tree) # Threshold parameters
discrpar_grmtree(tree) # Discrimination parameters
```

## Vignettes

Detailed tutorials are available:

```{r}
browseVignettes("grmtree")
```

1.  Getting Started: Basic GRM tree implementation

2.  GRM Forests: Ensemble methods for robust DIF detection

## GRM Forests Example

```{r}
# Fit a forest with 100 trees
forest <- grmforest(items ~ age + sex + education, 
                   data = mos_data,
                   ctrl = grmforest.control(n_tree = 100))

# Variable importance
vim <- varimp(forest)
plot(vim)

# Examine individual trees
plot(forest$trees[[1]]) # First tree
```

# References

## Methodological Foundations

-   Samejima, F. (1969). Estimation of latent ability using a response
    pattern of graded scores. Psychometrika Monograph Supplement, 34,
    100-114.

-   Strobl, C., Kopf, J., & Zeileis, A. (2015). Rasch trees: A new
    method for detecting differential item functioning in the Rasch
    model. Psychometrika, 80(2), 289-316.

-   Komboz, B., Strobl, C., & Zeileis, A. (2018). Tree-based global
    model tests for polytomous Rasch nodels. Educational and
    psychological measurement, 78(1), 128–166.
    <https://doi.org/10.1177/0013164416664394>.

-   Arimoro, O. I., Lix, L. M., Patten, S. B., Sawatzky, R., Sebille,
    V., Liu, J., Wiebe, S., Josephson, C. B., & Sajobi, T. T. (2025).
    Tree-based latent variable model for assessing differential item
    functioning in patient-reported outcome measures: a simulation
    study. Quality of life research : an international journal of
    quality of life aspects of treatment, care and rehabilitation,
    10.1007/s11136-025-04018-6. Advance online publication.
    <https://doi.org/10.1007/s11136-025-04018-6>.

## Applied Examples

-   Arimoro, O. I., Josephson, C. B., James, M. T., Patten, S. B.,
    Wiebe, S., Lix, L. M., & Sajobi, T. T. (2024). Screening for
    depression in patients with epilepsy: same questions but different
    meaning to different patients. Quality of life research : an
    international journal of quality of life aspects of treatment, care
    and rehabilitation, 33(12), 3409–3419.
    <https://doi.org/10.1007/s11136-024-03782-1>.

# Contributing

Contributions are welcome! Please submit issues and pull requests via
GitHub: <https://github.com/Predicare1/grmtree/issues>
