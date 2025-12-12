## Resubmission

This is a resubmission. In this version I have:

### Bug Fixes

* Fixed `grmfit()` function by removing problematic `tryCatch` warning handler that caused "no 'restart' 'muffleWarning' found" errors during tree fitting

* Fixed test failures in `test-grmforest.R` and `test-varimp.R` by updating tests to not expect warnings when tree fitting succeeds

### New Features

* Updated the implementation of multiple comparison adjustment methods (Holm, Benjamini-Hochberg, Benjamini-Yekutieli, Hochberg, Hommel) in `grmtree.control()` using a post-hoc pruning approach

* Added internal helper functions (`.adjust_and_prune_tree()`, `.prune_single_node()`, `.get_parent_path()`) for p-value adjustment and tree pruning

### Previous Fixes (from prior submission)

* Fixed test failure in `test-fscores.R` by properly handling convergence warnings from `mirt::fscores()` using `suppressWarnings()`

* Fixed URL timeout issue by updating GNU license URL in README.md from `https://www.gnu.org/licenses/gpl-3.0` to `https://www.gnu.org/licenses/gpl-3.0.html`

* Removed CRAN badge URL (returns 404 for new packages)
 
* Fixed DOI link format in README.md references

* Fixed missing `dplyr::` namespace for `glimpse()` in `grmtree_data` example

* Reduced vignette build time by:

- Removing duplicate GRMTree model fit in getting-started vignette
- Reduced the number of trees to 3 in the GRMForest vignette
- Setting `eval = FALSE` for computationally intensive `varimp()` examples in GRMForest vignette

* Updated Date field in DESCRIPTION and updated DESCRIPTION with quoted technical terms

* Resolved tidyselect deprecation warnings by updating .data$var to "var"


## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a new release.

## Test environments

* local Windows 11, R 4.5.1
* GitHub Actions (ubuntu-latest, windows-latest, macos-latest)

## Notes on check time

The package vignettes includes computationally intensive functions for IRT model fitting and the GRMForest. Vignette examples have been optimized to reduce CRAN check time while still demonstrating package functionality.
