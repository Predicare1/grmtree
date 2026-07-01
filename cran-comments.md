## Submission of version 0.2.0

This is a feature update to the CRAN package `grmtree`.

### New features

* Added the Longitudinal GRMTree for detecting response shift in
  patient-reported outcome measures measured at two time points. New
  user-facing functions include `longitudinal_grmtree()`, `rs_characterize()`
  (with a `print()` method), `prepare_longitudinal_data()`, the longitudinal
  parameter-extraction functions (`threshpar_`, `discrpar_`, `itempar_`,
  `fscores_`, and `latentpar_longitudinal_grmtree()`), and the response shift
  visualizations `plot_rs_tree()` and `plot_rs_heatmap()`.
* Added a new synthetic dataset, `grmtree_long_data`, and a new vignette,
  "Response Shift Detection with the Longitudinal GRMTree".

### Bug fixes

* Corrected the post-hoc multiple-comparison adjustment methods in
  `grmtree.control()` (Holm, Benjamini-Hochberg, Benjamini-Yekutieli, Hochberg,
  Hommel). The previous implementation applied the adjustment to per-node
  minimum p-values, which collapsed the within-node multiplicity across
  covariates; the adjustment is now applied globally across all
  covariate-by-node tests before pruning.

### Notes on check time

The computationally intensive longitudinal model fits are guarded with
`skip_on_cran()` in the test files, so they run in local and continuous
integration testing but are skipped on CRAN. All computationally intensive
vignette code chunks use `eval = FALSE` and display pre-computed example output.
These measures keep the CRAN check time under 10 minutes while still exercising
and demonstrating the full functionality.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local Windows 11, R 4.5.1
* GitHub Actions (ubuntu-latest, windows-latest, macos-latest)
* win-builder (R-devel)

---

## Previous submission (version 0.1.1)

This was a resubmission that reduced the CRAN check time to < 10 minutes by
reducing the number of trees in the test files of `grmforest` and `varimp`.

Earlier fixes across the 0.1.x submissions included:

* Added methodological references with DOI to the Description field.
* Replaced all `\dontrun{}` with `\donttest{}` in documentation examples.
* Fixed vignette timeout by setting `eval = FALSE` for computationally intensive
  code chunks in the GRMForest vignette.
* Reduced example complexity in `varimp()` documentation.
* Fixed `grmfit()` by removing a problematic `tryCatch` warning handler that
  caused "no 'restart' 'muffleWarning' found" errors during tree fitting.
* Updated the implementation of the multiple-comparison adjustment methods in
  `grmtree.control()` using a post-hoc pruning approach, with internal helpers
  `.adjust_and_prune_tree()`, `.prune_single_node()`, and `.get_parent_path()`.
* Fixed test failures in `test-fscores.R` by handling convergence warnings from
  `mirt::fscores()` with `suppressWarnings()`.
* Fixed a URL timeout by updating the GNU license URL in README.md, removed the
  CRAN badge URL (404 for new packages), and fixed the DOI link format.
* Resolved tidyselect deprecation warnings by updating `.data$var` to `"var"`.
