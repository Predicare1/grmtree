## Submission of version 0.2.1
This is a patch release of the CRAN package `grmtree` with bug fixes to the
GRM Forest functions. No user-facing API changes.

### Bug fixes
* `grmforest()` now runs correctly in parallel (`n_cores > 1`) on all
  platforms, including PSOCK/Windows workers. Worker processes now load the
  package namespace and receive the objects they need, fixing a "could not find
  function 'grmtree'" error. Results are identical regardless of the number of
  cores, since all resampling indices are drawn in the master process before
  fitting.
* `grmforest()` now coerces character partitioning variables to factors,
  fixing tree failures ("variable N is not integer") that occurred when a split
  fell on a character-typed covariate.
* `grmfit()` no longer computes standard errors during model fitting
  (`SE = FALSE`). This removes a `chol()` failure ("the leading minor ... is not
  positive") that could abort fitting on difficult resamples, and speeds up
  tree and forest construction. Fitted trees are unchanged, as the score
  functions used by the partitioning tests are identical with or without
  standard errors.

### Improvements
* `varimp()` scores out-of-bag respondents with the marginal (latent-integrated)
  log-likelihood using the correct GRM parameterization, and averages paired
  within-tree permutation differences. Out-of-bag membership is stored as row
  indices, and forests grown separately can be combined with `c()`.

### Notes on check time
Test files for `grmforest()` and `varimp()` use a small number of trees, and
all computationally intensive vignette code chunks use `eval = FALSE` with
pre-computed example output, keeping the CRAN check time under 10 minutes.

## R CMD check results
0 errors | 0 warnings | 0 notes on win-builder (R-devel).

On the local Windows machine, R CMD check reports one WARNING that
dependencies (partykit, mirt, libcoin, mvtnorm) were "built under R version
4.5.3" while running R 4.5.1; this is a local version mismatch only and does
not occur on CRAN's build systems.

## Test environments
* local Windows 11, R 4.5.1
* GitHub Actions (ubuntu-latest, windows-latest, macos-latest)
* win-builder (R-devel)

---

## Previous submission (version 0.2.0)

This was a feature update to the CRAN package `grmtree`. This update added new features 
and fixed some bugs:

* Added the Longitudinal GRMTree for detecting response shift in
  patient-reported outcome measures measured at two time points. New
  user-facing functions include `longitudinal_grmtree()`, `rs_characterize()`
  (with a `print()` method), `prepare_longitudinal_data()`, the longitudinal
  parameter-extraction functions (`threshpar_`, `discrpar_`, `itempar_`,
  `fscores_`, and `latentpar_longitudinal_grmtree()`), and the response shift
  visualizations `plot_rs_tree()` and `plot_rs_heatmap()`.
* Added a new synthetic dataset, `grmtree_long_data`, and a new vignette,
  "Response Shift Detection with the Longitudinal GRMTree".

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

0 errors | 0 warnings | 0 notes on win-builder (R-devel).

On the local Windows machine, R CMD check reports one WARNING that
dependencies (partykit, mirt, libcoin, mvtnorm) were "built under R version
4.5.3" while running R 4.5.1; this is a local version mismatch only and does
not occur on CRAN's build systems. A NOTE about being "unable to verify
current time" reflects a transient network condition and is not
package-related.

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
