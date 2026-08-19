# grmtree 0.2.1

## Bug fixes

* `grmforest()` now runs reliably in parallel (`n_cores > 1`) on all platforms,
  including PSOCK/Windows workers, and results are identical regardless of the
  number of cores. Out-of-bag membership is stored as row indices, and forests
  can be grown in chunks and combined with `c()`.
* `grmforest()` coerces character partitioning variables to factors, avoiding
  tree failures on such covariates.
* `varimp()` now scores out-of-bag respondents with the marginal (latent-
  integrated) log-likelihood using the correct GRM parameterization, and
  averages paired within-tree permutation differences.
* `grmfit()` no longer computes standard errors during fitting, which removes a
  `chol()` failure on hard resamples and speeds up tree/forest construction;
  results are unchanged.
* Adjust `longitudinal_grmtree()` to restrict the split test to ITEM parameters 
  (measurement invariance) and not include the structural parameters.
* Corrected item-level testing in `rs_characterize()`. Item-level tests now run
  only in nodes with omnibus response shift detected after the across-node
  adjustment (`RS_detected`), instead of the unadjusted p-value, so no item rows
  are produced for nodes that are not significant family-wise. The shift type is
  now read from the estimated item-parameter changes (occasion-general
  discrimination and thresholds) rather than a magnitude cut-point: a
  significant item is always classified as recalibration, reprioritization, or
  both, the "Significant (small effect)" label is removed, and the effect-size
  magnitude is reported separately in a new `RS_magnitude` column.
* Corrected the response-shift visualizations `plot_rs_tree()` and
  `plot_rs_heatmap()`. Both now display p-values in fixed notation (e.g.
  "p < 0.001") rather than scientific notation, label the latent parameters as
  "Mean shift (T2)" and "Cor(T1, T2)", spell out the response-shift types in
  full, and restrict the RS-type legend to Recalibration, Reprioritization,
  Both, and None (the effect-size annotation is no longer shown as a shift
  type).

## Improvements
* `varimp()` scores out-of-bag respondents with the marginal (latent-integrated)
  log-likelihood and averages paired within-tree permutation differences;
  out-of-bag membership is stored as row indices and forests can be combined
  with `c()`.
  
  
# grmtree 0.2.0 (2026)

## New features

* Added `longitudinal_grmtree()` for response shift (RS) detection in
  patient-reported outcome measures (PROMs) measured at two time points. The
  method embeds a constrained two-factor longitudinal graded response model
  within model-based recursive partitioning to identify patient subgroups whose
  longitudinal measurement model differs.
* Added `rs_characterize()`, with a `print()` method, for Phase 2 response shift
  characterization. Within each terminal node it performs an omnibus likelihood
  ratio test (constrained vs unconstrained model) and, where significant,
  item-level tests that classify each item as recalibration, reprioritization,
  or both. Supports hierarchical p-value correction both across nodes
  (`global_p_adjust`) and within nodes (`p_adjust`).
* Added `prepare_longitudinal_data()` to construct the wide-format response
  matrix required by `longitudinal_grmtree()` from separate baseline and
  follow-up item columns.
* Added longitudinal parameter-extraction functions:
  `threshpar_longitudinal_grmtree()`, `discrpar_longitudinal_grmtree()`,
  `itempar_longitudinal_grmtree()`, `fscores_longitudinal_grmtree()`, and
  `latentpar_longitudinal_grmtree()`.
* Added a `plot()` method for `longitudinal_grmtree` objects (threshold region
  plots showing the unique items), and two response shift visualizations,
  `plot_rs_tree()` and `plot_rs_heatmap()`.
* `generate_node_scores_dataset()` now supports both cross-sectional
  (`grmtree`) and longitudinal (`longitudinal_grmtree`) trees, and merges node
  assignments and factor scores back onto the original data frame.
* Added the synthetic `grmtree_long_data` dataset (longitudinal MOS-SS emotional
  domain, two time points) for examples, tests, and the new vignette.
* Added the vignette "Response Shift Detection with the Longitudinal GRMTree".

## Bug fixes

* Corrected the post-hoc multiple-comparison adjustment methods in
  `grmtree.control()` (Holm, Benjamini-Hochberg, Benjamini-Yekutieli, Hochberg,
  and Hommel). The previous implementation reduced each node to its minimum
  p-value before applying the adjustment, which collapsed the within-node
  multiplicity across covariates. The internal `.adjust_and_prune_tree()` now
  collects all covariate-by-node p-values, applies the adjustment globally, and
  then prunes non-significant nodes. This properly accounts for both within-node
  (multiple covariates) and across-node (multiple splits) multiplicity.

# grmtree 0.1.0 (2025)

This is the first official release of the `grmtree` package, providing methods
for fitting and analyzing graded response model (GRM) trees and forests.

* Introduced `grmtree()` for fitting tree-based graded response models.
* Added `grmforest()` for building forests of GRM trees.
* Implemented custom `print()` and `plot()` methods for GRM tree/forest objects.
* Enabled node-level GRM parameter extraction (thresholds, loadings,
  discrimination).
* Added support for extracting and analyzing factor scores per terminal node.
* Included helper functions for working with terminal nodes and fitted models.
* Vignette walkthrough provided using a sample data.
