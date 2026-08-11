#' Control Parameters for GRM Forests
#'
#' Creates a control object governing how a forest of graded response model
#' trees is grown: how observations are resampled for each tree, how many
#' partitioning variables are offered at each split, how failures are handled,
#' and how the work is distributed across processor cores.
#'
#' @param n_tree Number of trees in the forest (default: 100).
#' @param sampling Resampling scheme for each tree: `"bootstrap"` (drawn with
#'   replacement) or `"subsample"` (drawn without replacement). Default
#'   `"subsample"`.
#' @param sample_fraction Fraction of the original sample drawn for each tree.
#'   If `NULL` (the default) this is set to `1` for bootstrap sampling and
#'   `0.632` for subsampling, following Strobl et al. (2007).
#' @param mtry Number of partitioning variables randomly offered as split
#'   candidates at each node. `NULL` (default) offers all variables, which
#'   grows a bagged ensemble rather than a random forest. A common choice is
#'   `ceiling(sqrt(p))` for `p` partitioning variables.
#' @param remove_dead_trees Logical. If `TRUE` (default), trees that fail to
#'   fit are dropped with a warning; if `FALSE`, a failure aborts the forest.
#' @param control Control parameters for the individual trees, created by
#'   [grmtree.control()].
#' @param n_cores Number of processor cores used to fit trees in parallel
#'   (default: 1). Results do **not** depend on this value: all resampling
#'   indices are drawn in the master process before any fitting begins, so a
#'   forest grown on 20 cores is identical to the same forest grown on one.
#' @param seed Random seed for reproducibility (default: `NULL`).
#' @param verbose Logical. Report progress while growing the forest
#'   (default: `FALSE`).
#'
#' @return A list of class `grmforest_control`.
#'
#' @section Reproducibility and parallelism:
#' Naive parallelisation of a tree ensemble can silently change results,
#' because each worker inherits or advances the random number stream
#' differently; in the worst case several workers draw identical resamples and
#' the ensemble carries far less variance than it appears to. This
#' implementation avoids the problem structurally: the full list of `n_tree`
#' resampling index vectors is generated serially under `seed` before any tree
#' is fitted, and tree fitting is a deterministic function of those indices.
#' `n_cores` therefore affects only elapsed time.
#'
#' @examples
#' # Bagged ensemble of 50 trees
#' ctrl <- grmforest.control(n_tree = 50)
#'
#' # Random-forest style, 4 candidate variables per split, 4 cores
#' ctrl <- grmforest.control(n_tree = 100, mtry = 4, n_cores = 4, seed = 123)
#'
#' @references
#' Strobl, C., Boulesteix, A.-L., Zeileis, A., & Hothorn, T. (2007). Bias in
#' random forest variable importance measures. \emph{BMC Bioinformatics}, 8, 25.
#'
#' @seealso \code{\link{grmtree.control}} creates a control object for
#' `grmtree`, \code{\link{plot.grmtree}} creates plot for the `grmtree` object,
#' \code{\link{grmforest}} for GRM Forests,
#'
#' @export
grmforest.control <- function(n_tree = 100,
                              sampling = c("subsample", "bootstrap"),
                              sample_fraction = NULL,
                              mtry = NULL,
                              remove_dead_trees = TRUE,
                              control = grmtree.control(),
                              n_cores = 1,
                              seed = NULL,
                              verbose = FALSE) {

  if (!is.numeric(n_tree) || length(n_tree) != 1L) {
    stop("'n_tree' must be a single numeric value")
  }
  if (n_tree < 1) stop("'n_tree' must be at least 1")

  sampling <- match.arg(sampling)

  if (is.null(sample_fraction)) {
    sample_fraction <- if (sampling == "bootstrap") 1 else 0.632
  }
  if (!is.numeric(sample_fraction) || length(sample_fraction) != 1L) {
    stop("'sample_fraction' must be a single numeric value")
  }
  if (sample_fraction <= 0 || sample_fraction > 1) {
    stop("'sample_fraction' must be between 0 and 1")
  }

  if (!is.null(mtry) && (!is.numeric(mtry) || length(mtry) != 1L || mtry < 1)) {
    stop("'mtry' must be NULL or a positive integer")
  }
  if (!inherits(control, "grmtree_control")) {
    stop("'control' must be created by grmtree.control()")
  }
  if (!is.numeric(n_cores) || length(n_cores) != 1L || n_cores < 1) {
    stop("'n_cores' must be a positive integer")
  }
  if (!is.null(seed) && !is.numeric(seed)) {
    stop("'seed' must be NULL or a numeric value")
  }

  structure(
    list(n_tree            = as.integer(n_tree),
         sampling          = sampling,
         sample_fraction   = sample_fraction,
         mtry              = if (!is.null(mtry)) as.integer(floor(mtry)) else NULL,
         remove_dead_trees = isTRUE(remove_dead_trees),
         control           = control,
         n_cores           = as.integer(n_cores),
         seed              = if (!is.null(seed)) as.integer(seed) else NULL,
         verbose           = isTRUE(verbose)),
    class = "grmforest_control"
  )
}
