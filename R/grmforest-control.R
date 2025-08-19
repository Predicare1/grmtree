#' Control Parameters for GRM Forest
#'
#' Creates a control object for `grmforest` containing parameters that control
#' the forest growing process including sampling, tree growing, and error
#' handling.
#'
#' @param n_tree Number of trees in the forest (default: 100).
#' @param sampling Sampling method: "bootstrap" (with replacement) or
#'   "subsample" (without replacement) (default: "bootstrap").
#' @param sample_fraction Fraction of data to sample for each tree (default:
#'   0.632).
#' @param mtry Number of variables randomly sampled as candidates at each split.
#'   If NULL, all variables are considered (default: NULL).
#' @param remove_dead_trees Logical indicating whether to remove trees that
#'   encounter errors during fitting (default: TRUE).
#' @param control Control parameters for individual trees created by
#'   `grmtree.control()`.
#' @param alpha Significance level for splitting (default: 0.05).
#' @param minbucket Minimum number of observations in terminal nodes (default:
#'   20).
#' @param seed Random seed for reproducibility (default: NULL).
#'
#' @return A list of class `grmforest_control` containing: \item{n_tree}{Number
#'   of trees} \item{sampling}{Sampling method} \item{sample_fraction}{Sample
#'   fraction} \item{mtry}{Number of variables to try at each split}
#'   \item{remove_dead_trees}{Whether to remove failed trees}
#'   \item{control}{Tree control parameters} \item{seed}{Random seed}
#'
#' @examples
#' library(grmtree)
#' # Control with 50 trees using subsampling
#' ctrl <- grmforest.control(n_tree = 50, sampling = "subsample")
#'
#' # Control with specific tree parameters
#' ctrl <- grmforest.control(
#'   control = grmtree.control(minbucket = 30, alpha = 0.01)
#' )
#'
#' @seealso \code{\link{grmtree.control}} creates a control object for
#' `grmtree`, \code{\link{plot.grmtree}} creates plot for the `grmtree` object,
#' \code{\link{grmforest}} for GRM Forests,
#'
#' @export
grmforest.control <- function(
    n_tree = 100,
    sampling = "bootstrap",
    sample_fraction = 0.632,
    mtry = NULL,
    remove_dead_trees = TRUE,
    control = grmtree.control(),
    alpha = 0.05,
    minbucket = 20,
    seed = NULL
) {
  # Validate number of trees
  if (!is.numeric(n_tree) || length(n_tree) != 1) {
    stop("'n_tree' must be a single numeric value")
  }
  if (n_tree < 1) {
    stop("'n_tree' must be at least 1")
  }

  # Validate sampling method
  sampling_methods <- c("bootstrap", "subsample")
  if (!sampling %in% sampling_methods) {
    stop("'sampling' must be one of: ", paste(sampling_methods, collapse = ", "))
  }

  # Validate sample fraction
  if (!is.numeric(sample_fraction) || length(sample_fraction) != 1) {
    stop("'sample_fraction' must be a single numeric value")
  }
  if (sample_fraction <= 0 || sample_fraction > 1) {
    stop("'sample_fraction' must be between 0 and 1")
  }

  # Validate mtry
  if (!is.null(mtry) && (!is.numeric(mtry) || mtry < 1)) {
    stop("'mtry' must be NULL or a positive integer")
  }

  # Validate control parameters
  if (!inherits(control, "grmtree_control")) {
    stop("'control' must be created by grmtree.control()")
  }

  # Validate seed
  if (!is.null(seed) && !is.numeric(seed)) {
    stop("'seed' must be NULL or a numeric value")
  }

  # Return control object
  control_obj <- list(
    n_tree = as.integer(n_tree),
    sampling = sampling,
    sample_fraction = sample_fraction,
    mtry = if (!is.null(mtry)) as.integer(mtry) else NULL,
    remove_dead_trees = isTRUE(remove_dead_trees),
    control = control,
    seed = if (!is.null(seed)) as.integer(seed) else NULL
  )

  class(control_obj) <- "grmforest_control"
  return(control_obj)
}
