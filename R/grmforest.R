#' Fit a Forest of Graded Response Model Trees for Ensemble-Based DIF Detection
#'
#' This function implements a forest of graded response model trees (GRM Forest)
#' using bootstrap aggregation (bagging) or random subsampling to enhance the
#' detection and analysis of differential item functioning (DIF) in polytomous items.
#' The GRM Forest approach combines the strengths of multiple GRMTrees to provide
#' more robust and stable DIF detection, particularly for complex datasets with
#' high-dimensional covariates or subtle DIF patterns.
#'
#' The algorithm works by fitting multiple GRMTrees, each on a random sample of
#' the original data (either through bootstrap sampling or subsampling). For each
#' tree, approximately one-third of the observations are left out as out-of-bag
#' (OOB) samples, which are used for internal validation and variable importance
#' calculation. The ensemble approach reduces variance, minimizes overfitting,
#' and provides more reliable identification of covariates associated with DIF.
#'
#' Key advantages of the GRM Forest approach include:
#' - Enhanced stability in DIF detection across different sampling variations
#' - Robust variable importance measures that quantify the relative contribution
#'   of each covariate to DIF patterns
#' - Reduced false positive rates through consensus-based detection
#' - Ability to handle high-dimensional covariate spaces effectively
#' - Internal validation through out-of-bag error estimation
#'
#' The forest implementation supports both bootstrap aggregation (where samples
#' are drawn with replacement) and subsampling (without replacement), allowing
#' flexibility for different data characteristics and research objectives.
#'
#' @param formula A formula specifying the model structure with the response
#'   matrix on the left and partitioning variables on the right (e.g.,
#'   `response_matrix ~ age + gender + education + clinical_variables`).
#' @param data A data frame containing the response matrix and partitioning
#'   variables. The response matrix should contain polytomous items coded
#'   as ordered factors.
#' @param control A control object created by `grmforest.control()`.
#' @param ... Additional arguments passed to underlying `grmtree()` function.
#'
#' @return An object of class `grmforest` containing: \item{trees}{List of
#'   fitted GRM trees} \item{oob_samples}{List of out-of-bag samples for each
#'   tree} \item{formula}{The model formula} \item{data}{The original dataset}
#'   \item{call}{The function call}
#'
#' @examples
#'
#' \donttest{
#'   library(grmtree)
#'   library(hlt)
#'   data("asti", package = "hlt")
#'   asti$resp <- data.matrix(asti[, 1:4])
#'
#'   # Fit forest with default parameters
#'   forest <- grmforest(resp ~ gender + group, data = asti)
#'
#'   # Fit with custom control
#'   ctrl <- grmforest.control(n_tree = 50, sampling = "subsample")
#'   forest <- grmforest(resp ~ gender + group, data = asti, control = ctrl)
#' }
#'
#' @seealso \code{\link{grmtree}} fits a Graded Response Model Tree,
#' \code{\link{grmtree.control}} creates a control object for `grmtree`,
#' \code{\link{grmforest.control}} creates a control object for `grmforest`,
#' \code{\link{varimp}} calculates the variable importance for GRM Forest,
#' \code{\link{plot.varimp}} creates a bar plot of variable importance scores
#'
#' @export
grmforest <- function(formula, data, control = grmforest.control(), ...) {
  # Validate inputs
  if (!inherits(formula, "formula")) {
    stop("'formula' must be a valid formula object")
  }

  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame")
  }


  # Parse formula to check response variable
  mf <- tryCatch(
    model.frame(formula, data = data),
    error = function(e) {
      stop("Error in model.frame: ", e$message)
    }
  )

  y <- model.response(mf)
  if (!is.matrix(y)) {
    stop("Response variable must be a matrix of item responses")
  }

  if (!inherits(control, "grmforest_control")) {
    stop("'control' must be created by grmforest.control()")
  }

  if (nrow(data) < 10) {
    stop("Insufficient data: nrow(data) must be at least 10")
  }

  # Set random seed if specified
  if (!is.null(control$seed)) {
    set.seed(control$seed)
  }

  # Initialize forest structure
  forest <- list(
    trees = vector("list", control$n_tree),
    oob_samples = vector("list", control$n_tree),
    formula = formula,
    data = data,
    call = match.call()
  )

  n <- nrow(data)
  sample_size <- round(n * control$sample_fraction)

  # Grow forest
  for (i in seq_len(control$n_tree)) {
    # Sample data
    if (control$sampling == "bootstrap") {
      sample_idx <- sample.int(n, size = sample_size, replace = TRUE)
    } else {
      sample_idx <- sample.int(n, size = sample_size, replace = FALSE)
    }
    oob_idx <- setdiff(seq_len(n), sample_idx)

    # Fit tree with error handling
    tree <- tryCatch({
      grmtree(
        formula,
        data = data[sample_idx, , drop = FALSE],
        control = control$control,
        ...
      )
    }, error = function(e) {
      if (control$remove_dead_trees) {
        warning("Tree ", i, " failed: ", e$message)
        NULL
      } else {
        stop("Tree ", i, " failed: ", e$message)
      }
    })

    # Store results
    if (!is.null(tree)) {
      forest$trees[[i]] <- tree
      forest$oob_samples[[i]] <- data[oob_idx, , drop = FALSE]
    }

    # Progress reporting
    if (interactive() && i %% 10 == 0) {
      cat(sprintf("Fitted %d/%d trees\n", i, control$n_tree))
    }
  }

  # Remove NULL entries if any trees failed
  failed_trees <- sapply(forest$trees, is.null)
  if (any(failed_trees)) {
    forest$trees <- forest$trees[!failed_trees]
    forest$oob_samples <- forest$oob_samples[!failed_trees]
    warning(sum(failed_trees), " trees failed and were removed")
  }

  if (length(forest$trees) == 0) {
    stop("All trees failed to fit")
  }

  class(forest) <- "grmforest"
  return(forest)
}



#' Print Method for GRM Forest
#'
#' @param x A `grmforest` object
#' @param ... Additional arguments (currently unused)
#'
#' @return Invisibly returns the input object
#'
#' @export
print.grmforest <- function(x, ...) {
  if (!inherits(x, "grmforest")) {
    stop("'x' must be a grmforest object")
  }

  cat("GRM Forest with", length(x$trees), "trees\n")

  invisible(x)
}



