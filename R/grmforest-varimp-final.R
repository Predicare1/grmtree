#' Calculate Variable Importance for GRM Forest
#'
#' Computes permutation importance scores for variables in a GRM forest using
#' out-of-bag samples. Importance is measured by the decrease in log-likelihood
#' when a variable's values are permuted.
#'
#' @param forest A `grmforest` object created by `grmforest()`.
#' @param method Importance calculation method (currently only "permutation").
#' @param verbose Logical indicating whether to show progress messages.
#' @param seed Random seed for reproducibility.
#'
#' @return A named numeric vector of importance scores with class `varimp`.
#'   Higher values indicate more important variables.
#'
#' @examples
#' \donttest{
#' library(grmtree)
#' library(hlt)
#' data("asti", package = "hlt")
#' asti$resp <- data.matrix(asti[, 1:4])
#'
#' ## Fit the GRM Forest
#' forest <- grmforest(resp ~ gender + group, data = asti,
#' control = grmforest.control(n_tree = 30))
#'   importance <- varimp(forest)
#'
#' ## Print and plot the variable importance scores
#' print(importance)
#' plot(importance)
#' }
#'
#' @seealso \code{\link{grmtree}} fits a Graded Response Model Tree,
#'   \code{\link{grmforest}} for GRM Forests, \code{\link{grmforest.control}}
#'   creates a control object for `grmforest`, \code{\link{plot.varimp}} creates
#'   a bar plot of variable importance scores
#'
#' @export
#' @importFrom stats predict
varimp <- function(forest, method = "permutation", verbose = FALSE, seed = NULL) {
  # Validate inputs
  if (!inherits(forest, "grmforest")) {
    stop("'forest' must be a grmforest object created by grmforest()")
  }

  if (method != "permutation") {
    stop("Only permutation importance is implemented")
  }

  if (!is.null(seed)) set.seed(seed)


  # Get variable names from formula
  var_names <- all.vars(forest$formula)[-1]
  if (length(var_names) == 0) {
    stop("No predictor variables found in formula")
  }

  importance_scores <- numeric(length(var_names))
  names(importance_scores) <- var_names

  # Compute baseline performance
  baseline_perf <- evaluate_forest(forest, verbose)
  if (verbose) {
    message("Baseline OOB performance: ", baseline_perf)
    message("Using ", length(forest$trees), " trees")
  }

  # Calculate importance for each variable
  for (var in var_names) {
    if (verbose) message("\nProcessing variable: ", var)

    permuted_perf <- 0
    valid_trees <- 0

    for (i in seq_along(forest$trees)) {
      tree <- forest$trees[[i]]
      oob_data <- forest$oob_samples[[i]]

      # Skip invalid trees or OOB data
      if (is.null(tree) || is.null(oob_data) || nrow(oob_data) == 0) {
        if (verbose) message("Skipping tree ", i, " - no valid OOB data")
        next
      }

      # Create permuted copy
      permuted_data <- oob_data
      permuted_data[[var]] <- sample(permuted_data[[var]])

      # Evaluate permutation
      tree_perf <- evaluate_tree(tree, permuted_data, verbose)
      if (!is.na(tree_perf)) {
        permuted_perf <- permuted_perf + tree_perf
        valid_trees <- valid_trees + 1
        if (verbose) message("Tree ", i, " perf: ", tree_perf)
      }
    }

    # Calculate importance score
    if (valid_trees > 0) {
      importance_scores[var] <- baseline_perf - (permuted_perf / valid_trees)
      if (verbose) {
        message("Variable: ", var,
                " | Importance: ", importance_scores[var],
                " (based on ", valid_trees, " trees)")
      }
    } else {
      warning("No valid trees for variable '", var, "' - importance set to 0")
      importance_scores[var] <- 0
    }
  }

  if (valid_trees == 0) {
    warning("No valid trees with OOB data available")
    importance_scores <- numeric(length(var_names))
    names(importance_scores) <- var_names
    class(importance_scores) <- c("varimp", "numeric")
    return(importance_scores)  # Return properly structured zero vector
  }

  # Sort by importance
  importance_scores <- sort(importance_scores, decreasing = TRUE)
  class(importance_scores) <- c("varimp", "numeric")

  return(importance_scores)
}

# Internal helper functions --------------------------------------------------

evaluate_forest <- function(forest, verbose = FALSE) {
  total_perf <- 0
  valid_trees <- 0

  for (i in seq_along(forest$trees)) {
    tree <- forest$trees[[i]]
    oob_data <- forest$oob_samples[[i]]

    if (!is.null(tree) && !is.null(oob_data) && nrow(oob_data) > 0) {
      tree_perf <- evaluate_tree(tree, oob_data, verbose)
      if (!is.na(tree_perf)) {
        total_perf <- total_perf + tree_perf
        valid_trees <- valid_trees + 1
        if (verbose) message("Tree ", i, " baseline perf: ", tree_perf)
      }
    }
  }

  if (valid_trees == 0) {
    warning("No valid trees with OOB data available")
    return(0)
  }

  return(total_perf / valid_trees)
}

evaluate_tree <- function(tree, data, verbose = FALSE) {
  if (is.null(tree) || nrow(data) == 0) return(NA_real_)

  # Get terminal nodes
  terminal_nodes <- tryCatch(
    predict(tree, data, type = "node"),
    error = function(e) {
      if (verbose) warning("Prediction failed: ", e$message)
      return(rep(1, nrow(data))) # Default to root node
    }
  )

  loglik <- 0
  for (node_id in unique(terminal_nodes)) {
    node_data <- data[terminal_nodes == node_id, , drop = FALSE]
    if (nrow(node_data) > 0) {
      node_model <- tryCatch(
        get_node_model(tree, node_id),
        error = function(e) {
          if (verbose) warning("Node model error: ", e$message)
          NULL
        }
      )

      if (!is.null(node_model)) {
        node_loglik <- calculate_node_loglik(node_model, node_data)
        loglik <- loglik + node_loglik
      }
    }
  }

  return(loglik)
}

calculate_node_loglik <- function(grm_model, data) {
  # Extract response matrix - assumes response is in a matrix column called 'resp'
  y <- as.matrix(data$resp)
  if (is.null(y)) stop("Response matrix 'resp' not found in data")

  # Get item parameters
  item_params <- tryCatch(
    mirt::coef(grm_model, simplify = TRUE)$items,
    error = function(e) stop("Failed to get item parameters: ", e$message)
  )

  loglik <- 0
  for (i in seq_len(nrow(y))) {
    for (j in seq_len(ncol(y))) {
      response <- y[i, j]
      if (is.na(response)) next

      a <- item_params[j, "a1"]
      d <- item_params[j, grep("d", colnames(item_params))]

      # Calculate response probability
      if (response == 1) {
        prob <- stats::plogis(a * (0 - d[1]))
      } else if (response == length(d) + 1) {
        prob <- 1 - stats::plogis(a * (0 - d[length(d)]))
      } else {
        prob <- stats::plogis(a * (0 - d[response])) - stats::plogis(a * (0 - d[response - 1]))
      }

      # Avoid log(0)
      prob <- pmax(prob, .Machine$double.eps)
      loglik <- loglik + log(prob)
    }
  }

  return(loglik)
}

get_node_model <- function(tree, node_id) {
  node <- tryCatch(
    partykit::nodeapply(tree, ids = node_id, FUN = function(x) x)[[1]],
    error = function(e) stop("Failed to get node: ", e$message)
  )

  if (is.null(node$info$object)) {
    stop("No model found in node ", node_id)
  }

  return(node$info$object)
}
