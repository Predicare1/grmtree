#' Internal Function: Apply Function to Models in Nodes
#'
#' Applies a function to the GRM models in specified nodes of a tree.
#' This is an internal helper function and not meant to be called directly.
#'
#' @param object A `grmtree` object.
#' @param node Vector of node IDs (default: all nodes).
#' @param FUN Function to apply to each model.
#' @param drop Logical indicating whether to drop list structure if length 1.
#' @param ... Additional arguments passed to FUN.
#'
#' @return List of results (or single result if drop=TRUE and length(node)=1).
#'
#' @keywords internal
apply_to_models <- function(object, node = NULL, FUN = NULL, drop = FALSE, ...) {
  # Validate inputs
  if (!inherits(object, "grmtree")) {
    stop("'object' must be a grmtree object")
  }
  
  if (is.null(node)) {
    node <- partykit::nodeids(object, terminal = FALSE)
  }
  
  if (is.null(FUN)) {
    FUN <- function(object, ...) object
  }
  
  if (!is.function(FUN)) {
    stop("'FUN' must be a function")
  }
  
  # Apply function to models
  rval <- if ("object" %in% object$info$control$terminal) {
    tryCatch(
      partykit::nodeapply(object, node, function(n) FUN(partykit::info_node(n)$object, ...)),
      error = function(e) {
        stop("Failed to apply function to nodes: ", e$message)
      }
    )
  } else {
    tryCatch(
      lapply(partykit::refit.modelparty(object, node, drop = FALSE), FUN, ...),
      error = function(e) {
        stop("Failed to refit and apply function: ", e$message)
      }
    )
  }
  
  names(rval) <- node
  
  if (drop && length(node) == 1L) {
    rval <- rval[[1L]]
  }
  
  return(rval)
}
