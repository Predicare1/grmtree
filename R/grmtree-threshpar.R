#' Extract Threshold Parameters from GRM Tree
#'
#' Extracts threshold parameters for each item from all terminal nodes of a
#' graded response model tree. The thresholds represent the points on the latent
#' trait continuum where the probability of scoring in adjacent response
#' categories is equal.
#'
#' @param object A `grmtree` object.
#' @param node Optional vector of node IDs to extract from. If NULL (default),
#'   extracts from all terminal nodes.
#' @param ... Additional arguments (currently unused).
#'
#' @return A data.frame with threshold parameters for each item in each node,
#'   with columns: \item{Node}{Node ID} \item{Item}{Item name} \item{d1, d2,
#'   ...}{Threshold parameters for each category}
#'
#' @examples
#' \donttest{
#'   library(grmtree)
#'   library(hlt)
#'
#'   data("asti", package = "hlt")
#'   asti$resp <- data.matrix(asti[, 1:4])
#'
#'   # Fit GRM tree with gender and group as partitioning variables
#'   tree <- grmtree(resp ~ gender + group,
#'           data = asti,
#'           control = grmtree.control(minbucket = 30))
#'
#'   # Get all thresholds
#'   thresholds <- threshpar_grmtree(tree)
#'   print(thresholds)
#'
#' }
#'
#' @seealso \code{\link{grmtree}} fits a Graded Response Model Tree,
#' \code{\link{grmforest}} for GRM Forests, \code{\link{fscores_grmtree}} for
#' computing factor scores, \code{\link{discrpar_grmtree}} for extracting
#' discrimination parameters, \code{\link{itempar_grmtree}} for extracting item
#' parameters
#'
#' @export
#' @importFrom partykit nodeids nodeapply
#' @importFrom mirt coef
threshpar_grmtree <- function(object, node = NULL, ...) {
  # Validate input
  if (!inherits(object, "grmtree")) {
    stop("'object' must be a grmtree object")
  }

  # Get terminal nodes if not specified
  if (is.null(node)) {
    node <- partykit::nodeids(object, terminal = TRUE)
    if (length(node) == 0) {
      stop("No terminal nodes found in tree")
    }
  }

  # Validate node IDs
  all_nodes <- partykit::nodeids(object)
  invalid_nodes <- setdiff(node, all_nodes)
  if (length(invalid_nodes) > 0) {
    stop("Invalid node IDs: ", paste(invalid_nodes, collapse = ", "))
  }

  # Extract thresholds for each node
  thresholds <- do.call("rbind", lapply(node, function(n) {
    # Get model from node
    model <- tryCatch(
      partykit::nodeapply(object, ids = n, FUN = function(nd) nd$info$object)[[1]],
      error = function(e) {
        stop("Failed to extract model from node ", n, ": ", e$message)
      }
    )

    # Get coefficients
    coef_model <- tryCatch(
      mirt::coef(model, IRTpars=T, simplify = TRUE),
      error = function(e) {
        stop("Failed to extract coefficients from node ", n, ": ", e$message)
      }
    )

    # Extract thresholds
    thresh_cols <- grep("^b", colnames(coef_model$items))
    if (length(thresh_cols) == 0) {
      stop("No threshold parameters found in node ", n)
    }

    thresh <- coef_model$items[, thresh_cols, drop = FALSE]
    items <- rownames(coef_model$items)

    # Create output data frame
    thresh_df <- as.data.frame(thresh)
    thresh_df$Item <- items
    thresh_df$Node <- n

    # Reorder columns
    thresh_df <- thresh_df[, c("Node", "Item", colnames(thresh)), drop = FALSE]

    return(thresh_df)
  }))

  # Reset row names
  rownames(thresholds) <- NULL

  return(thresholds)
}


# Internal non-exported version of threshold extraction
grm_threshpar <- function(object, node = NULL, ...) {
  if (!inherits(object, "grmtree")) {
    stop("'object' must be a grmtree object")
  }

  if (is.null(node)) {
    node <- partykit::nodeids(object, terminal = TRUE)
  }

  thresholds <- tryCatch(
    do.call("rbind", partykit::nodeapply(object, ids = node, FUN = function(n) {
      model <- partykit::info_node(n)$object
      coef_model <- mirt::coef(model, IRTpars=T, simplify=TRUE)
      coef_model$items[, grep("^b", colnames(coef_model$items)), drop = FALSE]
    })),
    error = function(e) {
      stop("Failed to extract thresholds: ", e$message)
    }
  )

  return(thresholds)
}

