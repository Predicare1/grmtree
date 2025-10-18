#' Extract Item Parameters from GRM Tree
#'
#' Extracts both discrimination parameters and average threshold parameters for each item
#' from all terminal nodes of a graded response model tree. This provides a comprehensive
#' view of item characteristics across different nodes of the tree.
#'
#' @param object A `grmtree` object.
#' @param node Optional vector of node IDs to extract from. If NULL (default),
#'   extracts from all terminal nodes.
#' @param ... Additional arguments (currently unused).
#'
#' @return A data.frame with item parameters for each item in each node, with columns:
#'   \item{Node}{Node ID}
#'   \item{Item}{Item name}
#'   \item{Discrimination}{Discrimination parameter (a1)}
#'   \item{AvgThreshold}{Average of threshold parameters}
#'   \item{Thresholds}{All threshold parameters as a list column}
#'
#' @examples
#' \donttest{
#'   library(grmtree)
#'   library(hlt)
#'   data("asti", package = "hlt")
#'   asti$resp <- data.matrix(asti[, 1:4])
#'
#'   # Fit GRM tree with gender and group as partitioning variables
#'   tree <- grmtree(resp ~ gender + group,
#'           data = asti,
#'           control = grmtree.control(minbucket = 30))
#'
#'   # Get all item parameters
#'   items <- itempar_grmtree(tree)
#'   print(items)
#' }
#'
#' @seealso
#' \code{\link{grmtree}} fits a Graded Response Model Tree,
#' \code{\link{grmforest}} for GRM Forests,
#' \code{\link{fscores_grmtree}} for computing factor scores,
#' \code{\link{threshpar_grmtree}} for extracting threshold parameters,
#' \code{\link{discrpar_grmtree}} for extracting discrimination parameters
#'
#' @export
itempar_grmtree <- function(object, node = NULL, ...) {
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

  # Extract item parameters for each node
  item_list <- lapply(node, function(n) {
    # Get model from node
    model <- tryCatch(
      partykit::nodeapply(object, ids = n, FUN = function(nd) nd$info$object)[[1]],
      error = function(e) {
        stop("Failed to extract model from node ", n, ": ", e$message)
      }
    )

    # Get coefficients
    coef_model <- tryCatch(
      mirt::coef(model, IRTpars=T, simplify=TRUE),
      error = function(e) {
        stop("Failed to extract coefficients from node ", n, ": ", e$message)
      }
    )

    # Extract discrimination parameters
    if (!"a" %in% colnames(coef_model$items)) {
      stop("No discrimination parameters found in node ", n)
    }
    discr <- coef_model$items[, "a"]

    # Extract threshold parameters
    thresh_cols <- grep("^b", colnames(coef_model$items))
    if (length(thresh_cols) == 0) {
      stop("No threshold parameters found in node ", n)
    }
    thresholds <- coef_model$items[, thresh_cols, drop = FALSE]

    # Calculate average thresholds
    avg_thresh <- rowMeans(thresholds, na.rm = TRUE)

    # Create output data frame
    items <- rownames(coef_model$items)
    item_df <- data.frame(
      Node = n,
      Item = items,
      Discrimination = discr,
      AvgThreshold = avg_thresh,
      stringsAsFactors = FALSE
    )

    # Add thresholds as list column
    item_df$Thresholds <- lapply(seq_len(nrow(thresholds)), function(i) {
      unname(thresholds[i, ])
    })

    return(item_df)
  })

  # Combine all nodes' results
  items <- do.call("rbind", item_list)

  # Reset row names
  rownames(items) <- NULL

  return(items)
}
