#' Extract Discrimination Parameters from GRM Tree
#'
#' Extracts discrimination parameters (slope parameters) for each item from all
#' terminal nodes of a graded response model tree. The discrimination parameter
#' indicates how well an item distinguishes between respondents with different
#' levels of the latent trait.
#'
#' @param object A `grmtree` object.
#' @param node Optional vector of node IDs to extract from. If NULL (default),
#'   extracts from all terminal nodes.
#' @param ... Additional arguments (currently unused).
#'
#' @return A data.frame with discrimination parameters for each item in each
#'   node, with columns: \item{Node}{Node ID} \item{Item}{Item name}
#'   \item{Discrimination}{Discrimination parameter (a1)}
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
#'   # Get all discrimination parameters
#'   discr <- discrpar_grmtree(tree)
#'   print(discr)
#'
#' }
#'
#' @seealso \code{\link{grmtree}} fits a Graded Response Model Tree,
#' \code{\link{grmforest}} for GRM Forests, \code{\link{fscores_grmtree}} for
#' computing factor scores, \code{\link{threshpar_grmtree}} for extracting
#' threshold parameters, \code{\link{itempar_grmtree}} for extracting item
#' parameters
#'
#' @export
#' @importFrom partykit nodeids nodeapply
#' @importFrom mirt coef
discrpar_grmtree <- function(object, node = NULL, ...) {
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

  # Extract discrimination parameters for each node
  discr_list <- lapply(node, function(n) {
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

    discr <- coef_model$items[, "a", drop = FALSE]
    items <- rownames(coef_model$items)

    # Create output data frame
    discr_df <- data.frame(
      Node = n,
      Item = items,
      Discrimination = discr[, "a"],
      row.names = NULL
    )

    return(discr_df)
  })

  # Combine all nodes' results
  discrimination <- do.call("rbind", discr_list)

  # Reset row names
  rownames(discrimination) <- NULL

  return(discrimination)
}
