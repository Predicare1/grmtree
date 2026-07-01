#' Compute Latent Factor Scores for Each Terminal Node in a GRM Tree
#'
#' This function calculates latent factor scores for each terminal node in a GRM
#' tree object using specified scoring method (EAP, MAP, ML, or WLE).
#'
#' @param grmtree_obj A GRM tree object (from `grmtree()` function) containing
#'   fitted models in its terminal nodes.
#' @param method Scoring method to use: "EAP" (default), "MAP", "ML", or "WLE".
#'   See `mirt::fscores()` for details.
#'
#' @return A named list where each element contains the factor scores for a
#'   terminal node. Names correspond to node IDs. Returns NULL for nodes where
#'   computation fails. If no scores can be computed for any node, returns NULL
#'   with a warning.
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
#' # Compute EAP scores for all terminal nodes
#' node_scores <- fscores_grmtree(tree)
#'
#' # Compute MAP scores instead
#' node_scores_map <- fscores_grmtree(tree, method = "MAP")
#' }
#'
#' @seealso \code{\link[mirt]{fscores}} for factor scoring methods,
#' \code{\link{grmtree}} fits a Graded Response Model Tree,
#' \code{\link{grmforest}} for GRM Forests, \code{\link{threshpar_grmtree}} for
#' extracting threshold parameters, \code{\link{discrpar_grmtree}} for
#' extracting discrimination parameters, \code{\link{itempar_grmtree}} for
#' extracting item parameters, \code{\link{generate_node_scores_dataset}}
#' generates combined dataset with node assignments and factor scores
#'
#' @export
fscores_grmtree <- function(grmtree_obj, method = "EAP") {
  # Input validation
  if (missing(grmtree_obj)) {
    stop("Argument 'grmtree_obj' is missing with no default.")
  }
  if (!inherits(grmtree_obj, "grmtree")) {
    stop("grmtree_obj must be a GRM tree object from grmtree() function.")
  }
  if (!method %in% c("EAP", "MAP", "ML", "WLE")) {
    stop("method must be one of: 'EAP', 'MAP', 'ML', or 'WLE'")
  }

  # Identify terminal nodes - will return at least root node (1) for empty trees
  terminal_nodes <- partykit::nodeids(grmtree_obj, terminal = TRUE)

  # Initialize list to store scores for each node
  scores_list <- vector("list", length(terminal_nodes))
  names(scores_list) <- as.character(terminal_nodes)

  # Loop over each terminal node
  for (node_id in terminal_nodes) {
    message("Processing node: ", node_id)
    # Get data for the node
    node_data <- tryCatch(
      partykit::data_party(grmtree_obj, id = node_id),
      error = function(e) {
        return(data.frame()) # Return empty data.frame to trigger next check
      }
    )

    if (nrow(node_data) == 0) next

    # Extract the fitted model for the node
    node_model <- tryCatch(
      apply_to_models(grmtree_obj, node = node_id, drop = TRUE),
      error = function(e) NULL
    )

    if (is.null(node_model)) next

    # Compute factor scores for the node WITH convergence checking
    node_scores <- suppressWarnings(
      tryCatch({
        result <- mirt::fscores(node_model, method = method)
        # Check for convergence issues
        if (any(is.na(result)) || any(!is.finite(result))) {
          warning("Some factor score estimates may not have converged properly")
        }
        if (is.matrix(result)) result[, 1] else result  # Ensure vector output
      }, error = function(e) {
        warning("Factor score computation failed for node ", node_id, ": ", e$message)
        return(NULL)
      })
    )

    if (!is.null(node_scores)) {
      scores_list[[as.character(node_id)]] <- node_scores
    }
  }

  # Remove NULL elements
  scores_list <- Filter(Negate(is.null), scores_list)

  if (length(scores_list) == 0) {
    stop("No factor scores were successfully computed for any node.")
  }

  return(scores_list)
}

