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
#' \dontrun{
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

    # Compute factor scores for the node
    node_scores <- tryCatch(
      {
        scores <- mirt::fscores(node_model, method = method)
        if (is.matrix(scores)) scores[, 1] else scores  # Ensure vector output
      },
      error = function(e) NULL
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


#' Generate Combined Dataset with Node Assignments and Factor Scores
#'
#' Creates a dataset combining original data with node assignments and computed
#' factor scores. Maintains original row order while adding node membership and
#' factor score information.
#'
#' @param grmtree_obj A GRM tree object (from `grmtree()` function).
#' @param method Scoring method to use: "EAP" (default), "MAP", "ML", or "WLE".
#'
#' @return A data.frame containing:
#'         - Original variables from the model frame
#'         - 'node': Factor indicating terminal node membership (e.g., "Node 1")
#'         - 'factor_score': Computed latent factor scores
#'   Rows are in original order with sequential row names.
#'
#' @examples
#' \dontrun{
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
#' # Generate combined dataset
#' scored_data <- generate_node_scores_dataset(tree)
#'
#' # Plot scores by node
#' boxplot(factor_score ~ node, data = scored_data)
#' }
#'
#' @seealso \code{\link{grmtree}} fits a Graded Response Model Tree,
#' \code{\link{grmforest}} for GRM Forests, \code{\link{fscores_grmtree}} for
#' computing factor scores, \code{\link{threshpar_grmtree}} for extracting
#' threshold parameters, \code{\link{discrpar_grmtree}} for extracting
#' discrimination parameters, \code{\link{itempar_grmtree}} for extracting item
#' parameters
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
generate_node_scores_dataset <- function(grmtree_obj, method = "EAP") {
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

  # Get factor scores with warnings suppressed
  scores_list <- suppressWarnings(tryCatch(
    fscores_grmtree(grmtree_obj, method = method),
    error = function(e) NULL
  ))

  if (is.null(scores_list)) {
    stop("No factor scores available to create dataset.")
  }

  # Get all node IDs that have scores
  scored_nodes <- names(scores_list)

  # Initialize list to store combined data
  all_data <- vector("list", length(scored_nodes))

  # Loop through nodes with successful score computation
  for (i in seq_along(scored_nodes)) {
    node_id <- scored_nodes[i]

    # Get node data
    node_data <- tryCatch(
      partykit::data_party(grmtree_obj, id = as.integer(node_id)),
      error = function(e) {
        warning("Failed to get data for node ", node_id, ": ", e$message)
        return(NULL)
      }
    )

    if (is.null(node_data)) next

    # Create data frame with scores and node info
    result <- data.frame(
      node_data,
      node = factor(
        node_id,
        levels = scored_nodes,
        labels = paste0("Node ", seq_along(scored_nodes))
      ),
      factor_score = scores_list[[i]],
      row_id = rownames(node_data),
      stringsAsFactors = FALSE
    )

    all_data[[i]] <- result
  }

  # Combine all data
  combined_data <- do.call(rbind, all_data)

  if (is.null(combined_data)) {
    stop("Failed to combine data from all nodes.")
  }

  # Sort by original row order and clean up
  combined_data <- combined_data %>%
    dplyr::arrange(as.numeric(.data$row_id)) %>%
    dplyr::select(-.data$row_id) %>%
    dplyr::relocate(.data$node, .before = "factor_score")

  rownames(combined_data) <- NULL  # Reset row names

  return(combined_data)
}



