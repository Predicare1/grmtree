

#' Generate Dataset with Node Assignments and Factor Scores
#'
#' Creates a dataset by augmenting the original data with node assignments
#' and computed factor scores. Unlike the previous version which only returned
#' model frame variables, this version merges node and score information back
#' to the full original data frame.
#'
#' @param object A \code{grmtree} or \code{longitudinal_grmtree} object.
#' @param data The original data frame used to fit the tree. If provided,
#'   the output contains all columns from this data frame plus node and
#'   factor score columns. If NULL (default), returns only model frame
#'   variables (backward-compatible behavior).
#' @param method Scoring method: "EAP" (default), "MAP", "ML", or "WLE".
#'
#' @return A data.frame containing:
#'   \describe{
#'     \item{All original columns}{From \code{data} if provided}
#'     \item{node}{Factor indicating terminal node membership}
#'     \item{factor_score}{For cross-sectional grmtree: single latent score.
#'       For longitudinal: Theta_T1.}
#'     \item{Theta_T1}{(Longitudinal only) Latent trait at T1}
#'     \item{Theta_T2}{(Longitudinal only) Latent trait at T2}
#'   }
#'
#' @details
#' The function works by:
#' \enumerate{
#'   \item Predicting node membership for each observation using
#'     \code{predict(object, type = "node")}
#'   \item Computing factor scores within each terminal node using the
#'     node-specific model
#'   \item Merging the results back to the original data by row position
#' }
#'
#' When \code{data} is provided, the function ensures that the output
#' contains all columns from the original data frame, not just the
#' variables used in the model formula. This is important when the
#' original data contains clinical variables, identifiers, or other
#' columns not used as partitioning variables.
#'
#' @examplesIf interactive()
#' # Cross-sectional GRMTree
#' library(grmtree)
#' library(hlt)
#' data("asti", package = "hlt")
#' asti$resp <- data.matrix(asti[, 1:4])
#'
#'   # Fit GRM tree with gender and group as partitioning variables
#'   tree <- grmtree(resp ~ gender + group,
#'           data = asti,
#'           control = grmtree.control(minbucket = 30))
#'
#' # Generate combined dataset
#' scored_data <- generate_node_scores_dataset(tree, data = asti)
#'
#' # Plot scores by node
#' boxplot(factor_score ~ node, data = scored_data)
#'
#' @seealso \code{\link{grmtree}} fits a Graded Response Model Tree,
#' \code{\link{grmforest}} for GRM Forests, \code{\link{fscores_grmtree}} for
#' computing factor scores, \code{\link{threshpar_grmtree}} for extracting
#' threshold parameters, \code{\link{discrpar_grmtree}} for extracting
#' discrimination parameters, \code{\link{itempar_grmtree}} for extracting item
#' parameters, \code{\link{longitudinal_grmtree}} for longitudinal GRMTree,
#' \code{\link{fscores_longitudinal_grmtree}}, for computing factor scores for longitudinal GRMTree
#'
#' @export
#' @importFrom stats na.pass

generate_node_scores_dataset <- function(object, data = NULL, method = "EAP") {

  if (!inherits(object, "grmtree")) {
    stop("'object' must be a grmtree or longitudinal_grmtree object")
  }
  if (!method %in% c("EAP", "MAP", "ML", "WLE")) {
    stop("method must be one of: 'EAP', 'MAP', 'ML', or 'WLE'")
  }

  is_longitudinal <- inherits(object, "longitudinal_grmtree")

  # ---- Get node assignments ----
  # predict() on a mob object returns the terminal node ID for each obs
  node_ids <- predict(object, type = "node")

  terminal_nodes <- sort(unique(node_ids))
  n_obs <- length(node_ids)

  # ---- Compute factor scores per node ----
  if (is_longitudinal) {
    scores_list <- fscores_longitudinal_grmtree(object, method = method)
  } else {
    scores_list <- fscores_grmtree(object, method = method)
  }

  # ---- Build output vectors ----
  if (is_longitudinal) {
    theta_t1 <- rep(NA_real_, n_obs)
    theta_t2 <- rep(NA_real_, n_obs)

    for (nd_id in names(scores_list)) {
      nd_int <- as.integer(nd_id)
      obs_idx <- which(node_ids == nd_int)
      nd_scores <- scores_list[[nd_id]]

      if (nrow(nd_scores) != length(obs_idx)) {
        warning("Score count mismatch in node ", nd_id,
                ": expected ", length(obs_idx), ", got ", nrow(nd_scores))
        next
      }
      theta_t1[obs_idx] <- nd_scores$Theta_T1
      theta_t2[obs_idx] <- nd_scores$Theta_T2
    }
  } else {
    factor_score <- rep(NA_real_, n_obs)

    for (nd_id in names(scores_list)) {
      nd_int <- as.integer(nd_id)
      obs_idx <- which(node_ids == nd_int)
      nd_scores <- scores_list[[nd_id]]

      if (length(nd_scores) != length(obs_idx)) {
        warning("Score count mismatch in node ", nd_id)
        next
      }
      factor_score[obs_idx] <- nd_scores
    }
  }

  # ---- Create node labels ----
  node_factor <- factor(
    node_ids,
    levels = terminal_nodes,
    labels = paste0("Node ", terminal_nodes)
  )

  # ---- Build output data frame ----
  if (!is.null(data)) {
    # Merge back to original data
    if (nrow(data) != n_obs) {
      warning("Original data has ", nrow(data), " rows but model has ",
              n_obs, " observations (after na.action). ",
              "Attempting to match by model frame row indices.")
      # Get the row indices that survived na.action
      mf <- model.frame(formula(object), data = data, na.action = na.pass)
      complete_idx <- complete.cases(mf)
      if (sum(complete_idx) == n_obs) {
        out <- data[complete_idx, , drop = FALSE]
      } else {
        warning("Cannot match rows. Returning model frame variables only.")
        out <- data.frame(row.names = 1:n_obs)
      }
    } else {
      out <- data
    }
  } else {
    # Backward-compatible: return model frame variables
    out <- data.frame(row.names = 1:n_obs)
  }

  # Add node and scores
  out$node <- node_factor

  if (is_longitudinal) {
    out$Theta_T1 <- theta_t1
    out$Theta_T2 <- theta_t2
  } else {
    out$factor_score <- factor_score
  }

  rownames(out) <- NULL
  return(out)
}


