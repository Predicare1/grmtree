
#' Compute Latent Factor Scores for Longitudinal GRM Tree
#'
#' Computes latent factor scores (theta) for both T1 and T2 within each terminal
#' node of a longitudinal GRM tree. Unlike the cross-sectional
#' \code{\link{fscores_grmtree}} which returns a single theta per person, this
#' function returns two scores per person: \eqn{\theta_{T1}} and
#' \eqn{\theta_{T2}}.
#'
#' @param object A \code{longitudinal_grmtree} object.
#' @param method Scoring method: "EAP" (default), "MAP", "ML", or "WLE".
#' @param ... Additional arguments (currently unused).
#'
#' @return A named list (one element per terminal node). Each element is a
#'   \code{data.frame} with columns:
#'   \describe{
#'     \item{Theta_T1}{Estimated latent trait at Time 1}
#'     \item{Theta_T2}{Estimated latent trait at Time 2}
#'   }
#'
#' @examplesIf interactive()
#' library(grmtree)
#'
#' # Load the synthetic longitudinal data
#' data("grmtree_long_data", package = "grmtree")
#'
#'   # Prepare the wide-format response matrix
#'   items_t1 <- c("MOS_Listen", "MOS_Info", "MOS_Advice_Crisis", "MOS_Confide",
#'                 "MOS_Advice_Want", "MOS_Fears", "MOS_Personal", "MOS_Understand")
#'   ld <- prepare_longitudinal_data(
#'     data = grmtree_long_data,
#'     items_t1 = items_t1,
#'     items_t2 = paste0(items_t1, "_year1"),
#'     covariates = c("sex", "age", "residency", "job",
#'                    "education", "comorbidity_count", "ever_smoker")
#'   )
#'
#'   # Phase 1: fit the longitudinal GRM tree
#'   ltree <- longitudinal_grmtree(
#'     resp_wide ~ sex + age + residency + job +
#'       education + comorbidity_count + ever_smoker,
#'     data = ld, n_items = 8,
#'     control = grmtree.control(minbucket = 200)
#'   )
#'
#'   # Print the factor scores
#'   scores <- fscores_longitudinal_grmtree(ltree)
#'   # Scores for node 2
#'   head(scores[["2"]])
#'
#' @seealso \code{\link{longitudinal_grmtree}} for fitting the tree,
#'   \code{\link{generate_node_scores_dataset}} to generate node assignment and
#'   factor scores
#'
#' @export
#' @importFrom mirt fscores
#' @importFrom partykit nodeids nodeapply
fscores_longitudinal_grmtree <- function(object, method = "EAP", ...) {

  if (!inherits(object, "longitudinal_grmtree")) {
    stop("'object' must be a longitudinal_grmtree object")
  }
  if (!method %in% c("EAP", "MAP", "ML", "WLE")) {
    stop("method must be one of: 'EAP', 'MAP', 'ML', or 'WLE'")
  }

  terminal_nodes <- partykit::nodeids(object, terminal = TRUE)
  scores_list <- vector("list", length(terminal_nodes))
  names(scores_list) <- as.character(terminal_nodes)

  for (node_id in terminal_nodes) {
    message("Processing node: ", node_id)

    # Extract fitted model
    node_model <- tryCatch(
      partykit::nodeapply(
        object, ids = node_id,
        FUN = function(nd) nd$info$object
      )[[1]],
      error = function(e) NULL
    )
    if (is.null(node_model)) {
      warning("No model object in node ", node_id)
      next
    }

    # Compute factor scores — returns N x 2 matrix for two-factor model
    node_scores <- suppressWarnings(tryCatch({
      result <- mirt::fscores(node_model, method = method)
      if (is.matrix(result) && ncol(result) >= 2) {
        df <- data.frame(
          Theta_T1 = result[, 1],
          Theta_T2 = result[, 2]
        )
      } else if (is.matrix(result) && ncol(result) == 1) {
        df <- data.frame(Theta_T1 = result[, 1], Theta_T2 = NA)
        warning("Only one factor score column returned for node ", node_id)
      } else {
        df <- data.frame(Theta_T1 = result, Theta_T2 = NA)
      }
      df
    }, error = function(e) {
      warning("Factor score computation failed for node ", node_id, ": ", e$message)
      NULL
    }))

    if (!is.null(node_scores)) {
      scores_list[[as.character(node_id)]] <- node_scores
    }
  }

  scores_list <- Filter(Negate(is.null), scores_list)
  if (length(scores_list) == 0) {
    stop("No factor scores were successfully computed for any node.")
  }
  return(scores_list)
}
