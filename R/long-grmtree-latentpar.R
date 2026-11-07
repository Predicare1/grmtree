
#' Extract Latent Trait Parameters from Longitudinal GRM Tree
#'
#' Extracts the latent trait distribution parameters from each terminal node of
#' a longitudinal GRM tree: the T2 latent mean shift (\eqn{\mu_{T2}}), the T2
#' latent variance (\eqn{\sigma^2_{T2}}), and the T1-T2 correlation.
#'
#' @param object A \code{longitudinal_grmtree} object.
#' @param node Optional vector of node IDs. If NULL, all terminal nodes.
#' @param ... Additional arguments (currently unused).
#'
#' @return A data.frame with columns:
#'   \describe{
#'     \item{Node}{Terminal node ID}
#'     \item{n}{Sample size in the node}
#'     \item{mu_T2}{Latent mean at T2 (positive = improvement)}
#'     \item{sigma2_T2}{Latent variance at T2}
#'     \item{cor_T1_T2}{Correlation between T1 and T2 latent traits}
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
#'   # Print latent trait summary
#'   latent <- latentpar_longitudinal_grmtree(ltree)
#'   print(latent)
#'
#' @seealso \code{\link{longitudinal_grmtree}} for Phase 1 (tree fitting),
#'   \code{\link{fscores_longitudinal_grmtree}}, for computing factor scores for
#'   longitudinal GRMTree
#'
#' @export
#' @importFrom mirt coef
#' @importFrom partykit nodeids nodeapply info_node
latentpar_longitudinal_grmtree <- function(object, node = NULL, ...) {

  if (!inherits(object, "longitudinal_grmtree")) {
    stop("'object' must be a longitudinal_grmtree object")
  }

  if (is.null(node)) {
    node <- partykit::nodeids(object, terminal = TRUE)
  }

  results <- lapply(node, function(nd) {
    model <- partykit::nodeapply(
      object, ids = nd, FUN = function(n) n$info$object
    )[[1]]
    if (is.null(model)) {
      warning("No model object in node ", nd)
      return(data.frame(Node = nd, n = NA, mu_T2 = NA,
                        sigma2_T2 = NA, cor_T1_T2 = NA))
    }
    coefs <- mirt::coef(model, simplify = TRUE)
    means <- coefs$means
    cov_mat <- coefs$cov
    nobs <- partykit::nodeapply(
      object, ids = nd,
      FUN = function(n) partykit::info_node(n)$nobs
    )[[1]]
    data.frame(
      Node = nd,
      n = nobs,
      mu_T2 = round(means[2], 4),
      sigma2_T2 = round(cov_mat[2, 2], 4),
      cor_T1_T2 = round(cov_mat[1, 2] / sqrt(cov_mat[1, 1] * cov_mat[2, 2]), 4),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, results)
  rownames(out) <- NULL
  out
}
