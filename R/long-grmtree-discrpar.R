
#' Extract Discrimination Parameters from Longitudinal GRM Tree
#'
#' Extracts discrimination (slope) parameters for each unique item from all
#' terminal nodes of a longitudinal GRM tree. The longitudinal model uses
#' columns "a1" (T1 loading) and "a2" (T2 loading); this function extracts the
#' T1 discrimination since a1 = a2 under the equality constraint.
#'
#' @param object A \code{longitudinal_grmtree} object.
#' @param node Optional vector of node IDs. If NULL (default), extracts from all
#'   terminal nodes.
#' @param clean_names Logical. If TRUE (default), clean item names.
#' @param ... Additional arguments (currently unused).
#'
#' @return A data.frame with columns:
#'   \describe{
#'     \item{Node}{Terminal node ID}
#'     \item{Item}{Item name}
#'     \item{Discrimination}{Discrimination parameter (a1 from T1 items)}
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
#'   # Print the discrimination parameters
#'   discr <- discrpar_longitudinal_grmtree(ltree)
#' print(discr)
#'
#' @seealso \code{\link{longitudinal_grmtree}} for Phase 1 (tree fitting),
#'   \code{\link{threshpar_longitudinal_grmtree}} for extracting threshold
#'   parameters for longitudinal GRMTree,
#'   \code{\link{itempar_longitudinal_grmtree}} for extracting item parameters
#'   for longitudinal GRMTree
#'
#' @export
#' @importFrom partykit nodeids nodeapply
#' @importFrom mirt coef
discrpar_longitudinal_grmtree <- function(object, node = NULL,
                                          clean_names = TRUE, ...) {
  if (!inherits(object, "longitudinal_grmtree")) {
    stop("'object' must be a longitudinal_grmtree object")
  }

  n_items <- object$info$n_items
  if (is.null(n_items)) stop("Tree object does not contain n_items.")

  if (is.null(node)) {
    node <- partykit::nodeids(object, terminal = TRUE)
    if (length(node) == 0) stop("No terminal nodes found")
  }

  discr_list <- lapply(node, function(n) {
    model <- partykit::nodeapply(
      object, ids = n, FUN = function(nd) nd$info$object
    )[[1]]

    coef_model <- mirt::coef(model, IRTpars = TRUE, simplify = TRUE)
    all_items <- coef_model$items

    # Longitudinal model has "a1" and "a2" columns, not "a"
    if ("a1" %in% colnames(all_items)) {
      discr <- all_items[1:n_items, "a1"]
    } else if ("a" %in% colnames(all_items)) {
      discr <- all_items[1:n_items, "a"]
    } else {
      stop("No discrimination parameters (a1 or a) found in node ", n)
    }

    items <- rownames(all_items)[1:n_items]
    if (clean_names) {
      items <- gsub("^.*MOS_|^.*resp_wide|^.*resp", "", items)
      items <- gsub("_BL$|_Pre$|_T1$|_baseline$", "", items)
    }

    data.frame(
      Node = n,
      Item = items,
      Discrimination = discr,
      row.names = NULL,
      stringsAsFactors = FALSE
    )
  })

  discrimination <- do.call("rbind", discr_list)
  rownames(discrimination) <- NULL
  return(discrimination)
}
