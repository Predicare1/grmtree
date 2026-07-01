
#' Extract Threshold Parameters from Longitudinal GRM Tree
#'
#' Extracts threshold (difficulty) parameters for each unique item from all
#' terminal nodes of a longitudinal GRM tree. Only T1 item parameters are
#' returned since the constrained model enforces equal thresholds across T1 and
#' T2 within each node.
#'
#' @param object A \code{longitudinal_grmtree} object.
#' @param node Optional vector of node IDs. If NULL (default), extracts from all
#'   terminal nodes.
#' @param clean_names Logical. If TRUE (default), clean item names by removing
#'   common prefixes (e.g., "resp_wideMOS_") and suffixes (e.g., "_BL", "_T1").
#' @param ... Additional arguments (currently unused).
#'
#' @return A data.frame with columns:
#'   \describe{
#'     \item{Node}{Terminal node ID}
#'     \item{Item}{Item name (cleaned if \code{clean_names = TRUE})}
#'     \item{b1, b2, ..., bK}{Threshold parameters for each category boundary}
#'   }
#'   Contains \code{n_items} rows per node (not \code{2 * n_items}).
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
#'   # Print the threshold parameters
#'   thresholds <- threshpar_longitudinal_grmtree(ltree)
#'   print(thresholds)
#'
#'   # Using the raw names
#'   threshpar_longitudinal_grmtree(ltree, clean_names = FALSE)  # raw names
#'
#' @seealso \code{\link{longitudinal_grmtree}} for Phase 1 (tree fitting),
#'   \code{\link{discrpar_longitudinal_grmtree}} for extracting discrimination
#'   parameters for longitudinal GRMTree,
#'   \code{\link{itempar_longitudinal_grmtree}} for extracting item parameters
#'   for longitudinal GRMTree
#'
#' @export
#' @importFrom partykit nodeids nodeapply
#' @importFrom mirt coef
threshpar_longitudinal_grmtree <- function(object, node = NULL,
                                           clean_names = TRUE, ...) {
  if (!inherits(object, "longitudinal_grmtree")) {
    stop("'object' must be a longitudinal_grmtree object")
  }

  n_items <- object$info$n_items
  if (is.null(n_items)) {
    stop("Tree object does not contain n_items.")
  }

  if (is.null(node)) {
    node <- partykit::nodeids(object, terminal = TRUE)
    if (length(node) == 0) stop("No terminal nodes found")
  }

  thresholds <- do.call("rbind", lapply(node, function(n) {
    model <- partykit::nodeapply(
      object, ids = n, FUN = function(nd) nd$info$object
    )[[1]]

    coef_model <- mirt::coef(model, IRTpars = TRUE, simplify = TRUE)
    all_items <- coef_model$items
    thresh_cols <- grep("^b", colnames(all_items))

    if (length(thresh_cols) == 0) {
      stop("No threshold parameters found in node ", n)
    }

    # T1 items only (rows 1:n_items)
    thresh <- all_items[1:n_items, thresh_cols, drop = FALSE]
    items <- rownames(all_items)[1:n_items]

    if (clean_names) {
      items <- gsub("^.*MOS_|^.*resp_wide|^.*resp", "", items)
      items <- gsub("_BL$|_Pre$|_T1$|_baseline$", "", items)
    }

    thresh_df <- as.data.frame(thresh)
    thresh_df$Item <- items
    thresh_df$Node <- n
    thresh_df <- thresh_df[, c("Node", "Item", colnames(thresh)), drop = FALSE]
    return(thresh_df)
  }))

  rownames(thresholds) <- NULL
  return(thresholds)
}
