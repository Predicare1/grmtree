
#' Extract Item Parameters from Longitudinal GRM Tree
#'
#' Extracts both discrimination and threshold parameters for each unique item
#' from all terminal nodes, combining them into a single data frame with an
#' average threshold column. Only T1 items are returned.
#'
#' @param object A \code{longitudinal_grmtree} object.
#' @param node Optional vector of node IDs. If NULL, all terminal nodes.
#' @param clean_names Logical. If TRUE (default), clean item names.
#' @param ... Additional arguments (currently unused).
#'
#' @return A data.frame with columns:
#'   \describe{
#'     \item{Node}{Terminal node ID}
#'     \item{Item}{Item name}
#'     \item{Discrimination}{Discrimination parameter}
#'     \item{AvgThreshold}{Mean of all threshold parameters for the item}
#'     \item{b1, b2, ..., bK}{Individual threshold parameters}
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
#'   # Print the item parameters
#'   items <- itempar_longitudinal_grmtree(ltree)
#'   print(items)
#'
#' @seealso \code{\link{longitudinal_grmtree}} for Phase 1 (tree fitting),
#'   \code{\link{discrpar_longitudinal_grmtree}} for extracting discrimination
#'   parameters for longitudinal GRMTree,
#'   \code{\link{threshpar_longitudinal_grmtree}} for extracting threshold
#'   parameters for longitudinal GRMTree
#'
#' @export
itempar_longitudinal_grmtree <- function(object, node = NULL,
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

  item_list <- lapply(node, function(n) {
    model <- partykit::nodeapply(
      object, ids = n, FUN = function(nd) nd$info$object
    )[[1]]

    coef_model <- mirt::coef(model, IRTpars = TRUE, simplify = TRUE)
    all_items <- coef_model$items

    # Discrimination
    if ("a1" %in% colnames(all_items)) {
      discr <- all_items[1:n_items, "a1"]
    } else if ("a" %in% colnames(all_items)) {
      discr <- all_items[1:n_items, "a"]
    } else {
      stop("No discrimination parameters found in node ", n)
    }

    # Thresholds
    thresh_cols <- grep("^b", colnames(all_items))
    if (length(thresh_cols) == 0) stop("No threshold parameters in node ", n)
    thresholds <- all_items[1:n_items, thresh_cols, drop = FALSE]
    avg_thresh <- rowMeans(thresholds, na.rm = TRUE)

    items <- rownames(all_items)[1:n_items]
    if (clean_names) {
      items <- gsub("^.*MOS_|^.*resp_wide|^.*resp", "", items)
      items <- gsub("_BL$|_Pre$|_T1$|_baseline$", "", items)
    }

    item_df <- data.frame(
      Node = n,
      Item = items,
      Discrimination = discr,
      AvgThreshold = avg_thresh,
      stringsAsFactors = FALSE
    )
    # Add individual thresholds
    thresh_df <- as.data.frame(thresholds)
    rownames(thresh_df) <- NULL
    item_df <- cbind(item_df, thresh_df)

    return(item_df)
  })

  items <- do.call("rbind", item_list)
  rownames(items) <- NULL
  return(items)
}
