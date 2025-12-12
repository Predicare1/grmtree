#' Control Parameters for GRM Trees
#'
#' Creates a control object for `grmtree` containing various parameters
#' that control the tree growing process.
#'
#' @param minbucket Minimum number of observations in a terminal node (default: 20).
#' @param p_adjust Method for p-value adjustment. One of: "none", "bonferroni",
#'   "holm", "BH", "BY", "hochberg", or "hommel" (default: "none").
#' @param alpha Significance level for splitting (default: 0.05).
#' @param ... Additional arguments passed to `partykit::mob_control()`.
#'
#' @return A list of control parameters with class `grmtree_control`.
#'
#' @examples
#' # Use Bonferroni correction with alpha = 0.01
#' ctrl <- grmtree.control(p_adjust = "bonferroni", alpha = 0.01)
#'
#' @seealso
#' \code{\link{grmtree}} fits a Graded Response Model Tree
#'
#' @export
#' @importFrom partykit mob_control

grmtree.control <- function(minbucket = 20, p_adjust = "none", alpha = 0.05, ...) {


  # Validate minbucket
  if (!is.numeric(minbucket)) {
    stop("'minbucket' must be numeric")
  }
  if (minbucket < 1) {
    stop("'minbucket' must be at least 1")
  }
  # Validate alpha
  if (!is.numeric(alpha)) {
    stop("'alpha' must be numeric")
  }
  if (alpha <= 0 || alpha >= 1) {
    stop("'alpha' must be between 0 and 1")
  }
  # Validate p_adjust method
  p_adjust_methods <- c("none", "bonferroni", "holm", "BH", "BY", "hochberg", "hommel")
  if (!p_adjust %in% p_adjust_methods) {
    stop("'p_adjust' must be one of: ", paste(p_adjust_methods, collapse = ", "))
  }
  # Create control object
  control <- tryCatch(
    partykit::mob_control(
      minbucket = minbucket,
      bonferroni = (p_adjust == "bonferroni"),
      alpha = alpha,
      ytype = "matrix",
      ...
    ),
    error = function(e) {
      stop("Error creating control parameters: ", e$message)
    }
  )
  # Add custom p_adjust method
  control$p_adjust <- p_adjust
  class(control) <- c("grmtree_control", class(control))
  return(control)
}

# Internal helper function - adjust p-values and prune tree
# @keywords internal
.adjust_and_prune_tree <- function(tree, method, alpha, verbose = FALSE) {

  all_nodes <- partykit::nodeids(tree)
  terminal_nodes <- partykit::nodeids(tree, terminal = TRUE)
  inner_nodes <- setdiff(all_nodes, terminal_nodes)

  if (length(inner_nodes) == 0) return(tree)

  # Collect p-values from each inner node using sctest
  pval_list <- list()

  for (node_id in inner_nodes) {
    tryCatch({
      # sctest returns a MATRIX with rownames "statistic" and "p.value"
      test_matrix <- strucchange::sctest(tree, node = node_id)

      if (!is.null(test_matrix) && "p.value" %in% rownames(test_matrix)) {
        pvals <- test_matrix["p.value", ]
        pvals <- pvals[!is.na(pvals)]

        if (length(pvals) > 0) {
          # Replace exact 0 with machine epsilon
          pvals[pvals == 0] <- .Machine$double.eps
          min_pval <- min(pvals)
          pval_list[[as.character(node_id)]] <- min_pval
        }
      }
    }, error = function(e) NULL)
  }

  if (length(pval_list) == 0) return(tree)

  # Convert to vectors
  node_ids <- as.numeric(names(pval_list))
  raw_pvals <- unlist(pval_list)

  # Apply p-value adjustment
  adj_pvals <- stats::p.adjust(raw_pvals, method = method)

  # Find nodes where adjusted p >= alpha (should prune)
  nodes_to_prune <- node_ids[adj_pvals >= alpha]

  if (length(nodes_to_prune) == 0) return(tree)

  # Sort descending (prune deepest nodes first)
  nodes_to_prune <- sort(nodes_to_prune, decreasing = TRUE)

  # Prune each node
  for (node_id in nodes_to_prune) {
    current_inner <- setdiff(partykit::nodeids(tree),
                             partykit::nodeids(tree, terminal = TRUE))
    if (node_id %in% current_inner) {
      tree <- .prune_single_node(tree, node_id)
    }
  }

  return(tree)
}

# Internal helper function - prune a single node
# @keywords internal
.prune_single_node <- function(tree, node_id) {

  current_terminals <- partykit::nodeids(tree, terminal = TRUE)

  # Find which terminal nodes are under this node
  terminals_under_node <- c()
  for (term_id in current_terminals) {
    parent_path <- .get_parent_path(tree, term_id)
    if (node_id %in% parent_path) {
      terminals_under_node <- c(terminals_under_node, term_id)
    }
  }

  # Keep all terminals NOT under this node, plus this node itself
  keep_terminals <- setdiff(current_terminals, terminals_under_node)
  keep_terminals <- c(keep_terminals, node_id)
  keep_terminals <- sort(unique(keep_terminals))

  tryCatch({
    partykit::nodeprune(tree, ids = keep_terminals)
  }, error = function(e) {
    tree
  })
}

# Internal helper function - get parent path for a node
# @keywords internal
.get_parent_path <- function(tree, node_id) {
  path <- c(node_id)
  current <- node_id

  while (current > 1) {
    for (possible_parent in partykit::nodeids(tree)) {
      node <- tree[[possible_parent]]
      if (!is.null(node$kids)) {
        kid_ids <- sapply(node$kids, function(k) k$id)
        if (current %in% kid_ids) {
          path <- c(possible_parent, path)
          current <- possible_parent
          break
        }
      }
    }
    if (current == path[1]) break
  }

  return(path)
}
