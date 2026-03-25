#' Control Parameters for GRM Trees
#'
#' Creates a control object for `grmtree` containing various parameters that
#' control the tree growing process.
#'
#' @param minbucket Minimum number of observations in a terminal node (default:
#'   20).
#' @param p_adjust Method for p-value adjustment. One of: "none", "bonferroni",
#'   "holm", "BH", "BY", "hochberg", or "hommel" (default: "none").
#' @param alpha Significance level for splitting (default: 0.05).
#' @param ... Additional arguments passed to `partykit::mob_control()`.
#' @param initial_alpha For post-hoc adjustment methods (holm, BH, BY, hochberg,
#'   hommel), the significance threshold for initial tree construction before
#'   pruning. Must satisfy \code{alpha < initial_alpha < 1}. Default is
#'   \code{min(3 * alpha, 0.20)}. Lower values produce more conservative results
#'   but run faster; higher values provide more power but require more
#'   computation and may increase Type I error. Ignored for "none" and
#'   "bonferroni" methods.
#'
#' @return A list of control parameters with class `grmtree_control`.
#'
#' @examples
#' # Use Bonferroni correction with alpha = 0.01
#' ctrl <- grmtree.control(p_adjust = "bonferroni", alpha = 0.01)
#'
#' @seealso \code{\link{grmtree}} fits a Graded Response Model Tree
#'
#' @export
#' @importFrom partykit mob_control

grmtree.control <- function(minbucket = 20, p_adjust = "none", alpha = 0.05,
                            initial_alpha = NULL, ...) {


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

  # Set default initial_alpha
  if (is.null(initial_alpha)) {
    if (p_adjust %in% c("holm", "BH", "BY", "hochberg", "hommel")) {
      initial_alpha <- min(3 * alpha, 0.20)  # Heuristic: 3x target, capped
    } else {
      initial_alpha <- alpha  # Not used for "none" and "bonferroni"
    }
  }

  # Validate initial_alpha
  if (p_adjust %in% c("holm", "BH", "BY", "hochberg", "hommel")) {
    if (!is.numeric(initial_alpha) || initial_alpha <= alpha || initial_alpha >= 1) {
      stop("'initial_alpha' must satisfy: alpha < initial_alpha < 1")
    }
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
  control$initial_alpha <- initial_alpha
  class(control) <- c("grmtree_control", class(control))
  return(control)
}

# Internal helper function - adjust p-values and prune tree (CORRECTED)
# @keywords internal
.adjust_and_prune_tree <- function(tree, method, alpha, verbose = FALSE) {

  all_nodes <- partykit::nodeids(tree)
  terminal_nodes <- partykit::nodeids(tree, terminal = TRUE)
  inner_nodes <- setdiff(all_nodes, terminal_nodes)

  if (length(inner_nodes) == 0) return(tree)

  # ============================================================
  # Step 1: Collect ALL p-values from ALL covariates at ALL nodes
  # ============================================================
  # This is the key fix. Previously, we took min(p-values) per node
  # first, which discarded the within-node multiplicity before
  # adjustment. Now we keep every individual covariate test.

  pval_node_ids  <- integer(0)
  pval_cov_names <- character(0)
  pval_raw       <- numeric(0)

  for (node_id in inner_nodes) {
    tryCatch({
      test_matrix <- strucchange::sctest(tree, node = node_id)

      if (!is.null(test_matrix) && "p.value" %in% rownames(test_matrix)) {
        pvals <- test_matrix["p.value", ]
        pvals <- pvals[!is.na(pvals)]

        if (length(pvals) > 0) {
          # Replace exact 0 with machine epsilon
          pvals[pvals == 0] <- .Machine$double.eps

          pval_node_ids  <- c(pval_node_ids,  rep(node_id, length(pvals)))
          pval_cov_names <- c(pval_cov_names, names(pvals))
          pval_raw       <- c(pval_raw,       unname(pvals))
        }
      }
    }, error = function(e) NULL)
  }

  if (length(pval_raw) == 0) return(tree)

  # ============================================================
  # Step 2: Apply GLOBAL p-value adjustment across ALL tests
  # ============================================================
  # With P covariates and K inner nodes, we now adjust K*P values.
  # This properly accounts for BOTH within-node multiplicity
  # (testing multiple covariates) and across-node multiplicity
  # (testing at multiple nodes in the tree).

  pval_adj <- stats::p.adjust(pval_raw, method = method)

  if (verbose) {
    cat(sprintf("  Post-hoc adjustment (%s): %d total tests across %d nodes\n",
                method, length(pval_raw), length(inner_nodes)))
  }

  # ============================================================
  # Step 3: For each node, check if the split survives
  # ============================================================
  # MOB split on the covariate with the smallest raw p-value.
  # Since p.adjust preserves ordering, that covariate also has
  # the smallest adjusted p-value at its node. So min(adj_p)
  # per node is equivalent to checking the split variable.

  nodes_to_prune <- integer(0)

  for (node_id in inner_nodes) {
    idx <- which(pval_node_ids == node_id)
    if (length(idx) > 0) {
      min_adj_p <- min(pval_adj[idx])
      if (min_adj_p >= alpha) {
        nodes_to_prune <- c(nodes_to_prune, node_id)
      }

      if (verbose) {
        cat(sprintf("    Node %d: min raw p = %.6f, min adj p = %.6f -> %s\n",
                    node_id, min(pval_raw[idx]), min_adj_p,
                    ifelse(min_adj_p < alpha, "KEEP", "PRUNE")))
      }
    }
  }

  if (length(nodes_to_prune) == 0) return(tree)

  # ============================================================
  # Step 4: Prune non-significant nodes (deepest first)
  # ============================================================
  nodes_to_prune <- sort(nodes_to_prune, decreasing = TRUE)

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
