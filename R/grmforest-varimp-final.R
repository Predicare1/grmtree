#' Permutation Variable Importance for a GRM Forest
#'
#' Quantifies how much each partitioning variable contributes to detected
#' differential item functioning, by measuring the loss in out-of-bag marginal
#' log-likelihood when that variable's values are randomly permuted.
#'
#' @param forest A `grmforest` object from [grmforest()].
#' @param method Importance type. Currently only `"permutation"`.
#' @param nperm Number of permutations averaged per variable per tree
#'   (default: 1). Values of 3--10 reduce Monte Carlo noise at proportional
#'   cost.
#' @param quadpts Number of quadrature points used to integrate the latent
#'   trait out of the likelihood (default: 61).
#' @param verbose Logical. Report progress (default: `FALSE`).
#' @param seed Random seed for the permutations (default: `NULL`).
#' @param n_cores Number of cores used to evaluate trees in parallel
#'   (default: 1). As in [grmforest()], all permutation draws are generated in
#'   the master process first, so results are independent of `n_cores`.
#'
#' @return A named numeric vector of class `varimp`, sorted decreasing. Values
#'   are on the log-likelihood scale: larger means the variable matters more.
#'   Values near zero mean the variable carries no information about item
#'   parameter instability; small negative values are ordinary sampling noise
#'   around zero.
#'
#' @details
#' For each tree, the out-of-bag rows are passed down the tree and each
#' respondent is scored under the graded response model held in the terminal
#' node they land in. The score is the **marginal** log-likelihood, integrating
#' the latent trait against a standard normal:
#' \deqn{\ell = \sum_i \log \int \prod_j P(Y_{ij} = y_{ij} \mid \theta)
#'   \phi(\theta) \, d\theta}
#' evaluated by quadrature. The variable is then permuted within the
#' out-of-bag rows, respondents are re-routed through the tree, and the
#' likelihood is recomputed. Importance is the mean across trees of the
#' **paired** difference (baseline minus permuted) for that tree, which has the
#' same expectation as differencing two separate averages but lower variance
#' and no sensitivity to which trees happen to be usable.
#'
#' Category probabilities are obtained from [mirt::probtrace()] rather than
#' reconstructed from coefficients, so the correct parameterization is applied
#' for whatever was fitted.
#'
#' @section Sparse response categories:
#' In large instruments some items may have categories that are empty within a
#' particular terminal node, in which case `mirt` collapses them and the node
#' model's category count no longer matches the full dataset. Such items are
#' skipped for that node, with a single note at the end reporting how many were
#' affected. Because importance uses paired within-tree differences, this
#' reduces precision but does not bias the result. Persistent notes are a
#' signal to collapse sparse categories deliberately before fitting, or to
#' raise `minbucket`.
#'
#' @examplesIf interactive()
#' library(grmtree)
#' library(hlt)
#' data("asti", package = "hlt")
#' asti$resp <- data.matrix(asti[, 1:4])
#'
#' ## Fit the GRM Forest
#' forest <- grmforest(resp ~ gender + group, data = asti,
#' control = grmforest.control(n_tree = 10, seed = 123))
#'
#' importance <- varimp(forest, seed = 123)
#'
#' ## Print and plot the variable importance scores
#' print(importance)
#' plot(importance)
#'
#' #' ## Variable importance for a longitudinal forest (same call)
#'   data("grmtree_long_data", package = "grmtree")
#'   items_t1 <- c("MOS_Listen", "MOS_Info", "MOS_Advice_Crisis", "MOS_Confide",
#'                 "MOS_Advice_Want", "MOS_Fears", "MOS_Personal", "MOS_Understand")
#'   ld <- prepare_longitudinal_data(
#'     data = grmtree_long_data, items_t1 = items_t1,
#'     items_t2 = paste0(items_t1, "_year1"),
#'     covariates = c("sex", "age", "education"))
#'   lforest <- grmforest(resp_wide ~ sex + age + education, data = ld,
#'     control = grmforest.control(n_tree = 20, seed = 123,
#'                 control = grmtree.control(minbucket = 200)),
#'     tree_fun = longitudinal_grmtree, tree_args = list(n_items = 8))
#'   importance <- varimp(lforest, seed = 123)
#'   print(importance)
#'
#' @seealso \code{\link{grmtree}} fits a Graded Response Model Tree,
#'   \code{\link{grmforest}} for GRM Forests, \code{\link{grmforest.control}}
#'   creates a control object for `grmforest`, \code{\link{plot.varimp}} creates
#'   a bar plot of variable importance scores
#'
#' @export
#' @importFrom stats predict
varimp <- function(forest, method = "permutation", nperm = 1L, quadpts = 61L,
                   verbose = FALSE, seed = NULL, n_cores = 1L) {

  if (!inherits(forest, "grmforest")) {
    stop("'forest' must be a grmforest object created by grmforest()")
  }
  if (method != "permutation") {
    stop("Only permutation importance is implemented")
  }
  if (!is.numeric(nperm) || nperm < 1) stop("'nperm' must be a positive integer")
  nperm <- as.integer(nperm)

  ## Backward compatibility with forests saved before oob_indices existed
  if (is.null(forest$oob_indices)) {
    stop("This forest was created by an older version of grmforest() and ",
         "stores out-of-bag data rather than indices. Refit the forest, or ",
         "supply oob_indices manually.")
  }

  var_names <- all.vars(forest$formula[[3L]])
  if (length(var_names) == 0L) stop("No partitioning variables found in formula")

  ## Canonical response coding, from the full data
  full <- .grm_eval_frame(forest$formula, forest$data)
  levels_list <- .grm_levels(full$y)
  quad <- .grm_quad(quadpts)

  ## All permutation orderings are drawn up front, so n_cores is irrelevant
  if (!is.null(seed)) set.seed(seed)
  ntree <- length(forest$trees)
  perm_orders <- lapply(seq_len(ntree), function(i) {
    n_oob <- length(forest$oob_indices[[i]])
    lapply(var_names, function(v) {
      replicate(nperm, sample.int(max(n_oob, 1L)), simplify = FALSE)
    })
  })

  skipped <- new.env(parent = emptyenv())
  skipped$items <- integer(0)

  eval_tree <- function(i) {
    tree <- forest$trees[[i]]
    oob  <- forest$oob_indices[[i]]
    if (is.null(tree) || length(oob) == 0L) {
      return(c(baseline = NA_real_,
               stats::setNames(rep(NA_real_, length(var_names)), var_names)))
    }
    oob_data <- forest$data[oob, , drop = FALSE]

    base_ll <- .grm_tree_loglik(tree, forest$formula, oob_data,
                                levels_list, quad, skipped)
    if (!is.finite(base_ll)) {
      return(c(baseline = NA_real_,
               stats::setNames(rep(NA_real_, length(var_names)), var_names)))
    }

    drops <- vapply(seq_along(var_names), function(vi) {
      v <- var_names[vi]
      if (!v %in% names(oob_data)) return(NA_real_)
      lls <- vapply(seq_len(nperm), function(p) {
        pd <- oob_data
        ord <- perm_orders[[i]][[vi]][[p]]
        ord <- ord[ord <= nrow(pd)]
        pd[[v]] <- pd[[v]][ord]
        .grm_tree_loglik(tree, forest$formula, pd, levels_list, quad, skipped)
      }, numeric(1L))
      base_ll - mean(lls, na.rm = TRUE)
    }, numeric(1L))

    c(baseline = base_ll, stats::setNames(drops, var_names))
  }

  if (verbose) message("Evaluating ", ntree, " trees ...")
  res <- .grm_lapply(seq_len(ntree), eval_tree, n_cores = n_cores,
                     export = c(".grm_tree_loglik", ".grm_node_loglik", ".lgrm_node_loglik",
                                ".grm_eval_frame", ".grm_levels",
                                ".row_logsumexp", ".grm_quad"),
                     export_env = environment())
  res <- do.call(rbind, res)

  importance <- colMeans(res[, var_names, drop = FALSE], na.rm = TRUE)
  importance[is.nan(importance)] <- 0

  n_used <- sum(is.finite(res[, "baseline"]))
  if (n_used == 0L) {
    warning("No trees produced a usable out-of-bag likelihood; ",
            "all importance scores set to 0")
    importance[] <- 0
  } else if (verbose) {
    message("Mean out-of-bag log-likelihood: ",
            format(mean(res[, "baseline"], na.rm = TRUE)),
            " (", n_used, " of ", ntree, " trees)")
  }

  if (length(skipped$items) > 0L) {
    message(length(skipped$items), " item(s) were skipped in at least one ",
            "terminal node because the node model had fewer response ",
            "categories than the full data (sparse categories). Consider ",
            "collapsing rare categories before fitting, or raising minbucket.")
  }

  importance <- sort(importance, decreasing = TRUE)
  attr(importance, "n_tree") <- ntree
  attr(importance, "baseline") <- mean(res[, "baseline"], na.rm = TRUE)
  class(importance) <- c("varimp", "numeric")
  importance
}


#' Print Method for Variable Importance
#'
#' @param x A `varimp` object.
#' @param ... Currently unused.
#' @return Invisibly returns `x`.
#' @export
print.varimp <- function(x, ...) {
  cat("GRM Forest variable importance (permutation)\n")
  if (!is.null(attr(x, "n_tree"))) {
    cat("  Trees:", attr(x, "n_tree"),
        " Mean OOB log-likelihood:", format(attr(x, "baseline"), digits = 6), "\n")
  }
  cat("\n")
  print(round(unclass(x), 3L))
  invisible(x)
}
