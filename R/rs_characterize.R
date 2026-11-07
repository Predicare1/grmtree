################################################################################
##
## rs_characterize(): Post-hoc Response Shift Characterization
##
## After fitting longitudinal_grmtree() (Phase 1), this function tests for actual
## response shift WITHIN each terminal node by comparing the constrained model
## (item parameters equal across time) to an unconstrained model (item parameters
## free across time).
##
## Design (v2):
##   * Item-level testing runs ONLY in nodes whose omnibus RS survives the
##     across-node adjustment (RS_detected == TRUE), never on the raw p-value.
##   * DETECTION of an item uses a joint LRT (discrimination and thresholds),
##     adjusted across items; optional iterative purification (purify = TRUE).
##   * The TYPE of a flagged item (recalibration / reprioritization / both) is
##     read from the estimated IRT-metric changes relative to rs_threshold.
##     "None" means "not flagged".
##
## Author: Olayinka Arimoro
## Date: February 2026 (item-level redesign: 2026)
##
################################################################################

#' Characterize Response Shift Within Terminal Nodes of a Longitudinal GRM Tree
#'
#' After fitting a \code{\link{longitudinal_grmtree}} (Phase 1), this function
#' performs Phase 2: post-hoc response shift (RS) characterization within each
#' terminal node. For each subgroup identified by the tree it compares the
#' constrained model (no RS: item parameters equal across time) to an
#' unconstrained model (item parameters free across time) with a likelihood
#' ratio test (LRT); then, \emph{only in nodes where the omnibus test survives
#' the across-node adjustment}, it tests each item for RS and classifies the
#' \emph{type} of shift by nested LRTs on the item's parameter sets.
#'
#' @param object A \code{longitudinal_grmtree} object fitted by
#'   \code{\link{longitudinal_grmtree}}.
#' @param node Optional integer vector of terminal node IDs to analyze. If
#'   \code{NULL} (default), all terminal nodes are analyzed.
#' @param item_level Logical. If \code{TRUE} (default), perform item-level RS
#'   testing. It is performed \strong{only} for nodes whose omnibus RS is
#'   detected \emph{after} \code{global_p_adjust} (i.e. \code{RS_detected ==
#'   TRUE}); nodes that are not significant after adjustment are skipped
#'   entirely, so no item rows are produced for them.
#' @param alpha Numeric significance level for RS tests. Default \code{0.05}.
#'   Used for the omnibus decision (after \code{global_p_adjust}), the item
#'   detection decision (after \code{p_adjust}), and the component tests that
#'   assign the RS type.
#' @param p_adjust Character string specifying the method for adjusting the
#'   \strong{item detection} p-values \strong{within each node}. Controls the
#'   error rate when testing multiple items for RS within a single subgroup.
#'   Options: \code{"bonferroni"} (default), \code{"holm"}, \code{"BH"}
#'   (Benjamini-Hochberg), \code{"BY"}, \code{"fdr"}, \code{"hochberg"},
#'   \code{"hommel"}, or \code{"none"}. See \code{\link[stats]{p.adjust}}. FDR
#'   is recommended for the exploratory item search.
#' @param global_p_adjust Character string specifying the method for adjusting
#'   the omnibus RS p-values \strong{across terminal nodes}. Controls the
#'   family-wise error rate when testing RS in multiple subgroups. Options are
#'   the same as \code{p_adjust}. Default \code{"bonferroni"}. The
#'   \code{RS_detected} column is set from these adjusted p-values, and it is
#'   \code{RS_detected} that gates item-level testing. Only applied when there
#'   are two or more nodes.
#' @param rs_threshold Numeric. Used only when \code{type_method =
#'   "threshold"}: flagged items are labelled by comparing the estimated
#'   discrimination change and maximum threshold change with this value
#'   (default \code{0.3}). Ignored for the default \code{type_method = "wald"}.
#' @param purify Logical. If \code{TRUE}, item-level tests are purified
#'   iteratively: items flagged in one pass have their cross-occasion
#'   constraints released in the next, so that every item is tested against
#'   the non-flagged items only, until the set of flagged items no longer
#'   changes. This reduces false flags caused by response shift in other items
#'   (contamination). Default \code{FALSE} (single pass against the fully
#'   constrained model).
#' @param max_iter Maximum number of purification passes (default \code{10}).
#' @param type_method How flagged items are labelled. \code{"wald"} (default):
#'   separate Wald tests of the slope and of the thresholds, each with the
#'   other free (see Details); no cut-off. \code{"threshold"}: the magnitude
#'   rule of grmtree <= 0.3.0 based on \code{rs_threshold}.
#' @param verbose Logical. If \code{TRUE} (default), print progress messages
#'   for each node during processing.
#' @param ... Additional arguments passed to \code{\link[mirt]{mirt}} during
#'   model fitting.
#'
#' @return A list of class \code{"rs_characterization"} containing:
#'   \describe{
#'     \item{\code{global}}{A \code{data.frame} with one row per terminal node
#'       and columns:
#'       \describe{
#'         \item{\code{Node}}{Terminal node ID.}
#'         \item{\code{n}}{Sample size in the node.}
#'         \item{\code{LL_constrained}}{Log-likelihood of the constrained
#'           (no-RS) model.}
#'         \item{\code{LL_unconstrained}}{Log-likelihood of the unconstrained
#'           (RS-allowed) model.}
#'         \item{\code{LRT_chi2}}{Omnibus LRT statistic
#'           \eqn{-2(\ell_{con} - \ell_{uncon})}.}
#'         \item{\code{LRT_df}}{Degrees of freedom: the number of freed
#'           constraints minus 2, because the follow-up latent mean and variance
#'           are estimated in the constrained model but not identified in the
#'           unconstrained one.}
#'         \item{\code{LRT_p}}{Raw omnibus p-value.}
#'         \item{\code{LRT_p_adj}}{Omnibus p-value adjusted across nodes
#'           (present only when \code{global_p_adjust != "none"} and there are
#'           two or more nodes).}
#'         \item{\code{RS_detected}}{Logical: whether omnibus RS is detected at
#'           \code{alpha} after adjustment. \strong{This flag gates item-level
#'           testing.}}
#'         \item{\code{pseudo_R2}}{McFadden's pseudo-\eqn{R^2},
#'           \eqn{1 - \ell_{uncon}/\ell_{con}}.}
#'         \item{\code{AIC_diff}}{\eqn{AIC_{con} - AIC_{uncon}}; positive
#'           favours the unconstrained model.}
#'         \item{\code{mu_T2}}{Latent mean shift at T2 (constrained model).}
#'         \item{\code{sigma2_T2}}{Latent variance at T2.}
#'         \item{\code{cor_T1_T2}}{Test-retest correlation of the latent trait.}
#'         \item{\code{converged_constrained}, \code{converged_unconstrained}}{
#'           Convergence flags for the two models.}
#'       }
#'     }
#'     \item{\code{item_level}}{A \code{data.frame} of item-level results, one
#'       row per item \strong{only for RS-detected nodes} (\code{NULL} if no
#'       node was detected or \code{item_level = FALSE}):
#'       \describe{
#'         \item{\code{Node}, \code{Item}}{Node ID and item number (1..M).}
#'         \item{\code{LRT_chi2}, \code{LRT_df}, \code{LRT_p}}{The \emph{joint}
#'           item test (free this item's discrimination and thresholds vs the
#'           constrained model): statistic, df \eqn{= K} (1 discrimination plus
#'           \eqn{K-1} thresholds), and raw p-value. This is the item
#'           \emph{detection} test.}
#'         \item{\code{LRT_p_adj}}{Joint p-value adjusted across items within
#'           the node (\code{p_adjust}); an item is flagged when this is below
#'           \code{alpha}.}
#'         \item{\code{RS_type}}{\code{"Recalibration"}, \code{"Reprioritization"},
#'           \code{"Both"} or \code{"Undetermined"} for flagged items (see
#'           Details), \code{"None"} for items not flagged.}
#'         \item{\code{discr_diff}, \code{thresh_diff}}{Descriptive: the
#'           absolute T1-vs-T2 discrimination change and the maximum absolute
#'           threshold change, from the model in which the item is free.}
#'         \item{\code{W_slope}, \code{p_slope}, \code{W_location},
#'           \code{df_location}, \code{p_location}}{The slope and location
#'           Wald tests for flagged items (\code{type_method = "wald"}).}
#'         \item{\code{iterations}}{Number of purification passes.}
#'         \item{\code{RS_magnitude}}{\code{"large"}/\code{"small"} annotation
#'           against \code{rs_threshold} (\code{type_method = "threshold"}
#'           only; otherwise \code{NA}).}
#'       }
#'     }
#'     \item{\code{parameters}}{A named list (one element per node) of
#'       \code{constrained} and \code{unconstrained} item-parameter matrices
#'       (IRT parameterization).}
#'     \item{\code{n_items}, \code{alpha}, \code{p_adjust},
#'       \code{global_p_adjust}, \code{rs_threshold}}{The settings used.}
#'   }
#'
#' @details
#' ## The two-phase framework
#'
#' Phase 1 (\code{\link{longitudinal_grmtree}}) embeds a constrained
#' longitudinal GRM (item parameters equal across T1 and T2) in model-based
#' recursive partitioning, identifying subgroups that require different
#' \emph{constrained} measurement models. Phase 2 (this function) relaxes the
#' equality constraints within each subgroup and asks whether item parameters
#' actually change over time. This separates between-group measurement
#' heterogeneity (captured by the tree) from within-group temporal change
#' (response shift).
#'
#' ## Omnibus test and the gate on item-level testing
#'
#' For each terminal node, a constrained model (all item parameters equal across
#' waves) and an unconstrained model (all free) are fitted and compared by
#' \deqn{\chi^2 = -2(\ell_{con} - \ell_{uncon}), \qquad
#'       \mathrm{df} = \#\{\text{freed constraints}\} - 2.}
#' The omnibus p-values are then adjusted \strong{across nodes}
#' (\code{global_p_adjust}) and \code{RS_detected} is set from the adjusted
#' p-value. \strong{Item-level testing is performed only for nodes with}
#' \code{RS_detected == TRUE}: a node that is not significant after adjustment is
#' skipped, so it contributes no item rows. This is a deliberate two-pass design
#' (all omnibus tests first, then adjustment, then item-level only where
#' warranted) and it guarantees that the item-level results are consistent with
#' the omnibus decision.
#'
#' ## Item detection
#'
#' Within an RS-detected node, each item is first tested for \emph{any} shift by
#' a \emph{joint} LRT that frees that item's discrimination \emph{and}
#' thresholds against the constrained model (df \eqn{= K}: one discrimination
#' plus \eqn{K-1} thresholds). These joint p-values are adjusted across the
#' items in the node (\code{p_adjust}); an item is \strong{flagged} when its
#' adjusted joint p-value is below \code{alpha}. Unflagged items are
#' \code{"None"}.
#'
#' ## Type: two Wald tests, each with the other parameters free (default)
#'
#' Once items are flagged, one model is refitted with standard errors in which
#' the flagged items are free across occasions and all other items are
#' constrained (they act as anchors). For each flagged item two Wald tests are
#' computed in that model:
#' \itemize{
#'   \item \strong{slope test} (reprioritization): \eqn{a_{T1} = a_{T2}},
#'     df = 1;
#'   \item \strong{location test} (recalibration): \eqn{b_{k,T1} = b_{k,T2}}
#'     for all \eqn{K-1} thresholds, df = \eqn{K-1}. It is written without a
#'     ratio as \eqn{a_{T1} d_{k,T2} - a_{T2} d_{k,T1} = 0} (equivalent because
#'     \eqn{a > 0}), which the delta method approximates well.
#' }
#' Each test leaves the other set of parameters free. This matters because
#' \pkg{mirt} uses the slope-intercept form \eqn{d = -a\,b}: a pure slope
#' change moves the intercepts \eqn{d} but not the thresholds \eqn{b}, so a
#' test of \eqn{d} would wrongly react to it, while the test of \eqn{b} does not.
#' The label is \code{"Reprioritization"} if only the slope test is
#' significant, \code{"Recalibration"} if only the location test is,
#' \code{"Both"} if both are, and \code{"Undetermined"} if the item is flagged
#' by the joint test but neither component test is significant on its own
#' (the shift is real but cannot be attributed with confidence). No magnitude
#' cut-off is involved. The estimated changes (\code{discr_diff},
#' \code{thresh_diff}) are reported as descriptive information only.
#'
#' \code{type_method = "threshold"} restores the rule of grmtree <= 0.3.0,
#' which labels flagged items by comparing \code{discr_diff} and
#' \code{thresh_diff} with \code{rs_threshold}; it is kept for reproducing
#' earlier analyses.
#'
#' ## Hierarchical error control
#'
#' Two adjustment layers are applied: \code{global_p_adjust} across nodes for
#' "in which subgroups does RS occur?" (default Bonferroni), and \code{p_adjust}
#' across items within a node for "which items shift?" (default Bonferroni; FDR
#' recommended for exploration). The component tests that assign the type are a
#' post-hoc decomposition of an already-flagged item and are read at
#' \code{alpha}.
#'
#' ## Effect sizes
#'
#' McFadden's pseudo-\eqn{R^2} \eqn{= 1 - \ell_{uncon}/\ell_{con}} is inherently
#' small in IRT (~0.02) and is best compared across nodes. \eqn{AIC_{con} -
#' AIC_{uncon}} is positive when the unconstrained model is preferred after the
#' parsimony penalty. Item-level \code{discr_diff} and \code{thresh_diff} give
#' the raw magnitude of the T1-vs-T2 parameter changes.
#'
#' @references
#' Oort, F. J. (2005). Using structural equation modeling to detect response
#' shifts and true change. \emph{Quality of Life Research}, 14(3), 587--598.
#'
#' Sprangers, M. A. G., & Schwartz, C. E. (1999). Integrating response shift
#' into health-related quality of life research: a theoretical model.
#' \emph{Social Science & Medicine}, 48(11), 1507--1515.
#'
#' Benjamini, Y., & Hochberg, Y. (1995). Controlling the false discovery rate:
#' a practical and powerful approach to multiple testing. \emph{Journal of the
#' Royal Statistical Society: Series B}, 57(1), 289--300.
#'
#' @author Olayinka Imisioluwa Arimoro \email{olayinka.arimoro@ucalgary.ca},
#'   Lisa M. Lix, Tolulope T. Sajobi
#'
#' @examplesIf interactive()
#' library(grmtree)
#' data("grmtree_long_data", package = "grmtree")
#' items_t1 <- c("MOS_Listen", "MOS_Info", "MOS_Advice_Crisis", "MOS_Confide",
#'               "MOS_Advice_Want", "MOS_Fears", "MOS_Personal", "MOS_Understand")
#' ld <- prepare_longitudinal_data(
#'   data = grmtree_long_data, items_t1 = items_t1,
#'   items_t2 = paste0(items_t1, "_year1"),
#'   covariates = c("sex", "age", "residency", "job",
#'                  "education", "comorbidity_count", "ever_smoker"))
#' ltree <- longitudinal_grmtree(
#'   resp_wide ~ sex + age + residency + job +
#'     education + comorbidity_count + ever_smoker,
#'   data = ld, n_items = 8, control = grmtree.control(minbucket = 200))
#'
#' # Default: Bonferroni across nodes and within-node; type from estimated changes
#' rs <- rs_characterize(ltree)
#' print(rs)
#'
#' # FDR for the item search, Bonferroni across nodes
#' rs <- rs_characterize(ltree, p_adjust = "fdr", global_p_adjust = "bonferroni")
#'
#' rs$global      # omnibus per node (RS_detected gates the item tests)
#' rs$item_level  # item detection + type + effect sizes, for detected nodes only
#'
#' @seealso
#'   \code{\link{longitudinal_grmtree}} for Phase 1 (tree fitting),
#'   \code{\link{print.rs_characterization}} for the print method,
#'   \code{\link{prepare_longitudinal_data}} for data preparation,
#'   \code{\link[stats]{p.adjust}} for p-value adjustment methods
#'
#' @export
#' @importFrom mirt mirt mirt.model coef logLik extract.mirt
#' @importFrom partykit nodeids data_party
#' @importFrom stats pchisq na.omit p.adjust formula AIC
rs_characterize <- function(object, node = NULL,
                            item_level = TRUE, alpha = 0.05,
                            p_adjust = "bonferroni",
                            global_p_adjust = "bonferroni",
                            rs_threshold = 0.3,
                            purify = FALSE, max_iter = 10L,
                            type_method = c("wald", "threshold"),
                            verbose = TRUE, ...) {
  type_method <- match.arg(type_method)
  # ---- Input validation ----

  if (!inherits(object, "longitudinal_grmtree"))
    stop("'object' must be a longitudinal_grmtree object")
  valid_methods <- c("none", "bonferroni", "holm", "BH", "BY", "fdr",
                     "hochberg", "hommel")
  if (!p_adjust %in% valid_methods)
    stop("'p_adjust' must be a valid p.adjust method or 'none'")
  if (!global_p_adjust %in% valid_methods)
    stop("'global_p_adjust' must be a valid p.adjust method or 'none'")
  n_items <- object$info$n_items
  if (is.null(n_items))
    stop("Tree object does not contain n_items. Was it fit with longitudinal_grmtree()?")
  if (is.null(node)) node <- partykit::nodeids(object, terminal = TRUE)

  # ---- Identify the response variable name from the formula ----
  tree_formula <- object$info$call$formula
  if (is.null(tree_formula)) tree_formula <- formula(object)
  resp_name <- as.character(tree_formula[[2]])

  # ---- Build model specification strings ----
  ## Models with (some) cross-occasion item constraints identify the follow-up
  ## mean and variance, which are then freely estimated (as in Phase 1). In the
  ## fully unconstrained model they are not identified and are fixed at 0 and 1.
  model_str_base <- paste0(
    'Theta_T1 = 1-', n_items, '\n',
    'Theta_T2 = ', n_items + 1, '-', 2 * n_items, '\n',
    'COV = Theta_T1*Theta_T2, Theta_T2*Theta_T2\nMEAN = Theta_T2')
  model_str_uncon <- paste0(
    'Theta_T1 = 1-', n_items, '\n',
    'Theta_T2 = ', n_items + 1, '-', 2 * n_items, '\n',
    'COV = Theta_T1*Theta_T2')

  ## --- helper: per-item constraint tuples from the node's category counts ---
  make_tuples <- function(n_cats_per_col) {
    a_tup <- character(n_items); thr_tup <- vector("list", n_items)
    for (i in seq_len(n_items)) {
      j <- i + n_items
      a_tup[i] <- sprintf("(%d, a1, %d, a2)", i, j)
      n_thresh_i <- min(n_cats_per_col[i], n_cats_per_col[j]) - 1
      tt <- character(0)
      if (n_thresh_i > 0)
        for (d in seq_len(n_thresh_i)) tt <- c(tt, sprintf("(%d, %d, d%d)", i, j, d))
      thr_tup[[i]] <- tt
    }
    list(a_tup = a_tup, thr_tup = thr_tup)
  }

  ## --- helper: fit a graded two-factor model given a CONSTRAIN string ---
  fit_model <- function(y, constrain) {
    m2s <- if (is.null(constrain) || !nzchar(constrain)) model_str_uncon
    else paste0(model_str_base, '\nCONSTRAIN = ', constrain)
    tryCatch(
      mirt::mirt(data = y, model = mirt::mirt.model(m2s), itemtype = 'graded',
                 SE = FALSE, verbose = FALSE, technical = list(NCYCLES = 1000), ...),
      error = function(e) { warning("model fit failed: ", e$message); NULL })
  }

  na_global <- function(nd, n = 0) data.frame(
    Node = nd, n = n, LL_constrained = NA, LL_unconstrained = NA,
    LRT_chi2 = NA, LRT_df = NA, LRT_p = NA, RS_detected = NA,
    pseudo_R2 = NA, AIC_diff = NA, mu_T2 = NA, sigma2_T2 = NA, cor_T1_T2 = NA,
    converged_constrained = NA, converged_unconstrained = NA,
    stringsAsFactors = FALSE)

  ## ======================= PASS 1: omnibus per node =======================
  global_results <- list(); param_list <- list(); node_cache <- list()
  for (nd in node) {
    if (verbose) message("Processing node ", nd, " (omnibus)...")
    node_data <- tryCatch(partykit::data_party(object, id = nd),
                          error = function(e) NULL)
    if (is.null(node_data) || nrow(node_data) == 0) {
      global_results[[as.character(nd)]] <- na_global(nd); next }
    y <- node_data[[resp_name]]; if (!is.matrix(y)) y <- as.matrix(y)
    y <- .align_categories(y, n_items)
    n_node <- nrow(y)
    n_cats_per_col <- apply(y, 2, function(col) length(unique(na.omit(col))))
    tup <- make_tuples(n_cats_per_col)
    full_constrain <- paste(c(tup$a_tup, unlist(tup$thr_tup)), collapse = ", ")
    n_constraints  <- length(tup$a_tup) + length(unlist(tup$thr_tup))

    fit_con   <- fit_model(y, full_constrain)
    fit_uncon <- fit_model(y, NULL)
    if (is.null(fit_con) || is.null(fit_uncon)) {
      global_results[[as.character(nd)]] <- na_global(nd, n_node); next }

    ll_con <- as.numeric(logLik(fit_con)); ll_uncon <- as.numeric(logLik(fit_uncon))
    lrt_chi2 <- -2 * (ll_con - ll_uncon)
    ## the constrained model has the follow-up mean and variance as two extra
    ## free parameters, so the omnibus test has n_constraints - 2 df
    lrt_df <- mirt::extract.mirt(fit_uncon, 'nest') - mirt::extract.mirt(fit_con, 'nest')
    lrt_p <- pchisq(lrt_chi2, df = lrt_df, lower.tail = FALSE)
    pseudo_r2 <- 1 - (ll_uncon / ll_con)
    npar_con   <- mirt::extract.mirt(fit_con, 'nest')
    npar_uncon <- mirt::extract.mirt(fit_uncon, 'nest')
    aic_diff <- (-2 * ll_con + 2 * npar_con) - (-2 * ll_uncon + 2 * npar_uncon)
    cc <- mirt::coef(fit_con, simplify = TRUE)
    mu_T2 <- cc$means[2]; cov_mat <- cc$cov
    sigma2_T2 <- cov_mat[2, 2]
    cor_T1_T2 <- cov_mat[1, 2] / sqrt(cov_mat[1, 1] * cov_mat[2, 2])

    global_results[[as.character(nd)]] <- data.frame(
      Node = nd, n = n_node,
      LL_constrained = round(ll_con, 2), LL_unconstrained = round(ll_uncon, 2),
      LRT_chi2 = round(lrt_chi2, 3), LRT_df = lrt_df, LRT_p = lrt_p,
      RS_detected = lrt_p < alpha, pseudo_R2 = round(pseudo_r2, 4),
      AIC_diff = round(aic_diff, 2), mu_T2 = round(mu_T2, 3),
      sigma2_T2 = round(sigma2_T2, 3), cor_T1_T2 = round(cor_T1_T2, 3),
      converged_constrained = fit_con@OptimInfo$converged,
      converged_unconstrained = fit_uncon@OptimInfo$converged,
      stringsAsFactors = FALSE)
    param_list[[as.character(nd)]] <- list(
      constrained   = mirt::coef(fit_con,   IRTpars = TRUE, simplify = TRUE)$items,
      unconstrained = mirt::coef(fit_uncon, IRTpars = TRUE, simplify = TRUE)$items)
    node_cache[[as.character(nd)]] <- list(y = y, ll_con = ll_con, tup = tup)

    if (verbose) message("  n = ", n_node)
  }
  global_df <- do.call(rbind, global_results)

  ## ---- adjust omnibus across nodes -> RS_detected (this gates item tests) ----
  if (global_p_adjust != "none" && nrow(global_df) > 1) {
    non_na <- !is.na(global_df$LRT_p)
    global_df$LRT_p_adj <- NA_real_
    global_df$LRT_p_adj[non_na] <- p.adjust(global_df$LRT_p[non_na], method = global_p_adjust)
    global_df$RS_detected <- global_df$LRT_p_adj < alpha
    global_df$RS_detected[is.na(global_df$LRT_p_adj)] <- NA
    if (verbose) message("\nOmnibus p-values adjusted across ", sum(non_na),
                         " nodes using '", global_p_adjust, "' method.")
  }

  ## ---- verbose: full per-node omnibus summary (printed AFTER adjustment so
  ##      RS_detected is the final, family-wise decision that gates item tests) ----
  if (verbose) {
    has_adj <- "LRT_p_adj" %in% names(global_df)
    for (i in seq_len(nrow(global_df))) {
      g <- global_df[i, ]
      message("\nNode ", g$Node, " (n = ", g$n, "):")
      message("  Constrained LL = ", g$LL_constrained,
              ", Unconstrained LL = ", g$LL_unconstrained)
      if (has_adj && !is.na(g$LRT_p_adj))
        message("  LRT chi2 = ", g$LRT_chi2, ", df = ", g$LRT_df,
                ", p = ", formatC(g$LRT_p, format = "e", digits = 3),
                ", p_adj = ", formatC(g$LRT_p_adj, format = "e", digits = 3))
      else
        message("  LRT chi2 = ", g$LRT_chi2, ", df = ", g$LRT_df,
                ", p = ", formatC(g$LRT_p, format = "e", digits = 3))
      message("  RS detected: ", g$RS_detected,
              "  (pseudo R2 = ", g$pseudo_R2, ", AIC diff = ", round(g$AIC_diff, 1), ")")
      message("  mu_T2 = ", g$mu_T2, ", cor(T1,T2) = ", g$cor_T1_T2)
    }
  }

  ## ================ PASS 2: item-level in RS-detected nodes ================
  item_results <- list()
  if (item_level) {
    det_nodes <- global_df$Node[!is.na(global_df$RS_detected) & global_df$RS_detected]
    for (nd in det_nodes) {
      ch <- node_cache[[as.character(nd)]]; if (is.null(ch)) next
      y <- ch$y; ll_con <- ch$ll_con
      a_all <- ch$tup$a_tup; thr_all <- ch$tup$thr_tup; M <- n_items
      if (verbose) message("Node ", nd, ": item-level RS testing (RS detected)...")

      ## Detection + type, one joint fit per item:
      ##   detection = joint LRT (free item m's discrimination AND thresholds)
      ##              vs the constrained model, adjusted across items;
      ##   type      = read from the ESTIMATED T1-vs-T2 changes on the IRT
      ##               (a, b) metric (discr_diff, thresh_diff), because the
      ##               slope-intercept (a, d) parameterization confounds an
      ##               LRT decomposition (d = -a*b, so a pure discrimination
      ##               change forces the intercepts, firing a threshold LRT).
      ## One pass of item tests given a set F of items whose constraints are
      ## already released (F = empty set gives the original one-at-a-time
      ## tests against the fully constrained model). For m not in F the
      ## reference model frees F and the test additionally frees m; for m in F
      ## the test compares the model freeing F with the model that restores
      ## m's constraints. Each test therefore uses only the non-flagged items
      ## as anchors (iterative purification).
      con_except <- function(free) paste(c(a_all[-free], unlist(thr_all[-free])),
                                         collapse = ", ")
      item_pass <- function(F) {
        fit_ref <- if (length(F)) fit_model(y, con_except(F)) else NULL
        ll_ref  <- if (length(F)) {
          if (is.null(fit_ref)) NA_real_ else as.numeric(logLik(fit_ref))
        } else ll_con
        rows <- lapply(seq_len(M), function(m) {
          df_j <- 1 + length(thr_all[[m]])
          if (m %in% F) {
            fit_small <- fit_model(y, con_except(setdiff(F, m)))
            fit_big   <- fit_ref
          } else {
            fit_small <- NULL
            fit_big   <- fit_model(y, con_except(c(F, m)))
          }
          ll_small <- if (m %in% F) {
            if (is.null(fit_small)) NA_real_ else as.numeric(logLik(fit_small))
          } else ll_ref
          if (is.null(fit_big) || is.na(ll_small))
            return(data.frame(Node = nd, Item = m, LRT_chi2 = NA, LRT_df = df_j,
                              LRT_p = NA, discr_diff = NA, thresh_diff = NA,
                              stringsAsFactors = FALSE))
          chi2 <- -2 * (ll_small - as.numeric(logLik(fit_big)))
          p    <- pchisq(chi2, df = df_j, lower.tail = FALSE)
          co <- mirt::coef(fit_big, IRTpars = TRUE, simplify = TRUE)$items
          bn <- grep("^b\\d+$", colnames(co), value = TRUE)
          data.frame(Node = nd, Item = m, LRT_chi2 = round(chi2, 3), LRT_df = df_j,
                     LRT_p = round(p, 6),
                     discr_diff  = round(abs(co[m, "a1"] - co[m + M, "a2"]), 4),
                     thresh_diff = round(max(abs(co[m, bn] - co[m + M, bn]), na.rm = TRUE), 4),
                     stringsAsFactors = FALSE)
        })
        idf <- do.call(rbind, rows)
        ## item DETECTION: adjust joint p across items within the node
        idf$LRT_p_adj <- if (p_adjust != "none")
          round(p.adjust(idf$LRT_p, method = p_adjust), 6) else round(idf$LRT_p, 6)
        idf
      }

      F_set <- integer(0); iter <- 0L
      repeat {
        iter <- iter + 1L
        idf <- item_pass(F_set)
        F_new <- idf$Item[!is.na(idf$LRT_p_adj) & idf$LRT_p_adj < alpha]
        if (!purify || setequal(F_new, F_set) || iter >= max_iter) break
        if (length(F_new) >= M - 1L) {
          warning("Node ", nd, ": fewer than two anchor items left; ",
                  "purification stopped.")
          break
        }
        F_set <- sort(F_new)
      }
      idf$iterations <- iter
      flagged <- !is.na(idf$LRT_p_adj) & idf$LRT_p_adj < alpha

      if (type_method == "wald") {
        ## item TYPE by two tests, each with the other set of parameters free
        ## (see .rs_type_tests); no magnitude cut-off is involved.
        tt <- .rs_type_tests(y, a_all, thr_all, M,
                             flagged_items = idf$Item[flagged],
                             model_str = model_str_base, ...)
        idf <- cbind(idf, tt[match(idf$Item, tt$Item), c("W_slope", "p_slope",
                     "W_location", "df_location", "p_location")])
        rownames(idf) <- NULL
        sl <- flagged & !is.na(idf$p_slope)    & idf$p_slope    < alpha
        lo <- flagged & !is.na(idf$p_location) & idf$p_location < alpha
        rs_type <- rep("None", nrow(idf))
        rs_type[flagged]        <- "Undetermined"
        rs_type[sl & lo]        <- "Both"
        rs_type[sl & !lo]       <- "Reprioritization"
        rs_type[!sl & lo]       <- "Recalibration"
        idf$RS_type <- rs_type
        idf$RS_magnitude <- NA_character_
      } else {
        ## legacy rule (grmtree <= 0.3.0): label from the size of the estimated
        ## changes relative to rs_threshold.
        repri <- flagged & !is.na(idf$discr_diff)  & idf$discr_diff  > rs_threshold
        recal <- flagged & !is.na(idf$thresh_diff) & idf$thresh_diff > rs_threshold
        rs_type <- rep("None", nrow(idf))
        rs_type[repri & recal]  <- "Both"
        rs_type[repri & !recal] <- "Reprioritization"
        rs_type[!repri & recal] <- "Recalibration"
        edge <- flagged & !repri & !recal
        dom_repri <- !is.na(idf$discr_diff) & !is.na(idf$thresh_diff) &
          idf$discr_diff >= idf$thresh_diff
        rs_type[edge &  dom_repri] <- "Reprioritization"
        rs_type[edge & !dom_repri] <- "Recalibration"
        idf$RS_type <- rs_type
        idf$RS_magnitude <- ifelse(flagged,
          ifelse(pmax(idf$discr_diff, idf$thresh_diff, na.rm = TRUE) >= rs_threshold,
                 "large", "small"), NA_character_)
      }
      item_results[[as.character(nd)]] <- idf
      if (verbose) {
        idf <- item_results[[as.character(nd)]]; fl <- idf[idf$RS_type != "None", ]
        if (nrow(fl)) message("  Items with RS (adjusted): ",
                              paste(fl$Item, " (", fl$RS_type, ")", sep = "", collapse = ", "))
        else message("  No items flagged after adjustment")
      }
    }
  }
  item_level_df <- if (length(item_results)) do.call(rbind, item_results) else NULL

  out <- list(
    global = global_df, item_level = item_level_df, parameters = param_list,
    n_items = n_items, alpha = alpha, p_adjust = p_adjust,
    global_p_adjust = global_p_adjust, rs_threshold = rs_threshold,
    purify = purify, type_method = type_method)
  class(out) <- "rs_characterization"
  out
}
#' Print Method for Response Shift Characterization Results
#'
#' Displays a formatted summary of response shift characterization results
#' from \code{\link{rs_characterize}}, including omnibus RS test results per
#' terminal node and item-level RS testing results with significance markers.
#'
#' @param x An \code{rs_characterization} object returned by
#'   \code{\link{rs_characterize}}.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns \code{x}. Called for its side effect of printing
#'   formatted output to the console.
#'
#' @details
#' The output is organized in two sections:
#'
#' \strong{Omnibus RS Test:} For each terminal node, displays the LRT
#' statistic, degrees of freedom, raw and adjusted p-values (if applicable),
#' RS detection status, effect sizes (pseudo R-squared and AIC difference),
#' latent mean shift, T1-T2 correlation, and convergence warnings.
#'
#' \strong{Item-Level RS Testing:} For nodes where omnibus RS was detected,
#' displays per-item LRT statistics, raw and adjusted p-values, RS type
#' classification, and significance markers (\code{***} for items significant
#' after adjustment). Also reports the p-value adjustment method and RS
#' classification threshold used.
#'
#' @seealso \code{\link{rs_characterize}} for computing the results
#' @export
print.rs_characterization <- function(x, ...) {

  cat("=== Response Shift Characterization ===\n\n")

  g <- x$global

  cat("--- Omnibus RS Test (Constrained vs Unconstrained) ---\n")
  cat("H0: Item parameters are equal across T1 and T2\n")
  cat("H1: Item parameters differ across T1 and T2\n")
  has_global_adj <- "LRT_p_adj" %in% names(g)
  if (has_global_adj) {
    cat(sprintf("Omnibus p-values adjusted across nodes using '%s' method\n",
                x$global_p_adjust))
  }
  cat("\n")

  # Print global results nicely
  for (i in 1:nrow(g)) {
    cat(sprintf("Node %d (n = %d):\n", g$Node[i], g$n[i]))
    if (has_global_adj && !is.na(g$LRT_p_adj[i])) {
      cat(sprintf("  LRT: chi2 = %.3f, df = %d, p = %s, p_adj = %s\n",
                  as.numeric(g$LRT_chi2[i]), g$LRT_df[i],
                  formatC(g$LRT_p[i], format = "e", digits = 3),
                  formatC(g$LRT_p_adj[i], format = "e", digits = 3)))
    } else {
      cat(sprintf("  LRT: chi2 = %.3f, df = %d, p = %s\n",
                  as.numeric(g$LRT_chi2[i]), g$LRT_df[i],
                  formatC(g$LRT_p[i], format = "e", digits = 3)))
    }
    cat(sprintf("  RS detected: %s\n", g$RS_detected[i]))
    if ("pseudo_R2" %in% names(g)) {
      cat(sprintf("  Effect size: pseudo R2 = %.4f, AIC diff = %.2f\n",
                  as.numeric(g$pseudo_R2[i]), as.numeric(g$AIC_diff[i])))
    }
    cat(sprintf("  Latent mean shift (mu_T2): %.3f\n", as.numeric(g$mu_T2[i])))
    cat(sprintf("  Correlation (T1, T2): %.3f\n", as.numeric(g$cor_T1_T2[i])))
    if (!is.na(g$converged_constrained[i]) && !g$converged_constrained[i]) {
      cat("  WARNING: Constrained model did not converge\n")
    }
    if (!is.na(g$converged_unconstrained[i]) && !g$converged_unconstrained[i]) {
      cat("  WARNING: Unconstrained model did not converge\n")
    }
    cat("\n")
  }

  # Print item-level results if present
  if (!is.null(x$item_level)) {
    cat("--- Item-Level RS Testing ---\n")
    if (isTRUE(x$purify)) {
      cat("(Items tested against purified anchors: flagged items freed iteratively)\n")
    } else {
      cat("(Items freed one at a time from the constrained model)\n")
    }
    has_adj <- "LRT_p_adj" %in% colnames(x$item_level)
    if (has_adj) {
      cat(sprintf("P-values adjusted using '%s' method\n", x$p_adjust))
    }
    if (identical(x$type_method, "threshold")) {
      cat(sprintf("RS type: legacy magnitude rule, threshold %.2f\n", x$rs_threshold))
    } else {
      cat("RS type: Wald tests of slope (reprioritization) and location\n")
      cat("(recalibration), each with the other parameters free\n")
    }
    cat("\n")

    for (nd in unique(x$item_level$Node)) {
      cat(sprintf("Node %d:\n", nd))
      node_items <- x$item_level[x$item_level$Node == nd, ]

      # Use adjusted p-values for significance marking if available
      p_for_sig <- if (has_adj) node_items$LRT_p_adj else node_items$LRT_p
      node_items$Sig <- ifelse(p_for_sig < x$alpha & !is.na(p_for_sig),
                               " ***", "")

      for (j in 1:nrow(node_items)) {
        mag <- if ("RS_magnitude" %in% names(node_items)) node_items$RS_magnitude[j] else NA
        mag_txt <- if (!is.na(mag)) paste0(" [", mag, "]") else ""
        if ("p_slope" %in% names(node_items) && !is.na(node_items$p_slope[j]))
          mag_txt <- paste0(mag_txt, sprintf(" (slope p = %.3f, location p = %.3f)",
                                             node_items$p_slope[j], node_items$p_location[j]))
        if (has_adj) {
          cat(sprintf("  Item %2d: chi2 = %7.3f, df = %d, p = %.6f, p_adj = %.6f  %-18s%s%s\n",
                      node_items$Item[j],
                      node_items$LRT_chi2[j],
                      node_items$LRT_df[j],
                      node_items$LRT_p[j],
                      node_items$LRT_p_adj[j],
                      node_items$RS_type[j],
                      mag_txt,
                      node_items$Sig[j]))
        } else {
          cat(sprintf("  Item %2d: chi2 = %7.3f, df = %d, p = %.6f  %-18s%s%s\n",
                      node_items$Item[j],
                      node_items$LRT_chi2[j],
                      node_items$LRT_df[j],
                      node_items$LRT_p[j],
                      node_items$RS_type[j],
                      mag_txt,
                      node_items$Sig[j]))
        }
      }
      cat("\n")
    }
  }

  invisible(x)
}


# Internal: type tests for the items flagged in one node.
#
# One model is fitted with standard errors: the flagged items are free across
# occasions, every other item is constrained (the anchors, as after
# purification), and the follow-up mean and variance are free. For each flagged
# item m, with a = slope and d = intercepts (d = -a b) at T1 (L) and T2 (R):
#   slope test    : Wald test of a_R = a_L                          (df = 1)
#   location test : Wald test of b_R = b_L for all K - 1 thresholds, written
#                   without a ratio as a_L d_R,k - a_R d_L,k = 0     (df = K - 1)
# Each test leaves the other set of parameters free, so a slope change cannot
# show up as a location change (it changes d, but not b) and vice versa.
# Both statistics use the delta-method covariance from mirt's vcov().
# @keywords internal
.rs_type_tests <- function(y, a_all, thr_all, M, flagged_items, model_str, ...) {
  empty <- data.frame(Item = integer(0), W_slope = numeric(0), p_slope = numeric(0),
                      W_location = numeric(0), df_location = integer(0),
                      p_location = numeric(0), delta_a = numeric(0),
                      delta_b_max = numeric(0))
  if (!length(flagged_items)) return(empty)
  na_out <- data.frame(Item = flagged_items, W_slope = NA_real_, p_slope = NA_real_,
                       W_location = NA_real_, df_location = NA_integer_,
                       p_location = NA_real_, delta_a = NA_real_,
                       delta_b_max = NA_real_)
  keep <- setdiff(seq_len(M), flagged_items)
  if (!length(keep)) {
    warning("Type tests: every item is flagged, so no anchors remain; ",
            "flagged items are labelled 'Undetermined'.")
    return(na_out)
  }
  con <- paste(c(a_all[keep], unlist(thr_all[keep])), collapse = ", ")
  m2s <- if (nzchar(con)) paste0(model_str, "\nCONSTRAIN = ", con) else model_str
  fit <- tryCatch(suppressWarnings(mirt::mirt(y, mirt::mirt.model(m2s),
           itemtype = "graded", SE = TRUE, verbose = FALSE,
           technical = list(NCYCLES = 1000), ...)), error = function(e) NULL)
  if (is.null(fit)) {
    warning("Type tests: model with standard errors could not be fitted; ",
            "flagged items are labelled 'Undetermined'.")
    return(na_out)
  }
  V <- tryCatch(mirt::vcov(fit), error = function(e) NULL)
  if (is.null(V) || any(!is.finite(diag(V)))) {
    warning("Type tests: standard errors unavailable; flagged items are ",
            "labelled 'Undetermined'.")
    return(na_out)
  }
  ## vcov rows are labelled "<par>.<parnum>[.<parnum>...]"; map parnum -> row
  idx <- list()
  for (r in seq_len(nrow(V))) {
    for (p in strsplit(rownames(V)[r], ".", fixed = TRUE)[[1]][-1]) idx[[p]] <- r
  }
  v <- mirt::mod2values(fit)
  cn <- colnames(y)
  rows <- lapply(flagged_items, function(m) {
    getp <- function(item, slope) {
      s <- v[v$item == item & v$est, ]
      dn <- grep("^d[0-9]+$", s$name, value = TRUE)
      dn <- dn[order(as.integer(sub("^d", "", dn)))]
      s[match(c(slope, dn), s$name), ]
    }
    L <- getp(cn[m], "a1"); R <- getp(cn[m + M], "a2")
    if (anyNA(L$parnum) || anyNA(R$parnum) || nrow(L) != nrow(R))
      return(na_out[na_out$Item == m, ])
    pr <- unlist(idx[as.character(c(L$parnum, R$parnum))])
    if (length(pr) != nrow(L) + nrow(R)) return(na_out[na_out$Item == m, ])
    S <- V[pr, pr]
    K1 <- nrow(L) - 1L
    aL <- L$value[1]; dL <- L$value[-1]; aR <- R$value[1]; dR <- R$value[-1]
    gs <- c(-1, rep(0, K1), 1, rep(0, K1))
    Ws <- (aR - aL)^2 / as.numeric(t(gs) %*% S %*% gs)
    h <- aL * dR - aR * dL
    H <- matrix(0, K1, 2L * (K1 + 1L))
    H[, 1] <- dR;           H[cbind(seq_len(K1), 1 + seq_len(K1))] <- -aR
    H[, K1 + 2] <- -dL;     H[cbind(seq_len(K1), K1 + 2 + seq_len(K1))] <- aL
    Wl <- tryCatch(as.numeric(t(h) %*% solve(H %*% S %*% t(H), h)),
                   error = function(e) NA_real_)
    data.frame(Item = m, W_slope = Ws,
               p_slope = stats::pchisq(Ws, 1, lower.tail = FALSE),
               W_location = Wl, df_location = K1,
               p_location = stats::pchisq(Wl, K1, lower.tail = FALSE),
               delta_a = aR - aL,
               delta_b_max = max(abs((-dR / aR) - (-dL / aL))))
  })
  do.call(rbind, rows)
}
