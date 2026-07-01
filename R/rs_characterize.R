################################################################################
##
## rs_characterize(): Post-hoc Response Shift Characterization
##
## After fitting longitudinal_grmtree(), this function tests for actual
## response shift WITHIN each terminal node by comparing the constrained
## model (item parameters equal across time) to an unconstrained model
## (item parameters free across time).
##
## The tree splits identify subgroups with DIFFERENT measurement models
## (analogous to DIF). This function then asks: within each subgroup,
## do item parameters CHANGE over time (i.e., response shift)?
##
## Author: Olayinka Arimoro
## Date: February 2026
##
################################################################################

#' Characterize Response Shift Within Terminal Nodes of a Longitudinal GRM Tree
#'
#' After fitting a \code{\link{longitudinal_grmtree}} (Phase 1), this function
#' performs Phase 2: post-hoc response shift (RS) characterization within each
#' terminal node. For each subgroup identified by the tree, it compares the
#' constrained model (no RS: item parameters equal across time) to an
#' unconstrained model (item parameters free across time) using likelihood
#' ratio tests (LRTs), then identifies which specific items exhibit response
#' shift and classifies the type of shift.
#'
#' @param object A \code{longitudinal_grmtree} object fitted by
#'   \code{\link{longitudinal_grmtree}}.
#' @param node Optional integer vector of terminal node IDs to analyze. If
#'   \code{NULL} (default), all terminal nodes are analyzed.
#' @param item_level Logical. If \code{TRUE} (default), perform item-level RS
#'   testing by sequentially freeing one item at a time from the constrained
#'   model. Only performed for nodes where the omnibus test is significant.
#' @param alpha Numeric significance level for RS tests. Default is 0.05.
#'   Used for both omnibus and item-level significance decisions (after any
#'   p-value adjustments).
#' @param p_adjust Character string specifying the method for adjusting
#'   item-level p-values \strong{within each node}. Controls the error rate
#'   when testing multiple items for RS within a single subgroup. Options:
#'   \code{"bonferroni"} (default), \code{"holm"}, \code{"BH"}
#'   (Benjamini-Hochberg), \code{"BY"}, \code{"fdr"}, \code{"hochberg"},
#'   \code{"hommel"}, or \code{"none"}. See \code{\link[stats]{p.adjust}}.
#' @param global_p_adjust Character string specifying the method for adjusting
#'   omnibus RS p-values \strong{across terminal nodes}. Controls the
#'   family-wise error rate when testing RS in multiple subgroups. Options
#'   are the same as \code{p_adjust}. Default is \code{"bonferroni"}.
#'   The \code{RS_detected} column in the global results is updated based
#'   on adjusted p-values. Only applied when there are 2 or more nodes.
#' @param rs_threshold Numeric threshold for classifying the magnitude of
#'   parameter change in RS type determination, on the IRT parameter scale.
#'   Default is 0.3. A discrimination difference exceeding this threshold
#'   indicates reprioritization; a maximum threshold difference exceeding
#'   this indicates recalibration. Larger values produce more conservative
#'   classifications (fewer items classified as showing meaningful RS).
#'   Items that are statistically significant but below this threshold are
#'   classified as \code{"Significant (small effect)"}.
#' @param verbose Logical. If \code{TRUE} (default), print progress messages
#'   including log-likelihoods, LRT statistics, and RS classifications for
#'   each node during processing.
#' @param ... Additional arguments passed to \code{\link[mirt]{mirt}} during
#'   model fitting.
#'
#' @return A list of class \code{"rs_characterization"} containing:
#'   \describe{
#'     \item{\code{global}}{A \code{data.frame} with one row per terminal node
#'       and columns:
#'       \describe{
#'         \item{\code{Node}}{Terminal node ID}
#'         \item{\code{n}}{Sample size in the node}
#'         \item{\code{LL_constrained}}{Log-likelihood of the constrained
#'           (no-RS) model}
#'         \item{\code{LL_unconstrained}}{Log-likelihood of the unconstrained
#'           (RS-allowed) model}
#'         \item{\code{LRT_chi2}}{Likelihood ratio test statistic:
#'           \eqn{-2(LL_{con} - LL_{uncon})}}
#'         \item{\code{LRT_df}}{Degrees of freedom (number of freed
#'           constraints)}
#'         \item{\code{LRT_p}}{Raw p-value from chi-squared distribution}
#'         \item{\code{LRT_p_adj}}{Adjusted p-value (present only if
#'           \code{global_p_adjust != "none"} and there are 2+ nodes)}
#'         \item{\code{RS_detected}}{Logical: whether RS is detected at
#'           the specified \code{alpha} level (uses adjusted p if available)}
#'         \item{\code{pseudo_R2}}{McFadden's pseudo R-squared:
#'           \eqn{1 - LL_{uncon}/LL_{con}}}
#'         \item{\code{AIC_diff}}{AIC difference:
#'           \eqn{AIC_{con} - AIC_{uncon}}. Positive values indicate the
#'           unconstrained model fits better after parsimony penalty.}
#'         \item{\code{mu_T2}}{Latent mean shift at T2 from the constrained
#'           model. Positive = improvement, negative = decline.}
#'         \item{\code{sigma2_T2}}{Latent variance at T2}
#'         \item{\code{cor_T1_T2}}{Correlation between latent traits at T1
#'           and T2, reflecting stability of individual differences}
#'         \item{\code{converged_constrained}}{Logical: whether the
#'           constrained model converged}
#'         \item{\code{converged_unconstrained}}{Logical: whether the
#'           unconstrained model converged}
#'       }
#'     }
#'     \item{\code{item_level}}{A \code{data.frame} with item-level RS test
#'       results (one row per item per node where omnibus RS was detected),
#'       with columns:
#'       \describe{
#'         \item{\code{Node}}{Terminal node ID}
#'         \item{\code{Item}}{Item number (1 to \code{n_items})}
#'         \item{\code{LRT_chi2}}{LRT statistic for freeing this item}
#'         \item{\code{LRT_df}}{Degrees of freedom (1 discrimination +
#'           thresholds for this item)}
#'         \item{\code{LRT_p}}{Raw p-value}
#'         \item{\code{LRT_p_adj}}{Adjusted p-value (if \code{p_adjust !=
#'           "none"})}
#'         \item{\code{RS_type}}{Classification: \code{"Recalibration"},
#'           \code{"Reprioritization"}, \code{"Both"},
#'           \code{"Significant (small effect)"}, or \code{"None"}}
#'       }
#'       \code{NULL} if no nodes showed significant omnibus RS or
#'       \code{item_level = FALSE}.
#'     }
#'     \item{\code{parameters}}{A named list (one element per node) of lists
#'       containing \code{constrained} and \code{unconstrained} item parameter
#'       matrices (IRT parameterization with discrimination \code{a1}/\code{a2}
#'       and thresholds \code{b1}, ..., \code{bK}).}
#'     \item{\code{n_items}}{Number of items per time point.}
#'     \item{\code{alpha}}{Significance level used.}
#'     \item{\code{p_adjust}}{Item-level p-value adjustment method.}
#'     \item{\code{global_p_adjust}}{Omnibus p-value adjustment method.}
#'     \item{\code{rs_threshold}}{RS classification threshold.}
#'   }
#'
#' @details
#' ## The Two-Phase Framework
#'
#' The Longitudinal GRMTree operates as an integrated two-phase method:
#'
#' \strong{Phase 1 (\code{\link{longitudinal_grmtree}}):} Embeds a constrained
#' longitudinal GRM (item parameters equal across T1 and T2) within
#' model-based recursive partitioning. The tree identifies patient subgroups
#' requiring different constrained measurement models. This ensures that
#' subsequent RS testing is conducted within measurement-homogeneous groups.
#'
#' \strong{Phase 2 (this function):} Within each terminal node, relaxes the
#' equality constraint and tests whether item parameters actually change over
#' time. This separates between-group measurement heterogeneity (captured by
#' the tree) from within-group temporal change (response shift).
#'
#' ## Omnibus RS Test
#'
#' For each terminal node, the function fits two models to the node's data:
#' \enumerate{
#'   \item \strong{Constrained model:} Item parameters are equal across T1
#'     and T2 (no response shift). This model has the same structure as the
#'     model fitted during Phase 1.
#'   \item \strong{Unconstrained model:} Item parameters are free to differ
#'     between T1 and T2 (response shift allowed). All equality constraints
#'     on discrimination and threshold parameters are removed.
#' }
#'
#' The likelihood ratio test statistic is:
#' \deqn{\chi^2 = -2(LL_{constrained} - LL_{unconstrained})}
#'
#' with degrees of freedom equal to the number of freed constraints
#' (typically \eqn{M \times K} for \eqn{M} items with \eqn{K-1} thresholds
#' each, plus \eqn{M} discrimination parameters).
#'
#' ## Item-Level RS Testing
#'
#' When the omnibus test is significant, each item is tested individually by
#' freeing only that item's constraints while keeping all other items
#' constrained. The LRT compares the fully constrained model to the partially
#' constrained model with one item freed. This identifies which specific
#' items contribute to the overall response shift.
#'
#' ## RS Type Classification
#'
#' For items showing statistically significant RS (after p-value adjustment),
#' the type is classified by examining the freed item's T1 vs T2 parameters
#' from the partially constrained model:
#'
#' \describe{
#'   \item{\strong{Recalibration}}{Maximum absolute threshold difference
#'     exceeds \code{rs_threshold}. Interpretation: the patient's internal
#'     standards for endorsing response categories changed over time
#'     (Sprangers & Schwartz, 1999).}
#'   \item{\strong{Reprioritization}}{Absolute discrimination difference
#'     exceeds \code{rs_threshold}. Interpretation: the item's relationship
#'     to the underlying latent construct changed over time --- the item
#'     became more or less central to the construct.}
#'   \item{\strong{Both}}{Both discrimination and threshold changes exceed
#'     \code{rs_threshold}.}
#'   \item{\strong{Significant (small effect)}}{The LRT is significant but
#'     neither parameter change exceeds \code{rs_threshold}. The RS is
#'     statistically detectable but small in magnitude.}
#' }
#'
#' ## Hierarchical P-Value Correction
#'
#' The function implements a two-level correction strategy:
#' \enumerate{
#'   \item \strong{Across nodes (omnibus):} \code{global_p_adjust} controls
#'     the family-wise error rate for the question "in which subgroups does
#'     RS occur?" Default is Bonferroni.
#'   \item \strong{Within nodes (item-level):} \code{p_adjust} controls the
#'     error rate for the question "which items exhibit RS within a given
#'     subgroup?" Default is Bonferroni; FDR (Benjamini-Hochberg) is
#'     recommended for exploratory analyses.
#' }
#'
#' ## Effect Size Measures
#'
#' \describe{
#'   \item{McFadden's pseudo R-squared}{\eqn{1 - LL_{uncon}/LL_{con}}.
#'     Inherently small (~0.02) in IRT models because item parameters
#'     account for only a fraction of the total likelihood. Values should
#'     be compared across nodes rather than interpreted in absolute terms.}
#'   \item{AIC difference}{\eqn{AIC_{con} - AIC_{uncon}}. Positive values
#'     indicate the unconstrained model fits better even after penalizing
#'     for additional parameters. Negative values suggest the constrained
#'     model is preferred (response shift is not substantial enough to
#'     justify the additional parameters).}
#' }
#'
#' @references
#' Sprangers, M. A., & Schwartz, C. E. (1999). Integrating response shift
#' into health-related quality of life research. \emph{Social Science &
#' Medicine}, 48(11), 1507--1515.
#'
#' Benjamini, Y., & Hochberg, Y. (1995). Controlling the false discovery
#' rate: a practical and powerful approach to multiple testing. \emph{Journal
#' of the Royal Statistical Society: Series B}, 57(1), 289--300.
#'
#' @author Olayinka Imisioluwa Arimoro \email{olayinka.arimoro@ucalgary.ca},
#'   Lisa M. Lix, Tolulope T. Sajobi
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
#'   # After fitting the tree (Phase 1)
#'   ltree <- longitudinal_grmtree(
#'     resp_wide ~ sex + age + residency + job +
#'       education + comorbidity_count + ever_smoker,
#'     data = ld, n_items = 8,
#'     control = grmtree.control(minbucket = 200)
#'   )
#'
#'   # Phase 2: characterize response shift within each subgroup
#'   rs <- rs_characterize(ltree, p_adjust = "fdr",
#'     global_p_adjust = "bonferroni")
#'   print(rs)
#'
#'   # Phase 2: Characterize RS
#'   # Default: Bonferroni at both levels, threshold = 0.3
#'   rs <- rs_characterize(ltree)
#'
#'   # Recommended: FDR for item-level, Bonferroni for omnibus
#'   rs <- rs_characterize(ltree,
#'     p_adjust = "fdr",
#'     global_p_adjust = "bonferroni",
#'     rs_threshold = 0.3
#'   )
#'
#'   # No omnibus correction (each node tested independently)
#'   rs <- rs_characterize(ltree, global_p_adjust = "none")
#'
#'   # More conservative RS classification
#'   rs <- rs_characterize(ltree, rs_threshold = 0.5)
#'
#'   # View results
#'   print(rs)
#'
#'   # Access components
#'   rs$global         # Omnibus results per node
#'   rs$item_level     # Item-level results
#'   rs$parameters     # Raw parameter matrices
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
                            verbose = TRUE, ...) {

  # ---- Input validation ----
  if (!inherits(object, "longitudinal_grmtree")) {
    stop("'object' must be a longitudinal_grmtree object")
  }
  valid_methods <- c("none", "bonferroni", "holm", "BH", "BY", "fdr",
                     "hochberg", "hommel")
  if (!p_adjust %in% valid_methods) {
    stop("'p_adjust' must be a valid p.adjust method or 'none'")
  }
  if (!global_p_adjust %in% valid_methods) {
    stop("'global_p_adjust' must be a valid p.adjust method or 'none'")
  }

  n_items <- object$info$n_items
  if (is.null(n_items)) {
    stop("Tree object does not contain n_items. Was it fit with longitudinal_grmtree()?")
  }

  # Get terminal node IDs
  if (is.null(node)) {
    node <- partykit::nodeids(object, terminal = TRUE)
  }

  # ---- Identify the response variable name from the formula ----
  tree_formula <- object$info$call$formula
  if (is.null(tree_formula)) {
    tree_formula <- formula(object)
  }
  resp_name <- as.character(tree_formula[[2]])  # LHS of formula

  # ---- Build model specification strings ----
  model_str_base <- paste0(
    'Theta_T1 = 1-', n_items, '\n',
    'Theta_T2 = ', n_items + 1, '-', 2 * n_items, '\n',
    'COV = Theta_T1*Theta_T2\n',
    'MEAN = Theta_T2'
  )

  # ---- Process each terminal node ----
  global_results <- list()
  item_results <- list()
  param_list <- list()

  for (nd in node) {

    if (verbose) message("Processing node ", nd, "...")

    # ---- Extract data using data_party() ----
    node_data <- tryCatch(
      partykit::data_party(object, id = nd),
      error = function(e) {
        warning("Failed to extract data for node ", nd, ": ", e$message)
        NULL
      }
    )

    if (is.null(node_data) || nrow(node_data) == 0) {
      global_results[[as.character(nd)]] <- data.frame(
        Node = nd, n = 0,
        LL_constrained = NA, LL_unconstrained = NA,
        LRT_chi2 = NA, LRT_df = NA, LRT_p = NA,
        RS_detected = NA, pseudo_R2 = NA, AIC_diff = NA,
        mu_T2 = NA, sigma2_T2 = NA, cor_T1_T2 = NA,
        converged_constrained = NA, converged_unconstrained = NA,
        stringsAsFactors = FALSE
      )
      next
    }

    # Extract the response matrix by name (e.g., "resp_wide")
    y_node <- node_data[[resp_name]]
    if (!is.matrix(y_node)) y_node <- as.matrix(y_node)
    n_node <- nrow(y_node)

    if (verbose) message("  n = ", n_node)

    # ---- Detect thresholds for this subgroup ----
    n_cats_per_col <- apply(y_node, 2, function(col) length(unique(na.omit(col))))

    # ---- Fit CONSTRAINED model (no RS) ----
    constraint_parts <- character(0)
    for (i in 1:n_items) {
      j <- i + n_items
      constraint_parts <- c(constraint_parts, paste0("(", i, ", a1, ", j, ", a2)"))
      n_thresh_i <- min(n_cats_per_col[i], n_cats_per_col[j]) - 1
      if (n_thresh_i > 0) {
        for (d in 1:n_thresh_i) {
          constraint_parts <- c(constraint_parts, paste0("(", i, ", ", j, ", d", d, ")"))
        }
      }
    }
    all_constraints <- paste(constraint_parts, collapse = ", ")
    model_constrained_str <- paste0(model_str_base, '\nCONSTRAIN = ', all_constraints)

    fit_constrained <- tryCatch({
      mirt::mirt(
        data = y_node,
        model = mirt::mirt.model(model_constrained_str),
        itemtype = 'graded',
        SE = FALSE,
        verbose = FALSE,
        technical = list(NCYCLES = 1000)
      )
    }, error = function(e) {
      warning("Constrained model failed in node ", nd, ": ", e$message)
      NULL
    })

    # ---- Fit UNCONSTRAINED model (RS allowed) ----
    fit_unconstrained <- tryCatch({
      mirt::mirt(
        data = y_node,
        model = mirt::mirt.model(model_str_base),
        itemtype = 'graded',
        SE = FALSE,
        verbose = FALSE,
        technical = list(NCYCLES = 1000)
      )
    }, error = function(e) {
      warning("Unconstrained model failed in node ", nd, ": ", e$message)
      NULL
    })

    if (is.null(fit_constrained) || is.null(fit_unconstrained)) {
      global_results[[as.character(nd)]] <- data.frame(
        Node = nd, n = n_node,
        LL_constrained = NA, LL_unconstrained = NA,
        LRT_chi2 = NA, LRT_df = NA, LRT_p = NA,
        RS_detected = NA, pseudo_R2 = NA, AIC_diff = NA,
        mu_T2 = NA, sigma2_T2 = NA, cor_T1_T2 = NA,
        converged_constrained = NA, converged_unconstrained = NA,
        stringsAsFactors = FALSE
      )
      next
    }

    # ---- Omnibus LRT: Constrained vs Unconstrained ----
    ll_con <- as.numeric(logLik(fit_constrained))
    ll_uncon <- as.numeric(logLik(fit_unconstrained))
    n_constraints <- length(constraint_parts)
    lrt_chi2 <- -2 * (ll_con - ll_uncon)
    lrt_df <- n_constraints
    lrt_p <- pchisq(lrt_chi2, df = lrt_df, lower.tail = FALSE)

    # Effect sizes for omnibus RS test
    pseudo_r2 <- 1 - (ll_uncon / ll_con)  # McFadden's pseudo R²
    # AIC = -2*LL + 2*k; compute from extract.mirt to get npar safely
    npar_con <- mirt::extract.mirt(fit_constrained, 'nest')
    npar_uncon <- mirt::extract.mirt(fit_unconstrained, 'nest')
    aic_con <- -2 * ll_con + 2 * npar_con
    aic_uncon <- -2 * ll_uncon + 2 * npar_uncon
    aic_diff <- aic_con - aic_uncon  # Positive = unconstrained fits better

    # Extract latent trait parameters from constrained model
    coefs_con <- mirt::coef(fit_constrained, simplify = TRUE)
    mu_T2 <- coefs_con$means[2]
    cov_mat <- coefs_con$cov
    sigma2_T2 <- cov_mat[2, 2]
    cor_T1_T2 <- cov_mat[1, 2] / sqrt(cov_mat[1, 1] * cov_mat[2, 2])

    # Convergence check
    conv_con <- fit_constrained@OptimInfo$converged
    conv_uncon <- fit_unconstrained@OptimInfo$converged

    # Check for extreme parameters (potential estimation problems)
    coefs_uncon_items <- mirt::coef(fit_unconstrained, IRTpars = TRUE, simplify = TRUE)$items
    extreme_discr <- any(abs(coefs_uncon_items[, grep("^a", colnames(coefs_uncon_items))]) > 10,
                         na.rm = TRUE)
    if (extreme_discr && verbose) {
      warning("Node ", nd, ": Extreme discrimination parameters detected (>10). ",
              "Check for sparse categories or separation.")
    }

    global_results[[as.character(nd)]] <- data.frame(
      Node = nd,
      n = n_node,
      LL_constrained = round(ll_con, 2),
      LL_unconstrained = round(ll_uncon, 2),
      LRT_chi2 = round(lrt_chi2, 3),
      LRT_df = lrt_df,
      LRT_p = lrt_p,
      RS_detected = lrt_p < alpha,
      pseudo_R2 = round(pseudo_r2, 4),
      AIC_diff = round(aic_diff, 2),
      mu_T2 = round(mu_T2, 3),
      sigma2_T2 = round(sigma2_T2, 3),
      cor_T1_T2 = round(cor_T1_T2, 3),
      converged_constrained = conv_con,
      converged_unconstrained = conv_uncon,
      stringsAsFactors = FALSE
    )

    if (verbose) {
      message("  Constrained LL = ", round(ll_con, 2),
              ", Unconstrained LL = ", round(ll_uncon, 2))
      message("  LRT chi2 = ", round(lrt_chi2, 3),
              ", df = ", lrt_df,
              ", p = ", formatC(lrt_p, format = "e", digits = 3))
      message("  RS detected: ", lrt_p < alpha,
              "  (pseudo R2 = ", round(pseudo_r2, 4),
              ", AIC diff = ", round(aic_diff, 1), ")")
      message("  mu_T2 = ", round(mu_T2, 3),
              ", cor(T1,T2) = ", round(cor_T1_T2, 3))
    }

    # ---- Store parameters for comparison ----
    coefs_con_items <- mirt::coef(fit_constrained, IRTpars = TRUE, simplify = TRUE)$items

    param_list[[as.character(nd)]] <- list(
      constrained = coefs_con_items,
      unconstrained = coefs_uncon_items
    )

    # ---- Item-level RS testing ----
    if (item_level && lrt_p < alpha) {

      if (verbose) message("  Performing item-level RS testing...")

      item_rs <- lapply(1:n_items, function(m) {

        # Free item m: remove its constraints from the list
        j <- m + n_items

        # Build constraints excluding item m
        partial_parts <- character(0)
        for (i in 1:n_items) {
          if (i == m) next  # Skip this item
          jj <- i + n_items
          partial_parts <- c(partial_parts, paste0("(", i, ", a1, ", jj, ", a2)"))
          n_thresh_i <- min(n_cats_per_col[i], n_cats_per_col[jj]) - 1
          if (n_thresh_i > 0) {
            for (d in 1:n_thresh_i) {
              partial_parts <- c(partial_parts, paste0("(", i, ", ", jj, ", d", d, ")"))
            }
          }
        }

        if (length(partial_parts) == 0) return(NULL)

        partial_constraints <- paste(partial_parts, collapse = ", ")
        model_partial_str <- paste0(model_str_base, '\nCONSTRAIN = ', partial_constraints)

        fit_partial <- tryCatch({
          mirt::mirt(
            data = y_node,
            model = mirt::mirt.model(model_partial_str),
            itemtype = 'graded',
            SE = FALSE,
            verbose = FALSE,
            technical = list(NCYCLES = 1000)
          )
        }, error = function(e) {
          warning("Partial model (free item ", m, ") failed in node ", nd)
          NULL
        })

        if (is.null(fit_partial)) {
          return(data.frame(
            Node = nd, Item = m,
            LRT_chi2 = NA, LRT_df = NA, LRT_p = NA,
            RS_type = NA,
            stringsAsFactors = FALSE
          ))
        }

        # LRT: constrained (all equal) vs partial (item m free)
        ll_partial <- as.numeric(logLik(fit_partial))
        n_freed <- 1 + min(n_cats_per_col[m], n_cats_per_col[j]) - 1
        # freed: 1 discrimination + thresholds for item m
        item_chi2 <- -2 * (ll_con - ll_partial)
        item_p <- pchisq(item_chi2, df = n_freed, lower.tail = FALSE)

        # Determine RS type by comparing T1 vs T2 parameters
        rs_type <- "None"
        if (item_p < alpha) {
          coefs_partial <- mirt::coef(fit_partial, IRTpars = TRUE, simplify = TRUE)$items

          # Compare discrimination for item m
          a_t1 <- coefs_partial[m, "a1"]       # T1 item discrimination
          a_t2 <- coefs_partial[j, "a2"]       # T2 item discrimination
          discr_diff <- abs(a_t1 - a_t2)

          # Compare thresholds for item m
          d_names_t1 <- grep("^b\\d+$", colnames(coefs_partial), value = TRUE)
          thresh_t1 <- coefs_partial[m, d_names_t1]
          thresh_t2 <- coefs_partial[j, d_names_t1]
          thresh_diff <- max(abs(thresh_t1 - thresh_t2), na.rm = TRUE)

          # Classify RS type based on parameter differences
          has_reprioritization <- discr_diff > rs_threshold
          has_recalibration <- thresh_diff > rs_threshold

          if (has_reprioritization && has_recalibration) {
            rs_type <- "Both"
          } else if (has_reprioritization) {
            rs_type <- "Reprioritization"
          } else if (has_recalibration) {
            rs_type <- "Recalibration"
          } else {
            rs_type <- "Significant (small effect)"
          }
        }

        data.frame(
          Node = nd,
          Item = m,
          LRT_chi2 = round(item_chi2, 3),
          LRT_df = n_freed,
          LRT_p = round(item_p, 6),
          RS_type = rs_type,
          stringsAsFactors = FALSE
        )
      })

      item_results[[as.character(nd)]] <- do.call(rbind, item_rs)

      if (verbose) {
        item_df <- item_results[[as.character(nd)]]
        sig_items <- item_df[item_df$LRT_p < alpha & !is.na(item_df$LRT_p), ]
        if (nrow(sig_items) > 0) {
          message("  Items with RS (unadjusted): ",
                  paste(sig_items$Item, " (", sig_items$RS_type, ")",
                        sep = "", collapse = ", "))
        } else {
          message("  No individual items reached significance")
        }
      }
    }
  }

  # ---- Compile results ----
  global_df <- do.call(rbind, global_results)
  item_level_df <- if (length(item_results) > 0) do.call(rbind, item_results) else NULL

  # Apply p-value adjustment to omnibus RS tests across nodes
  if (global_p_adjust != "none" && nrow(global_df) > 1) {
    non_na <- !is.na(global_df$LRT_p)
    global_df$LRT_p_adj <- NA_real_
    global_df$LRT_p_adj[non_na] <- p.adjust(global_df$LRT_p[non_na],
                                            method = global_p_adjust)
    # Update RS_detected based on adjusted p-values
    global_df$RS_detected <- global_df$LRT_p_adj < alpha
    global_df$RS_detected[is.na(global_df$LRT_p_adj)] <- NA
    if (verbose) {
      message("\nOmnibus p-values adjusted across ", sum(non_na),
              " nodes using '", global_p_adjust, "' method.")
    }
  }

  # Apply p-value adjustment to item-level tests within each node
  if (!is.null(item_level_df) && p_adjust != "none") {
    for (nd_id in unique(item_level_df$Node)) {
      nd_rows <- which(item_level_df$Node == nd_id)
      raw_p <- item_level_df$LRT_p[nd_rows]
      item_level_df$LRT_p_adj[nd_rows] <- p.adjust(raw_p, method = p_adjust)
    }
    if (verbose) {
      message("\nItem-level p-values adjusted using '", p_adjust, "' method.")
    }
  }

  out <- list(
    global = global_df,
    item_level = item_level_df,
    parameters = param_list,
    n_items = n_items,
    alpha = alpha,
    p_adjust = p_adjust,
    global_p_adjust = global_p_adjust,
    rs_threshold = rs_threshold
  )
  class(out) <- "rs_characterization"
  return(out)
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
    cat("(Items freed one at a time from the constrained model)\n")
    has_adj <- "LRT_p_adj" %in% colnames(x$item_level)
    if (has_adj) {
      cat(sprintf("P-values adjusted using '%s' method\n", x$p_adjust))
    }
    cat(sprintf("RS classification threshold: %.2f\n", x$rs_threshold))
    cat("\n")

    for (nd in unique(x$item_level$Node)) {
      cat(sprintf("Node %d:\n", nd))
      node_items <- x$item_level[x$item_level$Node == nd, ]

      # Use adjusted p-values for significance marking if available
      p_for_sig <- if (has_adj) node_items$LRT_p_adj else node_items$LRT_p
      node_items$Sig <- ifelse(p_for_sig < x$alpha & !is.na(p_for_sig),
                               " ***", "")

      for (j in 1:nrow(node_items)) {
        if (has_adj) {
          cat(sprintf("  Item %2d: chi2 = %7.3f, df = %d, p = %.6f, p_adj = %.6f  %-25s%s\n",
                      node_items$Item[j],
                      node_items$LRT_chi2[j],
                      node_items$LRT_df[j],
                      node_items$LRT_p[j],
                      node_items$LRT_p_adj[j],
                      node_items$RS_type[j],
                      node_items$Sig[j]))
        } else {
          cat(sprintf("  Item %2d: chi2 = %7.3f, df = %d, p = %.6f  %-25s%s\n",
                      node_items$Item[j],
                      node_items$LRT_chi2[j],
                      node_items$LRT_df[j],
                      node_items$LRT_p[j],
                      node_items$RS_type[j],
                      node_items$Sig[j]))
        }
      }
      cat("\n")
    }
  }

  invisible(x)
}
