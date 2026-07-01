################################################################################
##
## LONGITUDINAL GRMTree: Functions for Response Shift Detection (Production)
##
## This file contains the core functions for the Longitudinal GRMTree method:
##   - longitudinal_grmfit(): Internal fitting function for the constrained
##     two-factor longitudinal GRM (production version: builds constraints
##     directly from the known parameter structure instead of fitting an
##     unconstrained model first, making it fast enough for repeated calls
##     inside MOB).
##   - longitudinal_grmtree(): Main tree-fitting function using MOB.
##   - prepare_longitudinal_data(): Data preparation helper.
##
## Note: Phase 2 response shift characterization (rs_characterize) and its
## print method live in a separate file (rs_characterize.R).
##
## Author: Olayinka Arimoro
## Date: February 2026
##
################################################################################

#' Internal Function: Fit Constrained Longitudinal Graded Response Model
#'
#' Fits a constrained two-factor longitudinal graded response model (GRM) to
#' wide-format item response data from two time points. This is an internal
#' function called by \code{\link{longitudinal_grmtree}} during model-based
#' recursive partitioning and is not intended to be used directly.
#'
#' This production version builds the equality constraints directly from the
#' known parameter structure rather than fitting an unconstrained model first to
#' discover that structure. Because the function is called hundreds of times
#' during tree construction, avoiding the extra (unconstrained) model fit at
#' every node yields a substantial speed-up with no change in results.
#'
#' @param y A numeric matrix of item responses in wide format. The first
#'   \code{n_items} columns contain responses at Time 1 (T1) and the next
#'   \code{n_items} columns contain responses at Time 2 (T2), for a total of
#'   \code{2 * n_items} columns. Each row represents one individual. The number
#'   of columns must be even and at least 4.
#' @param x Optional predictor matrix. Handled internally by the MOB framework
#'   and not used directly in model fitting.
#' @param start Optional starting values for model parameters. Passed to
#'   \code{\link[mirt]{mirt}}.
#' @param weights Optional case weights. Not currently used.
#' @param offset Optional offset. Not currently used.
#' @param estfun Logical indicating whether to compute empirical estimating
#'   (score) functions for the MOB parameter instability tests. When
#'   \code{TRUE}, standard errors are computed during model fitting (SE = TRUE)
#'   and \code{\link[mirt]{estfun.AllModelClass}} is called to extract the score
#'   contributions for each individual. Default is \code{FALSE}.
#' @param object Logical indicating whether to return the full
#'   \code{\link[mirt]{mirt}} model object. Default is \code{FALSE}. Set to
#'   \code{TRUE} when the fitted model is needed for post-hoc analyses (e.g., in
#'   \code{\link{rs_characterize}}).
#' @param ... Additional arguments passed to \code{\link[mirt]{mirt}}.
#'
#' @return A list containing:
#'   \item{coefficients}{Item parameter estimates from
#'     \code{mirt::coef(fit, IRTpars = TRUE, simplify = TRUE)}, including
#'     discrimination (\code{a1}, \code{a2}) and threshold (\code{b1}, ...,
#'     \code{bK}) parameters for all \code{2 * n_items} items, plus latent
#'     trait means and covariance matrix.}
#'   \item{objfun}{Negative log-likelihood of the fitted model. Used by MOB
#'     to evaluate splits via partitioned log-likelihood maximization.}
#'   \item{estfun}{Empirical estimating (score) functions if \code{estfun =
#'     TRUE}; \code{NULL} otherwise. An \code{N x p} matrix where \code{N}
#'     is the number of individuals and \code{p} is the number of model
#'     parameters. Used by MOB for score-based structural change tests.}
#'   \item{object}{The full \code{SingleGroupClass} model object
#'     if \code{object = TRUE}; \code{NULL} otherwise.}
#'
#' @details ## Constrained Two-Factor Longitudinal GRM
#'
#' The function specifies a two-factor GRM where the latent factors
#' \eqn{\theta_{T1}} and \eqn{\theta_{T2}} represent the construct at Time 1 and
#' Time 2, respectively. The model structure is:
#'
#' \deqn{\theta_{T1} \sim N(0, 1)} \deqn{\theta_{T2} \sim N(\mu_{T2},
#' \sigma^2_{T2})} \deqn{Cov(\theta_{T1}, \theta_{T2}) = \sigma_{12}}
#'
#' Items 1 through \eqn{M} load on \eqn{\theta_{T1}} and items \eqn{M+1} through
#' \eqn{2M} load on \eqn{\theta_{T2}}, where \eqn{M} is the number of items per
#' time point.
#'
#' Under the null hypothesis of no response shift, item parameters are
#' constrained equal across time points. For each item \eqn{m}:
#' \itemize{
#'   \item Discrimination: \eqn{a_{1,m} = a_{2,m+M}} (the T1 item's loading
#'     on \eqn{\theta_{T1}} equals the T2 item's loading on \eqn{\theta_{T2}})
#'   \item Thresholds: \eqn{b_{k,m} = b_{k,m+M}} for all category thresholds
#'     \eqn{k = 1, \ldots, K_m}
#' }
#'
#' The number of threshold constraints per item pair is determined by the
#' minimum number of observed response categories across T1 and T2 for that item
#' (minus 1). This handles cases where sparse response categories at one time
#' point produce fewer estimated thresholds (e.g., category 5 may be empty at T1
#' but observed at T2).
#'
#' ## Constraint Syntax
#'
#' Constraints are specified using \code{mirt}'s CONSTRAIN syntax:
#' \itemize{
#'   \item Discrimination: \code{(i, a1, j, a2)} constrains item \code{i}'s
#'     \code{a1} parameter to equal item \code{j}'s \code{a2} parameter
#'   \item Thresholds: \code{(i, j, dk)} constrains the \code{dk} threshold
#'     to be equal between items \code{i} and \code{j}
#' }
#'
#' where \code{i} is the T1 item index and \code{j = i + n_items} is the
#' corresponding T2 item index.
#'
#' ## Technical Details
#'
#' The EM algorithm is run with a maximum of 1000 cycles (\code{technical =
#' list(NCYCLES = 1000)}) to accommodate the complexity of the constrained
#' two-factor model. Verbose output from \code{mirt} is suppressed. If the model
#' fails to converge or encounters an error, the function stops with an
#' informative error message.
#'
#' @seealso \code{\link{grmtree}} fits a Graded Response Model Tree,
#'   \code{\link{longitudinal_grmtree}} for the main tree-fitting function that
#'   calls this internally, \code{\link[mirt]{mirt}} for the underlying IRT
#'   estimation engine, \code{\link{rs_characterize}} for post-hoc response
#'   shift testing within terminal nodes
#'
#' @keywords internal
#' @importFrom mirt mirt mirt.model coef logLik estfun.AllModelClass
longitudinal_grmfit <- function(y, x = NULL, start = NULL, weights = NULL,
                                offset = NULL, ...,
                                estfun = FALSE, object = FALSE) {
  if (!is.matrix(y)) y <- as.matrix(y)
  n_cols <- ncol(y)
  if (n_cols < 4 || n_cols %% 2 != 0) {
    stop("Response matrix must have an even number of columns. Found: ", n_cols)
  }
  n_items <- n_cols / 2
  # ---- Detect number of thresholds PER ITEM PAIR ----
  # Items may have different observed categories at T1 vs T2
  # (e.g., category 5 may be empty at T1 but observed at T2)
  # We must constrain only the thresholds that BOTH items have.
  # Use the MINIMUM category count within each T1-T2 pair.
  n_cats_per_col <- apply(y, 2, function(col) length(unique(na.omit(col))))
  # ---- Build model specification ----
  model_str <- paste0(
    'Theta_T1 = 1-', n_items, '\n',
    'Theta_T2 = ', n_items + 1, '-', 2 * n_items, '\n',
    'COV = Theta_T1*Theta_T2\n',
    'MEAN = Theta_T2'
  )
  # ---- Build constraints per item pair ----
  constraint_parts <- character(0)
  for (i in 1:n_items) {
    j <- i + n_items
    # Constrain discrimination: a1 of T1 item = a2 of T2 item
    constraint_parts <- c(constraint_parts, paste0("(", i, ", a1, ", j, ", a2)"))
    # Constrain thresholds: use min categories across T1 and T2 for this item
    n_thresh_i <- min(n_cats_per_col[i], n_cats_per_col[j]) - 1
    if (n_thresh_i > 0) {
      for (d in 1:n_thresh_i) {
        constraint_parts <- c(constraint_parts, paste0("(", i, ", ", j, ", d", d, ")"))
      }
    }
  }
  all_constraints <- paste(constraint_parts, collapse = ", ")
  # ---- Fit constrained model (ONE model, not two) ----
  model_spec <- paste0(model_str, '\nCONSTRAIN = ', all_constraints)
  fit <- tryCatch({
    mirt::mirt(
      data = y,
      model = mirt::mirt.model(model_spec),
      itemtype = 'graded',
      SE = estfun,
      verbose = FALSE,
      technical = list(NCYCLES = 1000),
      ...
    )
  }, error = function(e) {
    stop("Longitudinal GRM fitting failed: ", e$message)
  })
  # ---- Extract coefficients ----
  coefs <- mirt::coef(fit, IRTpars = TRUE, simplify = TRUE)
  if (is.null(coefs)) stop("No coefficients returned.")
  # ---- Extract estimating functions ----
  ef <- NULL
  if (estfun) {
    ef <- tryCatch({
      mirt::estfun.AllModelClass(fit)
    }, error = function(e) {
      warning("estfun extraction failed: ", e$message)
      NULL
    })
  }
  list(
    coefficients = coefs,
    objfun = -as.numeric(mirt::logLik(fit)),
    estfun = ef,
    object = if (object) fit else NULL
  )
}

#' Fit a Longitudinal Graded Response Model Tree for Response Shift Detection
#'
#' This function implements a tree-based longitudinal graded response model
#' using model-based recursive partitioning (MOB) to detect measurement
#' heterogeneity in patient-reported outcome measures (PROMs) measured at two
#' time points. The Longitudinal GRMTree extends the cross-sectional
#' \code{\link[grmtree]{grmtree}} to the longitudinal setting by embedding
#' a constrained two-factor GRM within the MOB framework. The resulting tree
#' identifies patient subgroups where the longitudinal measurement model
#' differs, providing the foundation for subgroup-specific response shift
#' characterization via \code{\link{rs_characterize}}.
#'
#' @param formula A formula specifying the model structure with a wide-format
#'   response matrix on the left-hand side and partitioning variables on the
#'   right-hand side (e.g., \code{resp_wide ~ age + sex + comorbidity_count}).
#'   The response matrix must have \code{2 * n_items} columns: the first
#'   \code{n_items} columns are Time 1 (T1) responses and the next
#'   \code{n_items} columns are Time 2 (T2) responses for the same items.
#' @param data A data frame containing the variables in the formula. Must
#'   include the response matrix as a matrix-valued column (created via
#'   \code{data$resp_wide <- as.matrix(...)}) and all partitioning variables
#'   as separate columns.
#' @param n_items Integer specifying the number of items per time point. If
#'   \code{NULL} (default), automatically detected as half the number of
#'   columns in the response matrix. Must satisfy
#'   \code{ncol(response) == 2 * n_items}.
#' @param na.action How to handle missing values. Default is \code{na.omit},
#'   which removes rows with any missing values in the response matrix or
#'   partitioning variables.
#' @param control A list of control parameters created by
#'   \code{\link[grmtree]{grmtree.control}}. Key parameters include:
#'   \describe{
#'     \item{\code{minbucket}}{Minimum number of observations in a terminal
#'       node. Should be at least 10 times the number of free parameters per
#'       node to ensure stable estimation. For 8 items with 5 categories each,
#'       the constrained longitudinal GRM has approximately 40 item parameters
#'       plus 3 latent parameters, suggesting \code{minbucket >= 200}.}
#'     \item{\code{alpha}}{Significance level for the parameter instability
#'       tests. Default is 0.05.}
#'     \item{\code{p_adjust}}{Method for adjusting p-values across covariates
#'       at each split. Options include \code{"bonferroni"} (default, applied
#'       locally during tree construction), \code{"BH"}
#'       (Benjamini-Hochberg, applied post-hoc with pruning), and others.
#'       See \code{\link[grmtree]{grmtree.control}} for details.}
#'   }
#' @param mtry Number of variables randomly sampled as candidates at each
#'   split. If \code{NULL} (default), all variables are considered. Can be
#'   used for random forest extensions.
#' @param ... Additional arguments passed to the internal fitting function
#'   \code{\link{longitudinal_grmfit}} and ultimately to
#'   \code{\link[mirt]{mirt}}.
#'
#' @return An object of class \code{c("longitudinal_grmtree", "grmtree",
#'   "modelparty", "party")} containing the fitted tree structure. The object
#'   inherits from \code{\link[partykit]{modelparty}} and includes:
#'   \describe{
#'     \item{Tree structure}{Accessible via standard \code{partykit} methods
#'       such as \code{\link[partykit]{nodeids}},
#'       \code{\link[partykit]{data_party}}, and indexing with \code{[[}.}
#'     \item{Node models}{Each terminal node contains a fitted constrained
#'       longitudinal GRM with item parameters (discrimination and thresholds),
#'       latent trait means (\eqn{\mu_{T2}}), and latent trait covariance
#'       matrix (\eqn{\sigma^2_{T2}}, \eqn{r_{T1,T2}}).}
#'     \item{\code{info$n_items}}{Number of items per time point.}
#'     \item{\code{info$model_type}}{\code{"longitudinal_grm"}.}
#'     \item{\code{info$p_adjust}}{The p-value adjustment method used.}
#'     \item{\code{info$call}}{The original function call.}
#'   }
#'
#' @details
#' ## Overview
#'
#' The Longitudinal GRMTree is a unified framework for response shift
#' detection that operates in two phases:
#'
#' \strong{Phase 1 (this function):} Identifies patient subgroups where the
#' constrained longitudinal measurement model differs. The constrained model
#' represents the null hypothesis of no response shift (item parameters equal
#' across time). MOB tests whether this null model's parameters are stable
#' across patient covariates and recursively partitions the sample where
#' instability is detected.
#'
#' \strong{Phase 2 (\code{\link{rs_characterize}}):} Within each terminal
#' node, tests whether item parameters actually change from T1 to T2 by
#' comparing the constrained model to an unconstrained model using likelihood
#' ratio tests. This phase characterizes response shift at the item level,
#' classifying changes as recalibration, reprioritization, or both.
#'
#' ## Constrained Two-Factor Longitudinal GRM
#'
#' Let \eqn{Y_{im}} denote the response of individual \eqn{i} to item \eqn{m}
#' at time \eqn{t}. The longitudinal GRM specifies two correlated latent
#' factors:
#'
#' \deqn{\theta_{i,T1} \sim N(0, 1)}
#' \deqn{\theta_{i,T2} \sim N(\mu_{T2}, \sigma^2_{T2})}
#' \deqn{Cov(\theta_{i,T1}, \theta_{i,T2}) = \sigma_{12}}
#'
#' For item \eqn{m} at time \eqn{t}, the graded response model is:
#'
#' \deqn{P(Y_{im,t} \geq j | \theta_{i,t}) = \frac{\exp(a_{m,t}(\theta_{i,t}
#' - b_{mj,t}))}{1 + \exp(a_{m,t}(\theta_{i,t} - b_{mj,t}))}}
#'
#' where \eqn{a_{m,t}} is the discrimination parameter and \eqn{b_{mj,t}} are
#' threshold parameters for item \eqn{m} at time \eqn{t}.
#'
#' Under the no-response-shift constraint:
#' \deqn{a_{m,T1} = a_{m,T2} \quad \text{and} \quad b_{mj,T1} = b_{mj,T2}
#' \quad \forall m, j}
#'
#' This means item parameters are identical across time, so any observed
#' changes in responses are attributed to true latent change (\eqn{\mu_{T2}})
#' rather than changes in measurement properties.
#'
#' ## The Longitudinal GRMTree Algorithm
#'
#' \strong{Step 1: Global Model Estimation.} Fit the constrained longitudinal
#' GRM to all individuals at the root node, estimating item parameters
#' \eqn{\hat{\beta}} and latent parameters \eqn{(\mu_{T2}, \sigma^2_{T2},
#' \sigma_{12})} via maximum likelihood.
#'
#' \strong{Step 2: Parameter Stability Testing.} For each covariate
#' \eqn{X_p}, compute individual score function contributions
#' \eqn{s(\hat{\beta}; \mathbf{y}_i)} and test whether these scores
#' fluctuate systematically with \eqn{X_p} using structural change tests.
#' The null hypothesis is that model parameters are stable across all
#' values of \eqn{X_p}.
#'
#' \strong{Step 3: Recursive Partitioning.} If significant instability is
#' detected (after p-value adjustment):
#' \itemize{
#'   \item Select the covariate \eqn{X_p^*} with strongest instability
#'   \item Find the optimal split point \eqn{c^*} maximizing the partitioned
#'     log-likelihood
#'   \item Split the sample: \eqn{X_p^* \leq c^*} vs. \eqn{X_p^* > c^*}
#' }
#'
#' \strong{Step 4: Recursion.} Repeat Steps 1--3 within each child node
#' until no significant instability remains or the minimum node size is
#' reached.
#'
#' \strong{Step 5 (Post-hoc):} Apply \code{\link{rs_characterize}} to test
#' for response shift within each terminal node.
#'
#' ## Formal Model Structure
#'
#' The fitted Longitudinal GRMTree provides a piecewise constrained
#' longitudinal GRM:
#'
#' \deqn{P(Y_{im,t} = k | \theta_{i,t}, \mathbf{x}_i) = \sum_{b=1}^B
#' I(\mathbf{x}_i \in \mathcal{X}_b) \cdot P_b(Y_{im,t} = k | \theta_{i,t})}
#'
#' where \eqn{B} is the number of terminal nodes, \eqn{\mathcal{X}_b} is
#' the covariate subspace defining terminal node \eqn{b}, and \eqn{P_b} is
#' the node-specific constrained longitudinal GRM. Each terminal node
#' contains:
#' \itemize{
#'   \item Node-specific constrained item parameters (equal across T1 and T2)
#'   \item Node-specific latent trait parameters: \eqn{\mu_{T2,b}},
#'     \eqn{\sigma^2_{T2,b}}, \eqn{r_{T1,T2,b}}
#' }
#'
#' ## Interpretation
#'
#' The tree structure identifies patient subgroups where the constrained
#' longitudinal measurement model differs. This can reflect:
#' \itemize{
#'   \item Different item discrimination patterns across subgroups
#'   \item Different threshold locations across subgroups
#'   \item Different magnitudes of true latent change (\eqn{\mu_{T2}})
#'   \item Different test-retest correlations
#' }
#'
#' Crucially, the tree does \strong{not} directly detect response shift
#' (temporal change in item parameters within a subgroup). Response shift
#' is tested in Phase 2 using \code{\link{rs_characterize}}, which relaxes
#' the equality constraints within each terminal node. A tree with only one
#' terminal node (no split) indicates that no covariate moderates the
#' measurement model; it does \strong{not} imply the absence of response
#' shift, since \code{\link{rs_characterize}} can still detect uniform RS in
#' the unsplit root node.
#'
#' ## Relationship to Existing Methods
#'
#' The Longitudinal GRMTree extends several existing approaches:
#' \itemize{
#'   \item \strong{GRMTree} (Arimoro et al., 2026): Cross-sectional DIF
#'     detection using tree-based GRM. The longitudinal extension embeds a
#'     two-factor model instead of a single-factor model.
#'   \item \strong{LIRTree} (Ames & Leventhal, 2021): Longitudinal
#'     tree-based IRT using Rasch/2PL models. The GRM extension allows
#'     item-specific discrimination parameters, which are important for
#'     PROMs where items vary in discriminating ability.
#'   \item \strong{Oort SEM} (Oort, 2005): Response shift detection via
#'     structural equation modeling. The tree-based approach removes the
#'     requirement for pre-specified subgroups.
#' }
#'
#' ## Practical Recommendations
#'
#' \itemize{
#'   \item \strong{Minimum node size:} Use at least 10--25 times the number
#'     of free parameters per node. For \eqn{M} items with \eqn{K} response
#'     categories, the constrained model has approximately \eqn{M(K-1) + M + 3}
#'     parameters (\eqn{M} discriminations, \eqn{M(K-1)} thresholds, and 3
#'     latent parameters). For 8 items with 5 categories:
#'     \eqn{8 \times 4 + 8 + 3 = 43} parameters, suggesting
#'     \code{minbucket = 200--400}.
#'   \item \strong{Number of covariates:} Bonferroni correction becomes
#'     increasingly conservative with more covariates. Consider using
#'     \code{p_adjust = "BH"} for exploratory analyses with many covariates.
#'   \item \strong{Sample size:} With typical PROM instruments (5--15 items),
#'     a total sample of at least 500--1000 is recommended for adequate power
#'     to detect meaningful splits.
#'   \item \strong{Response matrix preparation:} Use
#'     \code{\link{prepare_longitudinal_data}} to construct the wide-format
#'     response matrix from separate T1 and T2 item columns.
#' }
#'
#' @references
#' Arimoro, O. I., Lix, L. M., Patten, S. B., Sawatzky, R., Sebille, V.,
#' Liu, J., Wiebe, S., Josephson, C. B., & Sajobi, T. T. (2025). Tree-based
#' latent variable model for assessing differential item functioning in
#' patient-reported outcome measures: a simulation study. \emph{Quality of
#' Life Research}. \doi{10.1007/s11136-025-04018-6}
#'
#' Ames, A. J., & Leventhal, B. C. (2021). Application of a longitudinal
#' IRTree model: response style changes over time. \emph{Educational and
#' Psychological Measurement}, 81(3), 561--582.
#'
#' Oort, F. J. (2005). Using structural equation modeling to detect response
#' shifts and true change. \emph{Quality of Life Research}, 14(3), 587--598.
#'
#' Samejima, F. (1969). Estimation of latent ability using a response pattern
#' of graded scores. \emph{Psychometrika Monograph Supplement}, 34, 100--114.
#'
#' Sprangers, M. A., & Schwartz, C. E. (1999). Integrating response shift
#' into health-related quality of life research. \emph{Social Science &
#' Medicine}, 48(11), 1507--1515.
#'
#' Zeileis, A., Hothorn, T., & Hornik, K. (2008). Model-based recursive
#' partitioning. \emph{Journal of Computational and Graphical Statistics},
#' 17(2), 492--514.
#'
#' @author Olayinka Imisioluwa Arimoro \email{olayinka.arimoro@ucalgary.ca},
#'   Lisa M. Lix, Tolulope T. Sajobi
#'
#' @examplesIf interactive()
#' library(grmtree)
#'
#'   # Load the synthetic longitudinal data
#'   data("grmtree_long_data", package = "grmtree")
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
#'   # Print tree structure
#'   print(ltree)
#'
#'   # Plot threshold regions
#'   plot(ltree, type = "regions", tnex = 2L)
#'
#'   # Phase 2: characterize response shift within each subgroup
#'   rs <- rs_characterize(ltree, p_adjust = "fdr",
#'     global_p_adjust = "bonferroni")
#'   print(rs)
#'
#'
#' @seealso
#'   \code{\link{rs_characterize}} for Phase 2 response shift characterization,
#'   \code{\link{prepare_longitudinal_data}} for data preparation,
#'   \code{\link[grmtree]{grmtree}} for cross-sectional DIF detection,
#'   \code{\link[grmtree]{grmtree.control}} for control parameters,
#'   \code{\link[partykit]{mob}} for the underlying MOB framework
#'
#' @export
#' @importFrom partykit mob mob_control nodeids
#' @importFrom stats model.frame model.response na.omit p.adjust formula
longitudinal_grmtree <- function(formula, data, n_items = NULL,
                                 na.action = na.omit,
                                 control = grmtree.control(),
                                 mtry = NULL, ...) {
  if (!inherits(formula, "formula")) stop("'formula' must be a formula")
  if (!is.data.frame(data)) stop("'data' must be a data.frame")
  cl <- match.call(expand.dots = TRUE)
  mf <- model.frame(formula, data = data, na.action = na.action)
  y <- model.response(mf)
  if (!is.matrix(y)) {
    stop("Response variable must be a wide-format matrix with 2*n_items columns.")
  }
  if (is.null(n_items)) {
    if (ncol(y) %% 2 != 0) {
      stop("Odd number of columns (", ncol(y), "). Specify n_items.")
    }
    n_items <- ncol(y) / 2
    message("Auto-detected n_items = ", n_items)
  } else if (ncol(y) != 2 * n_items) {
    stop("Expected ", 2 * n_items, " columns, found ", ncol(y))
  }
  if (n_items < 2) stop("Need at least 2 items per time point.")
  # Fitting function closure
  long_grmfit_wrapper <- function(y, x = NULL, start = NULL, weights = NULL,
                                  offset = NULL, ..., estfun = FALSE,
                                  object = FALSE) {
    longitudinal_grmfit(y = y, x = x, start = start, weights = weights,
                        offset = offset, estfun = estfun, object = object, ...)
  }
  p_adjust <- if (!is.null(control$p_adjust)) control$p_adjust else "none"
  alpha <- control$alpha
  minbucket <- control$minsize
  if (p_adjust %in% c("holm", "BH", "BY", "hochberg", "hommel")) {
    initial_alpha <- control$initial_alpha
    ctrl_full <- partykit::mob_control(
      minbucket = minbucket, bonferroni = FALSE,
      alpha = initial_alpha, ytype = "matrix"
    )
    rval <- partykit::mob(
      formula = formula, data = data,
      fit = long_grmfit_wrapper,
      control = ctrl_full, na.action = na.action
    )
    if (exists(".adjust_and_prune_tree")) {
      rval <- .adjust_and_prune_tree(rval, p_adjust, alpha)
    }
  } else {
    rval <- partykit::mob(
      formula = formula, data = data,
      fit = long_grmfit_wrapper,
      control = control, na.action = na.action
    )
  }
  rval$info$p_adjust <- p_adjust
  rval$info$call <- cl
  rval$info$n_items <- n_items
  rval$info$model_type <- "longitudinal_grm"
  class(rval) <- c("longitudinal_grmtree", "grmtree", "modelparty", "party")
  return(rval)
}

#' Prepare Wide-Format Longitudinal PROM Data
#'
#' Constructs a wide-format data frame suitable for
#' \code{\link{longitudinal_grmtree}} from separate Time 1 and Time 2 item
#' response columns. The function creates a matrix-valued column containing
#' the concatenated T1 and T2 responses, along with any specified covariates.
#'
#' @param data A data frame containing item response columns for both time
#'   points and any covariates.
#' @param items_t1 Character vector of column names for Time 1 item responses,
#'   in the order they should appear in the response matrix.
#' @param items_t2 Character vector of column names for Time 2 item responses,
#'   in the same order as \code{items_t1}. Must have the same length.
#' @param covariates Optional character vector of column names for covariates
#'   to include in the output data frame (e.g., \code{c("age", "sex")}).
#' @param id Optional character string specifying the subject ID column name.
#'   If provided, the ID column is included in the output.
#'
#' @return A data frame with:
#'   \describe{
#'     \item{Covariates}{All columns specified in \code{covariates}}
#'     \item{\code{resp_wide}}{A matrix-valued column with
#'       \code{2 * length(items_t1)} columns. The first half contains T1
#'       responses (named \code{Item1_T1}, ..., \code{ItemM_T1}) and the
#'       second half contains T2 responses (named \code{Item1_T2}, ...,
#'       \code{ItemM_T2}).}
#'   }
#'   Rows with any missing values in the response matrix are removed, with
#'   a message indicating how many rows were dropped.
#'
#' @examples
#' library(grmtree)
#'
#' # Load the synthetic longitudinal data
#' data("grmtree_long_data", package = "grmtree")
#'
#'   # Prepare the wide-format response matrix from separate T1 and T2 columns
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
#'   # Check structure
#'   str(ld$resp_wide)  # 1500 x 16 matrix
#'
#' @seealso \code{\link{longitudinal_grmtree}} for fitting the tree
#'
#' @export
#' @importFrom stats complete.cases
prepare_longitudinal_data <- function(data, items_t1, items_t2,
                                      covariates = NULL, id = NULL) {
  if (length(items_t1) != length(items_t2)) {
    stop("items_t1 and items_t2 must have the same length.")
  }
  all_cols <- c(items_t1, items_t2, covariates, id)
  missing <- setdiff(all_cols, names(data))
  if (length(missing) > 0) stop("Columns not found: ", paste(missing, collapse = ", "))
  resp_mat <- as.matrix(data[, c(items_t1, items_t2)])
  colnames(resp_mat) <- c(paste0("Item", seq_along(items_t1), "_T1"),
                          paste0("Item", seq_along(items_t2), "_T2"))
  out <- data.frame(row.names = 1:nrow(data))
  if (!is.null(covariates)) for (cov in covariates) out[[cov]] <- data[[cov]]
  if (!is.null(id)) out[[id]] <- data[[id]]
  out$resp_wide <- resp_mat
  complete <- complete.cases(resp_mat)
  if (sum(!complete) > 0) {
    message("Removed ", sum(!complete), " incomplete rows. Remaining: ", sum(complete))
    out <- out[complete, ]
  }
  return(out)
}
