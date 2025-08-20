#' Internal Function: Fit Graded Response Model
#'
#' Fits a graded response model (GRM) to item response data. This is an internal
#' function called by \code{grmtree} and is not intended to be used directly.
#'
#' @param y A matrix of item responses (rows = persons, columns = items)
#' @param x Optional predictor matrix (not currently used)
#' @param start Optional starting values (passed to mirt)
#' @param weights Optional case weights (not currently used)
#' @param offset Optional offset (not currently used)
#' @param estfun Logical indicating whether to compute empirical estimating
#'   functions
#' @param object Logical indicating whether to return the full mirt model object
#' @param ... Additional arguments passed to \code{mirt::mirt}
#'
#' @return A list containing: \item{coefficients}{Item parameter estimates}
#'   \item{objfun}{Negative log-likelihood value} \item{estfun}{Empirical
#'   estimating functions if requested} \item{object}{Full mirt model object if
#'   requested}
#'
#' @importFrom mirt mirt logLik
#' @keywords internal
grmfit <- function(y, x = NULL, start = NULL, weights = NULL, offset = NULL, ...,
                   estfun = FALSE, object = FALSE) {

  # Validate input response matrix
  if (!is.matrix(y)) {
    stop("Response variable 'y' must be a matrix. Found: ", class(y))
  }

  if (ncol(y) < 2) {
    stop("Response matrix must have at least 2 columns (items). Found: ", ncol(y))
  }

  if (nrow(y) < 20) {
    warning("Small sample size (n < 20) may lead to unstable parameter estimates")
  }

  # Warn about unused arguments
  if (!(is.null(x) || NCOL(x) == 0L)) {
    warning("Predictor matrix 'x' is not used in GRM fitting")
  }

  if (!is.null(offset)) {
    warning("Offset term is not used in GRM fitting")
  }

  # Fit GRM model with error handling
  fit <- tryCatch(
    mirt::mirt(y, model = 1, itemtype = 'graded',
               SE = TRUE, verbose = FALSE, ...),
    error = function(e) {
      stop("GRM model fitting failed: ", e$message)
    },
    warning = function(w) {
      warning("GRM fitting warning: ", w$message)
      invokeRestart("muffleWarning")
    }
  )

  # Extract log-likelihood with error handling
  loglik <- tryCatch(
    mirt::logLik(fit),
    error = function(e) {
      stop("Could not extract log-likelihood: ", e$message)
    }
  )

  # Extract coefficients with error handling
  coefs <- tryCatch(
    mirt::coef(fit),
    error = function(e) {
      stop("Could not extract coefficients: ", e$message)
    }
  )

  # Prepare return value
  rval <- list(
    coefficients = coefs,
    objfun = -as.numeric(loglik),  # Convert to negative log-likelihood
    estfun = if (estfun) {
      tryCatch(
        mirt::estfun.AllModelClass(fit),
        error = function(e) {
          warning("Could not compute estimating functions: ", e$message)
          NULL
        }
      )
    } else NULL,
    object = if (object) fit else NULL
  )

  # Validate return structure
  if (is.null(rval$coefficients)) {
    stop("Model fitting failed - no coefficients returned")
  }

  if (!is.finite(rval$objfun)) {
    warning("Non-finite log-likelihood value returned")
  }

  return(rval)
}


#' Fit a Graded Response Model Tree for Differential Item Functioning Detection
#'
#' This function implements a tree-based graded response model (GRM) using
#' model-based recursive partitioning to detect and account for differential
#' item functioning (DIF) in polytomous items. The GRMTree combines the
#' statistical framework of item response theory with recursive partitioning to
#' identify heterogeneous subgroups in the population where item parameters
#' (discrimination and thresholds) vary systematically across covariates.
#'
#' The algorithm works by first estimating a global GRM for the entire sample,
#' then recursively testing for parameter instability with respect to available
#' covariates. When significant DIF is detected, the sample is partitioned into
#' homogeneous subgroups, each with their own set of item parameters. This
#' approach allows for the identification of complex interaction effects and
#' provides interpretable tree structures that visualize how item functioning
#' varies across different patient subgroups.
#'
#' GRMTree is particularly useful in health outcomes research where
#' patient-reported outcome measures may function differently across diverse
#' demographic, clinical, or socioeconomic subgroups. The resulting tree
#' diagrams facilitate the development of personalized assessment strategies and
#' can inform targeted interventions by identifying specific patient
#' characteristics associated with differential item interpretation.
#'
#' @param formula A formula specifying the model structure with the response
#'   matrix on the left and partitioning variables on the right (e.g.,
#'   `response_matrix ~ age + gender`).
#' @param data A data frame containing the variables in the model.
#' @param na.action How to handle missing values (default: `na.omit`).
#' @param control A list of control parameters created by `grmtree.control()`.
#' @param mtry Number of variables randomly sampled as candidates at each split.
#'   If NULL, all variables are considered.
#' @param ... Additional arguments passed to the fitting function.
#'
#' @return An object of class `grmtree` inheriting from `modelparty` containing
#'   the fitted tree structure.
#'
#' @details
#'
#' # Conventional Graded Response Model (GRM)
#'
#' Let \eqn{Y_{im}} denote the response of the \eqn{i^{th}} (\eqn{i=1,\ldots,N})
#' individual to the \eqn{m^{th}} (\eqn{m = 1,2,\ldots,M}) item. The graded
#' response model is described as:
#'
#' \deqn{P(Y_{im} \geq j | \tau_{mj}, \lambda_m, \theta_i) =
#' \frac{\exp(-(\tau_{mj} - \lambda_m \theta_i))}{1 + \exp(-(\tau_{mj} -
#' \lambda_m \theta_i))}}
#'
#' where:
#' - \eqn{P(Y_{im} \geq j | \tau_{mj}, \lambda_m, \theta_i)} is the probability that
#' individual \eqn{i}'s response is in category \eqn{j} or higher on item
#' \eqn{m},
#' - \eqn{\tau_{mj}} is the threshold parameter between categories \eqn{j-1} and
#' \eqn{j} for item \eqn{m},
#' - \eqn{\lambda_m} is the discrimination parameter for item \eqn{m},
#' - \eqn{\theta_i \sim N(0,1)} is the latent trait score for individual \eqn{i}.
#'
#' This parametrization is equivalent to the conventional IRT formulation where
#' item discrimination is \eqn{a_m = \lambda_m} and item difficulty is
#' \eqn{b_{mj} = \tau_{mj} / \lambda_m}.
#'
#' # Graded Response Model Tree (GRMTree) Implementation
#'
#' The GRMTree is a hybrid model that integrates the GRM with model-based
#' recursive partitioning to detect and account for differential item
#' functioning (DIF) across subgroups defined by covariates. The algorithm
#' proceeds through the following steps:
#'
#' **Step 1: Global Model Estimation**
#'
#' Estimate the GRM item parameters \eqn{(\hat{\tau}_{mj}, \hat{\lambda}_m)}
#' jointly for all individuals in the study cohort at the root node via maximum
#' likelihood estimation:
#'
#' \deqn{\hat{\beta}_{\text{global}} = \arg\max_{\beta} \sum_{i=1}^N \log
#' L(\beta; \mathbf{y}_i)}
#'
#' where \eqn{\beta = (a_1, \ldots, a_J, b_{11}, \ldots, b_{J,m-1})} contains
#' all item parameters (discrimination and difficulty), providing a baseline
#' model assuming parameter invariance.
#'
#' **Step 2: Parameter Stability Testing**
#'
#' For each available covariate \eqn{X_p} (\eqn{p = 1, \ldots, P}), assess the
#' stability of the item parameters by conducting score-based structural change
#' tests. This involves: 1. Calculating the score function contributions
#' \eqn{s(\hat{\beta}; y_i, x_i)} for each individual, 2. Ordering these scores
#' with respect to each covariate \eqn{X_p}, 3. Testing the null hypothesis
#' \eqn{H_0: \mathbb{E}[s(\hat{\beta}; y_i, x_i)] = 0} for all \eqn{i} against
#' the alternative that scores fluctuate systematically with \eqn{X_p},
#' indicating parameter instability (DIF).
#'
#' **Step 3: Recursive Partitioning**
#'
#' If significant instability is detected (\eqn{p < \alpha_{\text{adj}}}):
#' - **Covariate Selection:** Identify the covariate \eqn{X_p^*} with the most
#' significant instability (smallest adjusted p-value),
#' - **Split Point Determination:** Find the optimal cut-point \eqn{c^*} that
#' maximizes the partitioned log-likelihood: \deqn{\ell_{\text{left}}(\beta) +
#' \ell_{\text{right}}(\beta) = \sum_{i: X_{pi} \leq c^*} \log L(\beta; y_i) +
#' \sum_{i: X_{pi} > c^*} \log L(\beta; y_i)} over all possible cut-points on
#' \eqn{X_p^*},
#' - **Sample Splitting:** Partition the sample into two child nodes based on the
#' rule \eqn{X_p^* \leq c^*}.
#'
#' **Step 4: Recursive Application & Stopping Criteria**
#'
#' Repeat Steps 1-3 recursively within each resulting child node until one of
#' the following stopping criteria is met:
#'
#' 1. **No Significant Instability:** No covariate shows significant parameter
#' instability after multiple testing correction (\eqn{\alpha_{\text{adj}} =
#' \alpha/m}, where multiple adjustment methods can be applied, including
#' Bonferroni, Holm, Benjamini-Hochberg, etc.).
#'
#' 2. **Minimum Node Size:** The subsample size falls below a prespecified
#' minimum (e.g., \eqn{n < 10 \times} the number of item parameters).
#'
#' # Formal GRMTree Structure
#'
#' The final GRMTree provides a piecewise GRM where each terminal node
#' represents a subgroup with homogeneous item parameters, explicitly modeling
#' the detected DIF structure within the data. The resulting GRMTree model can
#' be expressed as a mixture of subgroup-specific GRMs:
#'
#' \deqn{P(Y_{ij} = k | \theta_i, \mathbf{x}_i) = \sum_{b=1}^B I(\mathbf{x}_i
#' \in \mathcal{X}_b) \cdot P_b(Y_{ij} = k | \theta_i)}
#'
#' where:
#' - \eqn{B} is the number of terminal nodes,
#' - \eqn{\mathcal{X}_b} is the covariate subspace defining terminal node \eqn{b},
#' - \eqn{P_b} is the node-specific GRM with parameters \eqn{\beta_b},
#' - \eqn{I(\cdot)} is the indicator function.
#'
#' Each terminal node \eqn{b} contains a complete GRM with:
#' - **Node-specific item parameters:** \eqn{\beta_b = (a_{1b}, \ldots, a_{Jb}, b_{11b}, \ldots, b_{J,m-1,b})}
#' - **Local ability distribution:** \eqn{\theta_i | \mathbf{x}_i \in \mathcal{X}_b \sim N(0, 1)}
#'
#' This approach allows **differential item functioning (DIF)** to be detected
#' and modeled explicitly through the tree structure, where item parameters can
#' vary across subgroups defined by covariates, while maintaining the
#' conditional distribution of the latent trait within each subgroup.
#'
#' @references ## Methodological Foundations
#'
#'   Samejima, F. (1969). Estimation of latent ability using a response pattern
#'   of graded scores. Psychometrika Monograph Supplement, 34, 100-114.
#'
#'   Strobl, C., Kopf, J., & Zeileis, A. (2015). Rasch trees: A new method for
#'   detecting differential item functioning in the Rasch model. Psychometrika,
#'   80(2), 289-316.
#'
#'   Komboz, B., Strobl, C., & Zeileis, A. (2018). Tree-based global model tests
#'   for polytomous Rasch models. Educational and psychological measurement,
#'   78(1), 128-166. https://doi.org/10.1177/0013164416664394
#'
#'   Arimoro, O. I., Lix, L. M., Patten, S. B., Sawatzky, R., Sebille, V., Liu,
#'   J., Wiebe, S., Josephson, C. B., & Sajobi, T. T. (2025). Tree-based latent
#'   variable model for assessing differential item functioning in
#'   patient-reported outcome measures: a simulation study. Quality of Life
#'   Research. https://doi.org/10.1007/s11136-025-04018-6
#'
#'   ## Applied Examples
#'
#'   Arimoro, O. I., Josephson, C. B., James, M. T., Patten, S. B., Wiebe, S.,
#'   Lix, L. M., & Sajobi, T. T. (2024). Screening for depression in patients
#'   with epilepsy: same questions but different meaning to different patients.
#'   Quality of Life Research, 33(12), 3409-3419.
#'   https://doi.org/10.1007/s11136-024-03782-1
#'
#' @author Olayinka Imisioluwa Arimoro \email{olayinka.arimoro@ucalgary.ca},
#'   Lisa M. Lix, Tolulope T. Sajobi
#'
#' @examples
#' \donttest{
#'   library(grmtree)
#'   library(hlt)
#'
#'   # Prepare the asti data (from the hlt package)
#'   data("asti", package = "hlt")
#'   asti$resp <- data.matrix(asti[, 1:4])
#'
#'   # Fit GRM tree with gender and group as partitioning variables
#'   tree <- grmtree(resp ~ gender + group,
#'           data = asti,
#'           control = grmtree.control(minbucket = 30))
#'
#'   ## Print and plot the tree
#'   print(tree)
#'   plot(tree)
#'
#'   # Extract item parameters for specific subgroups
#'   discr_params <- discrpar_grmtree(tree)
#'   threshold_params <- threshpar_grmtree(tree)
#' }
#'
#' @seealso \code{\link{print.grmtree}} prints the detailed summary results of
#'   the `grmtree` object, \code{\link{grmtree.control}} creates a control
#'   object for `grmtree`, \code{\link{plot.grmtree}} creates plot for the
#'   `grmtree` object, \code{\link{grmforest}} for GRM Forests,
#'   \code{\link{varimp}} calculates the variable importance for GRM Forest,
#'   \code{\link{fscores_grmtree}} for computing factor scores,
#'   \code{\link{threshpar_grmtree}} for extracting threshold parameters,
#'   \code{\link{discrpar_grmtree}} for extracting discrimination parameters,
#'   \code{\link{itempar_grmtree}} for extracting item parameters
#'
#' @export
#' @importFrom partykit mob
#' @importFrom stats model.frame model.response na.omit
grmtree <- function(formula, data, na.action = na.omit,
                    control = grmtree.control(), mtry = NULL, ...) {

  # Validate inputs
  if (!inherits(formula, "formula")) {
    stop("'formula' must be a valid formula object")
  }

  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame")
  }

  # Keep call for later reference
  cl <- match.call(expand.dots = TRUE)

  # Parse the formula and handle missing values
  mf <- tryCatch(
    model.frame(formula, data = data, na.action = na.action),
    error = function(e) {
      stop("Error in model.frame: ", e$message)
    }
  )

  # Extract response matrix
  y <- model.response(mf)
  if (!is.matrix(y)) {
    stop("Response variable must be a matrix. Did you provide a multi-item response matrix?")
  }

  # Set custom fluctuation test if p-value adjustment is specified
  if (!is.null(control$p_adjust) && control$p_adjust != "none") {
    control$ftest <- grm_fluc_test
  }

  # Call mob with the custom control
  m <- match.call(expand.dots = FALSE)
  m$fit <- grmfit
  m$control <- control
  m$formula <- formula
  m$data <- data
  if ("..." %in% names(m)) m[["..."]] <- NULL
  m[[1L]] <- as.call(quote(partykit::mob))

  rval <- tryCatch(
    eval(m, parent.frame()),
    error = function(e) {
      stop("Error in tree fitting: ", e$message)
    }
  )

  # Extend class and keep original call
  rval$info$call <- cl
  class(rval) <- c("grmtree", "modelparty", "party")

  return(rval)
}

