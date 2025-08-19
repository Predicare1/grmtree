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

# Internal function - not exported
grm_fluc_test <- function(object, ...) {
  if (!inherits(object, "grmtree")) {
    stop("Internal error: object must be a grmtree object")
  }

  default_test <- partykit::mob_control()$ftest
  test_result <- default_test(object, ...)

  ctrl <- object$control
  p_adjust_method <- if (!is.null(ctrl$p_adjust)) ctrl$p_adjust else "none"

  if (p_adjust_method != "none") {
    test_result$p.value <- stats::p.adjust(test_result$p.value, method = p_adjust_method)
  }

  return(test_result)
}
