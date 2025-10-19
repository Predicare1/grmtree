
#' Print Method for GRM Tree Objects
#'
#' Displays a formatted summary of a GRM (Graded Response Model) tree object.
#' This function extends \code{print.modelparty} from the partykit package with
#' specialized formatting for GRM trees.
#'
#' @param x A GRM tree object of class 'grmtree'.
#' @param title Character string specifying the title for the print output
#'   (default: "Graded Response Model Tree").
#' @param objfun Character string labeling the objective function
#'   (default: "negative log-likelihood").
#' @param ... Additional arguments passed to \code{print.modelparty}.
#'
#' @return Invisibly returns the GRM tree object. Primarily called for its side
#'   effect of printing a formatted summary.
#'
#' @details The print method provides a comprehensive summary of the GRM tree,
#' including:
#' \itemize{
#'   \item Model formula used for fitting
#'   \item Tree structure with node information
#'   \item Item parameter estimates for each terminal node
#'   \item Confidence intervals for parameters
#'   \item Group parameters (mean and covariance)
#'   \item Summary statistics (number of nodes, objective function value)
#' }
#'
#' @examples
#' \dontrun{
#' library(grmtree)
#' library(hlt)
#' data("asti", package = "hlt")
#' asti$resp <- data.matrix(asti[, 1:4])
#'
#' # Fit GRM tree
#' tree <- grmtree(resp ~ gender + group,
#'                 data = asti,
#'                 control = grmtree.control(minbucket = 30))
#'
#' # Print the tree summary
#' print(tree)
#'
#' # Alternative syntax (automatically calls print.grmtree)
#' tree
#' }
#'
#' @seealso \code{\link[partykit]{print.modelparty}} for the underlying printing
#'   infrastructure, \code{\link{grmtree}} for creating GRM tree objects
#'
#'
#' @method print grmtree
#' @export


print.grmtree <- function(x,
                          title = "Graded Response Model Tree",
                          objfun = "negative log-likelihood",
                          ...) {

  if (!inherits(x, "grmtree")) {
    stop("'x' must be a grmtree object")
  }

  # Explicitly call partykit's print method
  partykit::print.modelparty(x, title = title, objfun = objfun, ...)

  invisible(x)
}





















