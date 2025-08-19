
#' Plot Variable Importance
#'
#' Creates a bar plot of variable importance scores with options for both
#' ggplot2 and base R graphics.
#'
#' @param x A `varimp` object from `varimp()`.
#' @param top_n Number of top variables to display (NULL for all).
#' @param use_ggplot Logical indicating whether to use ggplot2 (if available).
#' @param ... Additional arguments passed to plotting functions.
#'
#' @return Invisibly returns the input object.
#'
#' @examples
#' \donttest{
#'  library(grmtree)
#'  library(hlt)
#'  data("asti", package = "hlt")
#'  asti$resp <- data.matrix(asti[, 1:4])
#'
#'  # Fit forest with default parameters
#'  forest <- grmforest(resp ~ gender + group, data = asti)
#'  imp <- varimp(forest)
#'  plot(imp)
#'  plot(imp, top_n = 1) ## select top 1 importance variable
#'  plot(imp, use_ggplot = FALSE) # Use base R graphics
#' }
#'
#' @seealso \code{\link{varimp}} calculates the variable importance for GRM
#' Forest, \code{\link{grmforest}} for GRM Forests,
#' \code{\link{grmforest.control}} creates a control object for `grmforest`,
#' \code{\link{plot.grmtree}} creates plot for the `grmtree` object
#'
#' @export
#' @importFrom graphics barplot par text abline
#' @importFrom rlang .data

plot.varimp <- function(x, top_n = NULL, use_ggplot = TRUE, ...) {
  if (!inherits(x, "varimp")) {
    stop("'x' must be a varimp object from varimp()")
  }

  # Convert to named numeric vector preserving class
  x_vec <- unclass(x)

  # Handle zero-length case
  if (length(x_vec) == 0) {
    stop("No importance values to plot")
  }

  # Handle top_n selection
  if (!is.null(top_n)) {
    if (!is.numeric(top_n) || length(top_n) != 1 || top_n < 1) {
      stop("'top_n' must be NULL or a positive integer")
    }
    top_n <- min(top_n, length(x_vec))
    x_vec <- x_vec[seq_len(top_n)]
  }

  # Calculate smart xlim that works for all cases
  x_range <- range(x_vec)
  if (all(x_vec >= 0)) {
    # All positive values - extend upper limit only
    xlim <- c(0, x_range[2] * 1.1)
  } else if (all(x_vec <= 0)) {
    # All negative values - extend lower limit only
    xlim <- c(x_range[1] * 1.1, 0)
  } else {
    # Mixed positive and negative
    xlim <- x_range * 1.1
  }

  if (use_ggplot) {
    # ggplot2 version
    imp_data <- data.frame(
      Variable = factor(names(x_vec), levels = names(x_vec)[order(x_vec)]),
      Importance = as.numeric(x_vec)
    )

    p <- ggplot2::ggplot(imp_data, ggplot2::aes(x = .data$Importance,
                                                y = stats::reorder(.data$Variable, .data$Importance))) +
      ggplot2::geom_col(fill = ifelse(imp_data$Importance >= 0, "skyblue", "salmon"),
                        width = 0.7, alpha = 0.8) +
      ggplot2::labs(
        title = "Variable Importance",
        x = "Importance Score (log-likelihood difference)",
        y = NULL
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
        panel.grid.major.y = ggplot2::element_blank()
      ) +
      ggplot2::xlim(xlim)  # Use calculated xlim

    if (length(x_vec) <= 20) {
      p <- p +
        ggplot2::geom_text(
          ggplot2::aes(label = round(.data$Importance, 2)),
          hjust = ifelse(imp_data$Importance >= 0, -0.2, 1.2),
          size = 3
        )
    }

    print(p)

  } else {
    # Base R version
    op <- graphics::par(mar = c(5, max(8, max(nchar(names(x_vec))) / 1.5), 4, 2) + 0.1)
    on.exit(graphics::par(op))

    colors <- ifelse(x_vec >= 0, "skyblue", "salmon")

    bp <- graphics::barplot(
      as.numeric(x_vec),
      names.arg = names(x_vec),
      horiz = TRUE,
      las = 1,
      col = colors,
      main = "Variable Importance",
      xlab = "Importance Score (log-likelihood difference)",
      xlim = xlim,  # Use calculated xlim
      ...
    )

    graphics::text(
      x = ifelse(x_vec >= 0, x_vec, x_vec) + (ifelse(x_vec >= 0, 1, -1) * 0.02 * diff(x_range)),
      y = bp,
      labels = round(x_vec, 2),
      pos = ifelse(x_vec >= 0, 4, 2),
      cex = 0.8
    )

    graphics::abline(v = 0, lty = 2, col = "gray")
  }

  invisible(x)
}

# Register the method if not already registered
if (!"plot.varimp" %in% methods("plot")) {
  registerS3method("plot", "varimp", plot.varimp)
}
