#' Plot Method for GRM Tree Objects
#'
#' Visualizes a GRM (Graded Response Model) tree with different types of
#' terminal node plots. This function extends \code{plot.modelparty} from the
#' partykit package with specialized visualizations for GRM trees.
#'
#' @param x A GRM tree object of class 'grmtree'.
#' @param type Type of terminal node plot to display:
#'   \describe{
#'     \item{"regions"}{Threshold regions plot (default)}
#'     \item{"profile"}{Item parameter profile plot}
#'     \item{"histogram"}{Histogram of factor scores with normal curve}
#'   }
#' @param what Type of parameters to plot when \code{type = "profile"}:
#'   \describe{
#'     \item{"item"}{Both discrimination and threshold parameters (default)}
#'     \item{"threshold"}{Only threshold parameters}
#'     \item{"discrimination"}{Only discrimination parameters}
#'   }
#' @param tnex Numeric scaling factor for terminal node extension (default: 2).
#' @param drop_terminal Logical indicating whether to drop terminal node IDs
#'   (default: TRUE).
#' @param spacing Numeric value controlling spacing between elements (default:
#'   0.1).
#' @param ... Additional arguments passed to the terminal panel functions.
#'
#' @return Invisibly returns the GRM tree object. Primarily called for its side
#'   effect of producing a plot.
#'
#' @details The function provides three visualization types:
#' \itemize{
#'   \item \strong{Regions plot}: Shows threshold parameters as colored regions, useful
#'     for visualizing the difficulty parameters across items and nodes.
#'   \item \strong{Profile plot}: Displays either item parameters (discrimination and
#'     average thresholds), just thresholds, or just discrimination parameters as
#'     line plots across items.
#'   \item \strong{Histogram}: Shows the distribution of factor scores in each node
#'     with an overlaid normal curve.
#' }
#'
#' @examples
#' \dontrun{
#'
#' library(grmtree)
#' library(hlt)
#' data("asti", package = "hlt")
#' asti$resp <- data.matrix(asti[, 1:4])
#'
#' # Fit GRM tree with gender and group as partitioning variables
#' tree <- grmtree(resp ~ gender + group,
#'           data = asti,
#'           control = grmtree.control(minbucket = 30))
#'
#' # Default regions plot
#' plot(tree)
#'
#' # Profile plot showing item parameters
#' plot(tree, type = "profile")
#'
#' # Profile plot showing only thresholds
#' plot(tree, type = "profile", what = "threshold")
#'
#' # Histograms of factor scores
#' plot(tree, type = "histogram")
#' }
#'
#' @seealso \code{\link[partykit]{plot.modelparty}} for the underlying plotting
#'   infrastructure, \code{\link{grmtree}} for creating GRM tree objects,
#'   \code{\link{plot.varimp}} creates a bar plot of variable importance scores
#'
#' @method plot grmtree
#' @import grid
#' @export



plot.grmtree <- function(x, type = c("regions", "profile", "histogram"),
                         what = c("item", "threshold", "discrimination"),
                         tnex = 2L, drop_terminal = TRUE, spacing = 0.1, ...) {

  # Input validation
  if (!inherits(x, "grmtree")) {
    stop("The input object must be of class 'grmtree'.")
  }

  type <- match.arg(type)
  what <- match.arg(what)

  if (!is.numeric(tnex) || length(tnex) != 1 || tnex <= 0) {
    stop("'tnex' must be a single positive numeric value.")
  }

  if (!is.logical(drop_terminal) || length(drop_terminal) != 1) {
    stop("'drop_terminal' must be a single logical value.")
  }

  if (!is.numeric(spacing) || length(spacing) != 1 || spacing < 0) {
    stop("'spacing' must be a single non-negative numeric value.")
  }

  # Select appropriate terminal panel function
  terminal_panel <- switch(
    type,
    regions = {
      tryCatch(
        node_regionplot_grmtree(x, spacing = spacing, ...),
        error = function(e) {
          stop("Failed to create regions plot: ", e$message)
        }
      )
    },
    profile = {
      tryCatch(
        node_profileplot_grmtree(x, what = what, spacing = spacing, ...),
        error = function(e) {
          stop("Failed to create profile plot: ", e$message)
        }
      )
    },
    histogram = {
      tryCatch(
        node_histogram_grmtree(x, spacing = spacing, ...),
        error = function(e) {
          stop("Failed to create histogram plot: ", e$message)
        }
      )
    }
  )

  # Call partykit's plot.modelparty with our terminal panel
  tryCatch(
    {
      partykit::plot.modelparty(
        x,
        terminal_panel = terminal_panel,
        tnex = tnex,
        drop_terminal = drop_terminal,
        ...
      )
    },
    error = function(e) {
      stop("Plotting failed: ", e$message)
    }
  )

  # Invisibly return the tree object
  invisible(x)
}

# Function to create a histogram and normal curve for each terminal node with added horizontal spacing
node_histogram_grmtree <- function(grmtree_obj, fill = "lightgray", border = "black", spacing = 0.1, ...) {
  # Extract factor scores for all nodes in the tree
  scores_list <- fscores_grmtree(grmtree_obj)

  function(node) {
    # Get the node ID and corresponding factor scores
    node_id <- partykit::id_node(node)
    factor_scores <- scores_list[[as.character(node_id)]]
    if (is.null(factor_scores) || length(factor_scores) == 0) return()

    # Create histogram data for the factor scores
    hist_data <- graphics::hist(factor_scores, plot = FALSE, ...) # Generate histogram bins without plotting

    # Calculate mean and standard deviation for the normal curve
    mean_score <- mean(factor_scores)
    sd_score <- stats::sd(factor_scores)

    # Create x values for the normal curve
    x_vals <- seq(min(hist_data$breaks), max(hist_data$breaks), length.out = 100)

    # Scale the normal curve to match histogram height and width
    normal_curve <- stats::dnorm(x_vals, mean = mean_score, sd = sd_score) *
      length(factor_scores) * diff(hist_data$breaks[1:2])  # Correct scaling for bin width

    # Adjust horizontal spacing for this node
    x_shift <- node_id * spacing

    # Set up x and y scales based on histogram data
    x_scale <- range(hist_data$breaks) + c(-1, 1) * x_shift  # Shift x-range for spacing
    y_scale <- c(0, max(hist_data$counts, max(normal_curve))) # Ensure y-scale fits all elements
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(1, 1), name = paste0("node_", node_id)))
    grid::pushViewport(grid::viewport(xscale = x_scale, yscale = y_scale))

    # Draw histogram bars
    for (i in seq_along(hist_data$counts)) {
      grid::grid.rect(
        x = mean(hist_data$breaks[i:(i + 1)]),  # Center of each bar
        y = hist_data$counts[i] / 2,  # Center the height
        width = diff(hist_data$breaks[i:(i + 1)]),  # Bar width
        height = hist_data$counts[i],  # Bar height
        gp = grid::gpar(fill = fill, col = border),  # Fill and border colors
        default.units = "native"
      )
    }

    # Add the normal curve
    grid::grid.lines(
      x = x_vals,
      y = normal_curve,
      gp = grid::gpar(col = "blue", lwd = 2), # Line color and width
      default.units = "native"
    )

    ## Draw axes
    # Add x-axis
    grid::grid.xaxis(
      at = hist_data$breaks,  # Use histogram breaks for x-axis ticks
      label = round(hist_data$breaks, 2),  # Round labels for readability
      gp = grid::gpar(cex = 0.95),  # Adjust font size
      main = TRUE,  # Place the x-axis at the bottom
      edits = grid::gEdit(gPath = "labels", y = unit(-0.75, "lines"))  # Move labels closer to the axis
    )

    # Add node name and sample size below the histogram
    grid::grid.text(
      label = paste0("Node ", node_id, " (n = ", length(factor_scores), ")"),
      x = grid::unit(0.5, "npc"),  # Centered horizontally
      y = grid::unit(1, "npc") - unit(-0.9, "lines"),  # Position at the top of the plot
      gp = grid::gpar(fontface = "bold", cex = 1.0)  # Bold font with slightly reduced size
    )

    # Pop the viewport for this node
    grid::popViewport(2)
  }
}

node_regionplot_grmtree <- function(mobobj, names = FALSE, abbreviate = TRUE, type = c("mode", "median", "mean"),
                                     ref = NULL, ylim = NULL, off = 0.1, col_fun = grDevices::gray.colors, bg = "white",
                                     ylines = 2, spacing = 0.2) {
  ## Check input
  stopifnot(!is.null(mobobj))
  stopifnot(off >= 0)
  type <- match.arg(type)

  # Internal threshold extraction
  grm_threshpar_list <- function(object, node = NULL, ...) {
    if (is.null(node)) {
      node <- partykit::nodeids(object, terminal = TRUE)
    }
    # Extract thresholds for each node
    delta_lst <- lapply(node, function(n) {
      # Extract the model for the given node
      model <- partykit::nodeapply(object, ids = n, FUN = function(nd) nd$info$object)[[1]]

      # Extract threshold coefficients
      coef_model <- mirt::coef(model, IRTpars=T, simplify=TRUE)
      thresh <- coef_model$items[, grep("b", colnames(coef_model$items))]

      # Convert to a list of vectors (one per item)
      thresh_list <- split(thresh, seq(nrow(thresh)))
      names(thresh_list) <- rownames(thresh)
      return(thresh_list)
    })
    # Name the list by node IDs
    names(delta_lst) <- node
    return(delta_lst)
  }

  ## Setup parameters
  node <- partykit::nodeids(mobobj, terminal = TRUE)
  delta_lst <- grm_threshpar_list(mobobj, node = node)
  m <- max(sapply(delta_lst, length))
  xi <- 0:m + c(0:(m - 1), m - 1) * off
  xlim <- c(xi[1], xi[m + 1])

  ## Setup axis range
  if (is.null(ylim)) {
    ylim <- grDevices::extendrange(unlist(lapply(delta_lst, function(x) unlist(x)), use.names = FALSE), f = 0.25)
  }

  ## Labeling
  if (isTRUE(names)) {
    names <- lapply(delta_lst, names)
  } else if (is.character(names)) {
    names <- split(rep(names, length(node)), f = rep(1:length(node), each = length(names)))
  } else {
    ncf <- lapply(delta_lst, length)
    names <- lapply(ncf, function(m) {
      lab <- rep("", m)
      lab[c(1, m)] <- c(1, m)
      pr <- pretty(1:m, n = 4)
      pr <- pr[pr > 1 & pr < m]
      lab[pr] <- pr
      lab
    })
    abbreviate <- FALSE
  }

  ## Abbreviation
  if (is.logical(abbreviate)) {
    nlab <- max(unlist(lapply(names, function(j) nchar(j))))
    abbreviate <- if (abbreviate) as.numeric(cut(nlab, c(-Inf, 1.5, 4.5, 7.5, Inf))) else nlab
  }
  names <- lapply(names, function(j) abbreviate(j, abbreviate))

  ## Label for extraction
  names(names) <- names(delta_lst) <- node

  ## Panel function
  panelfun <- function(node) {
    ## Select node ID, coefficients, identified items, x-position vector, and label
    id <- as.character(partykit::id_node(node))
    delta_unsorted <- delta_lst[[id]]
    lab <- paste("node", id, sep = "")
    namesi <- names[[id]]

    ## Sort thresholds for display
    delta_sorted <- lapply(delta_unsorted, sort)



    ## Original panel setup (your preferred layout)
    top.vp <- grid::viewport(layout = grid::grid.layout(nrow = 2, ncol = 1,
                                            widths = unit(1, "null"),
                                            heights = unit(c(1, 1), c("lines", "null"))),
                       width = unit(0.9, "npc"),
                       height = unit(1, "npc") - unit(3, "lines"),
                       name = paste(lab, "_effects", sep = ""))
    grid::pushViewport(top.vp)
    grid::grid.rect(gp = gpar(fill = bg, col = 0), name = paste(lab, "_border", sep = ""))

    ## main title
    grid::pushViewport(viewport(layout.pos.col = 1, layout.pos.row = 1, name = paste(lab, "_title_vp", sep = "")))
    grid::grid.text(paste("Node ", id, " (n = ", partykit::info_node(node)$nobs, ")", sep = ""), name = paste(lab, "_title", sep = ""))
    grid::upViewport()

    ## Plot area
    grid::pushViewport(viewport(layout.pos.col = 1, layout.pos.row = 2, name = lab))
    lab <- paste(lab, "_plot", sep = "")

    ## Setup plotting area (3x3 grid)
    wcol <- c(ylines, 1, 1)
    hrow <- c(0.5, 1, 1)

    top.vp <- grid::viewport(layout = grid.layout(nrow = 3, ncol = 3, widths = unit(wcol, c("lines", "null", "lines")), heights = unit(hrow, c("lines", "null", "lines"))), name = paste(lab, "_top_vp", sep = ""))
    bmargin.vp <- grid::viewport(layout.pos.row = 3, layout.pos.col = 2, name = paste(lab, "_bottom-margin_vp", sep = ""))
    lmargin.vp <- grid::viewport(layout.pos.row = 2, layout.pos.col = 1, name = paste(lab, "_left-margin_vp", sep = ""))
    rmargin.vp <- grid::viewport(layout.pos.row = 2, layout.pos.col = 3, name = paste(lab, "_right-margin_vp", sep = ""))
    plot.vp <- grid::viewport(layout.pos.row = 2, layout.pos.col = 2, name = paste(lab, "_vp", sep = ""), xscale = xlim, yscale = ylim)
    grid::pushViewport(top.vp)
    grid::pushViewport(plot.vp)

    ## plot rectangles per item
    for (j in seq_along(delta_sorted)) {
      ncat <- length(delta_sorted[[j]]) + 1
      grid::grid.rect(x = rep.int(xi[j], ncat), y = c(ylim[1], delta_sorted[[j]]), width = rep.int(1, ncat),
                height = diff.default(c(ylim[1], delta_sorted[[j]], ylim[2])), just = c("left", "bottom"),
                gp = grid::gpar(fill = col_fun(ncat)), default.units = "native", name = paste(lab, "_item", j, "_rect", sep = ""))
    }



    ## add box and axis
    grid::grid.rect(name = paste(lab, "_plot-box", sep = ""), gp = gpar(fill = NA))
    grid::grid.xaxis(at = (xi[-(m+1)] + 0.5), label = namesi, main = TRUE, name = paste(lab, "_xaxis-bottom", sep = ""))
    grid::grid.yaxis(main = TRUE, name = paste(lab, "_yaxis-left", sep = ""))
    grid::upViewport()

    ## add descriptions
    grid::pushViewport(lmargin.vp)
    grid::upViewport(2)

    ## go back to uper vp
    grid::upViewport(2)
  }

  ## Return panel function
  return(panelfun)
}
class(node_regionplot_grmtree) <- "grapcon_generator"


## Profile plot
node_profileplot_grmtree <- function(mobobj, what = c("item", "threshold", "discrimination"),
                                     parg = list(type = NULL, ref = NULL, alias = TRUE, logit = FALSE),
                                     id = TRUE, names = FALSE, abbreviate = TRUE, index = TRUE,
                                     ref = TRUE, col = "black", border = col, linecol = "black",
                                     refcol = "lightgray", bg = "white", cex = 0.5, pch = 21,
                                     xscale = NULL, yscale = NULL, ylines = 2, ...) {
  ## Check input
  what <- match.arg(what)
  refpar <- parg$ref
  alias <- if (is.null(parg$alias)) TRUE else parg$alias
  addargs <- list(...)
  logit <- parg$logit

  ## Node IDs (terminal nodes only)
  node <- partykit::nodeids(mobobj, terminal = TRUE)

  ## Get all coefficients (item, thresholds, or discrimination)
  cf <- lapply(node, function(n) {
    model <- partykit::nodeapply(mobobj, ids = n, FUN = function(nd) nd$info$object)[[1]]
    coef_model <- mirt::coef(model, IRTpars=T, simplify=TRUE)
    if (what == "threshold") {
      coef_model$items[, grep("b", colnames(coef_model$items))]  # Thresholds
    } else if (what == "discrimination") {
      coef_model$items[, "a"]  # Discrimination (use "a" instead of "a1")
    } else if (what == "item") {
      # Compute item parameters (discrimination and average thresholds)
      thresholds <- coef_model$items[, grep("b", colnames(coef_model$items))]
      avg_thresholds <- rowMeans(thresholds, na.rm = TRUE)
      item_params <- cbind(coef_model$items[, "a"], avg_thresholds)  # Discrimination and average thresholds
      colnames(item_params) <- c("Discrimination", "AvgThreshold")
      item_params
    }
  })
  names(cf) <- as.character(node)  # Ensure names are character

  ## Labeling
  if (isTRUE(names)) {
    nms <- lapply(cf, rownames)
  } else if (is.character(names)) {
    nms <- split(rep(names, length(node)), f = rep(1:length(node), each = length(names)))
  } else {
    ncf <- lapply(cf, NROW)
    nms <- lapply(ncf, function(m) {
      lab <- rep("", m)
      lab[c(1, m)] <- c(1, m)
      pr <- pretty(1:m, n = 4)
      pr <- pr[pr > 1 & pr < m]
      lab[pr] <- pr
      lab
    })
    abbreviate <- FALSE
  }

  ## Abbreviation
  if (is.logical(abbreviate)) {
    nlab <- max(unlist(lapply(nms, function(j) nchar(j))))
    abbreviate <- if (abbreviate) as.numeric(cut(nlab, c(-Inf, 1.5, 4.5, 7.5, Inf))) else nlab
  }
  nms <- lapply(nms, function(j) abbreviate(j, abbreviate))

  ## Axis scale
  if (is.null(xscale)) xscale <- c(0.5, max(sapply(cf, NROW)) + 0.5)  # Add padding for first/last item
  rg <- range(unlist(cf)[is.finite(unlist(cf))], na.rm = TRUE)
  r <- diff(rg)
  if (!r) r <- 1
  if (is.null(yscale)) yscale <- rg + c(-0.1, 0.1) * r

  ## Panel function for profile plots in nodes
  panelfun <- function(node) {
    ## Node index (ensure it's a terminal node)
    idn <- as.character(partykit::id_node(node))
    if (!idn %in% names(cf)) {
      stop("Node ID ", idn, " not found in the coefficients list. Ensure terminal nodes are used.")
    }

    ## Get coefficients and labels
    cfi <- cf[[idn]]
    nmsi <- nms[[idn]]

    ## Viewport setup
    top_vp <- grid::viewport(layout = grid::grid.layout(nrow = 2, ncol = 3,
                                            widths = unit(c(ylines, 1, 2), c("lines", "null", "lines")),
                                            heights = unit(c(1, 1), c("lines", "null"))),
                       width = unit(0.9, "npc"), height = unit(1, "npc") - unit(3, "lines"),
                       name = paste("node_profileplot", idn, sep = ""))
    grid::pushViewport(top_vp)
    grid::grid.rect(gp = gpar(fill = bg, col = 0))

    ## Main title
    top <- viewport(layout.pos.col = 2, layout.pos.row = 1)
    grid::pushViewport(top)
    mainlab <- paste(ifelse(id, paste("Node", idn, "(n = "), ""),
                     partykit::info_node(node)$nobs, ifelse(id, ")", ""), sep = "")
    grid::grid.text(mainlab)
    grid::popViewport()

    ## Actual plot
    plot_vpi <- viewport(layout.pos.col = 2, layout.pos.row = 2, xscale = xscale, yscale = yscale,
                         name = paste("node_profileplot", idn, "plot", sep = ""))
    grid::pushViewport(plot_vpi)

    ## Plot thresholds, discrimination, or item parameters
    if (what == "threshold") {
      for (j in 1:ncol(cfi)) {
        grid::grid.lines(1:nrow(cfi), cfi[, j], gp = gpar(col = linecol, lty = j), default.units = "native")
      }
    } else if (what == "discrimination") {
      grid::grid.lines(1:length(cfi), cfi, gp = gpar(col = linecol), default.units = "native")
      grid::grid.points(1:length(cfi), cfi, gp = gpar(col = border, fill = col, cex = cex), pch = pch, default.units = "native")
    } else if (what == "item") {
      # Plot discrimination and average thresholds
      grid::grid.lines(1:nrow(cfi), cfi[, "Discrimination"], gp = gpar(col = linecol, lty = 1), default.units = "native")
      grid::grid.lines(1:nrow(cfi), cfi[, "AvgThreshold"], gp = gpar(col = linecol, lty = 2), default.units = "native")
      grid::grid.points(1:nrow(cfi), cfi[, "Discrimination"], gp = gpar(col = border, fill = col, cex = cex), pch = pch, default.units = "native")
      grid::grid.points(1:nrow(cfi), cfi[, "AvgThreshold"], gp = gpar(col = border, fill = col, cex = cex), pch = pch + 1, default.units = "native")

      ## Add a compact legend in the top-right corner with line symbols and vertical spacing
      legend_vp <- grid::viewport(x = unit(0.95, "npc"), y = unit(1.0, "npc"),
                            width = unit(0.33, "npc"), height = unit(0.08, "npc"),  # Increased height for spacing
                            just = c("right", "top"))
      grid::pushViewport(legend_vp)
      grid::grid.rect(gp = gpar(fill = "white", col = "black", alpha = 0.8))  # Semi-transparent background

      # Add line symbols and labels with vertical spacing
      grid::grid.lines(x = unit(c(0.1, 0.3), "npc"), y = unit(0.75, "npc"),
                 gp = gpar(col = linecol, lty = 1), default.units = "npc")
      grid::grid.text("Discrim", x = unit(0.35, "npc"), y = unit(0.75, "npc"),
                just = "left", gp = gpar(col = linecol, fontface = "bold"))

      grid::grid.lines(x = unit(c(0.1, 0.3), "npc"), y = unit(0.3, "npc"),
                 gp = gpar(col = linecol, lty = 2), default.units = "npc")
      grid::grid.text("Threshold", x = unit(0.35, "npc"), y = unit(0.3, "npc"),
                just = "left", gp = gpar(col = linecol, fontface = "bold"))

      grid::popViewport()
    }

    ## Add axes
    grid::grid.xaxis(at = 1:length(nmsi), label = nmsi)
    grid::grid.yaxis(at = c(ceiling(yscale[1] * 100)/100, floor(yscale[2] * 100)/100))  # Improved y-axis
    grid::grid.rect(gp = gpar(fill = "transparent"))

    grid::upViewport(2)
  }

  return(panelfun)
}
class(node_profileplot_grmtree) <- "grapcon_generator"




