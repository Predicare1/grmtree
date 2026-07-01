################################################################################
##
## Plot Methods for Longitudinal GRMTree
##
## Custom plotting functions for longitudinal_grmtree objects.
## The key difference from grmtree plots: the constrained longitudinal model
## has 2*n_items columns but item parameters are equal across T1 and T2,
## so we only display the first n_items (T1) to avoid redundancy.
##
## Author: Olayinka Arimoro
## Date: March 2026
##
################################################################################

#' Plot Method for Longitudinal GRM Tree Objects
#'
#' Visualizes a longitudinal GRM tree with threshold region plots in terminal
#' nodes. Unlike the cross-sectional \code{\link[grmtree]{plot.grmtree}},
#' this method displays only the unique item parameters (T1 items), since
#' the constrained longitudinal model enforces equal parameters across T1
#' and T2 within each node.
#'
#' @param x A \code{longitudinal_grmtree} object.
#' @param type Type of terminal node plot. Currently only \code{"regions"}
#'   is supported for the longitudinal model.
#' @param tnex Numeric scaling factor for terminal node extension (default: 2).
#' @param drop_terminal Logical indicating whether to drop terminal node IDs
#'   (default: TRUE).
#' @param names Logical or character vector. If \code{TRUE}, use item names
#'   from the response matrix. If a character vector, use as custom labels.
#'   If \code{FALSE} (default), use numeric indices 1 through n_items.
#' @param abbreviate Logical or numeric. If \code{TRUE}, abbreviate item
#'   names. If numeric, abbreviate to that many characters.
#' @param ... Additional arguments passed to the terminal panel function.
#'
#' @return Invisibly returns the tree object. Called for its side effect of
#'   producing a plot.
#'
#' @details
#' The region plot displays threshold parameters as colored horizontal bands
#' for each item within each terminal node. Darker shading represents lower
#' response categories and lighter shading represents higher categories.
#' The height of each band corresponds to the range of the latent trait
#' over which that response category is most likely.
#'
#' Because the constrained longitudinal GRM enforces
#' \eqn{a_{m,T1} = a_{m,T2}} and \eqn{b_{k,m,T1} = b_{k,m,T2}}, the T1
#' and T2 item parameters are identical. The plot therefore shows only the
#' \code{n_items} unique items rather than all \code{2 * n_items} columns
#' in the response matrix.
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
#'   # Phase 1: fit the longitudinal GRM tree
#'   ltree <- longitudinal_grmtree(
#'     resp_wide ~ sex + age + residency + job +
#'       education + comorbidity_count + ever_smoker,
#'     data = ld, n_items = 8,
#'     control = grmtree.control(minbucket = 200)
#'   )
#'
#'   # Region plot with numeric labels
#'   plot(ltree)
#'
#'   # Region plot with item names
#'   plot(ltree, names = TRUE)
#'
#'   # Custom labels
#'   plot(ltree, names = c("Listen", "Info", "Crisis",
#'     "Confide", "Advice", "Fears", "Personal", "Understand"))
#'
#' @seealso \code{\link{longitudinal_grmtree}} for fitting the tree,
#'   \code{\link[grmtree]{plot.grmtree}} for cross-sectional tree plots
#'
#' @method plot longitudinal_grmtree
#' @import grid
#' @export
plot.longitudinal_grmtree <- function(x,
                                      type = "regions",
                                      tnex = 2L,
                                      drop_terminal = TRUE,
                                      names = FALSE,
                                      abbreviate = TRUE,
                                      ...) {

  if (!inherits(x, "longitudinal_grmtree")) {
    stop("The input object must be of class 'longitudinal_grmtree'.")
  }

  type <- match.arg(type, choices = c("regions"))

  n_items <- x$info$n_items
  if (is.null(n_items)) {
    stop("Tree object does not contain n_items. Was it fit with longitudinal_grmtree()?")
  }

  # Build the terminal panel
  terminal_panel <- node_regionplot_longitudinal(
    x, n_items = n_items, names = names, abbreviate = abbreviate, ...
  )

  # Use partykit's plot infrastructure
  partykit::plot.modelparty(
    x,
    terminal_panel = terminal_panel,
    tnex = tnex,
    drop_terminal = drop_terminal,
    ...
  )

  invisible(x)
}


#' Region Plot Panel for Longitudinal GRM Tree Nodes
#'
#' Internal function that generates the terminal node panel function for
#' region plots of longitudinal GRM trees. Extracts only T1 item thresholds
#' (items 1 through n_items) since T2 items have identical constrained
#' parameters.
#'
#' @param mobobj A \code{longitudinal_grmtree} object.
#' @param n_items Number of items per time point.
#' @param names Logical, character vector, or FALSE for labeling.
#' @param abbreviate Logical or numeric for name abbreviation.
#' @param ylim Optional y-axis limits.
#' @param off Offset between items (default: 0.1).
#' @param col_fun Color function for response categories (default:
#'   \code{gray.colors}).
#' @param bg Background color (default: "white").
#' @param ylines Width of y-axis margin in lines (default: 2).
#' @param ... Additional arguments (unused).
#'
#' @return A panel function suitable for use with
#'   \code{\link[partykit]{plot.modelparty}}.
#'
#' @keywords internal
node_regionplot_longitudinal <- function(mobobj, n_items, names = FALSE,
                                         abbreviate = TRUE,
                                         ylim = NULL, off = 0.1,
                                         col_fun = grDevices::gray.colors,
                                         bg = "white", ylines = 2, ...) {

  stopifnot(!is.null(mobobj))
  stopifnot(off >= 0)

  # ---- Extract T1-only thresholds for each terminal node ----
  node_ids <- partykit::nodeids(mobobj, terminal = TRUE)

  delta_lst <- lapply(node_ids, function(n) {
    model <- partykit::nodeapply(
      mobobj, ids = n, FUN = function(nd) nd$info$object
    )[[1]]

    coef_model <- mirt::coef(model, IRTpars = TRUE, simplify = TRUE)
    all_items <- coef_model$items
    thresh_cols <- grep("^b", colnames(all_items))

    # Take only T1 items (rows 1:n_items)
    thresh <- all_items[1:n_items, thresh_cols, drop = FALSE]

    # Convert to list of vectors (one per item)
    thresh_list <- split(thresh, seq_len(nrow(thresh)))

    # Get clean item names from T1 rows
    raw_names <- rownames(all_items)[1:n_items]
    names(thresh_list) <- raw_names

    return(thresh_list)
  })
  names(delta_lst) <- as.character(node_ids)

  # ---- Setup x-axis parameters ----
  m <- n_items
  xi <- 0:m + c(0:(m - 1), m - 1) * off
  xlim <- c(xi[1], xi[m + 1])

  # ---- Setup y-axis range ----
  if (is.null(ylim)) {
    all_vals <- unlist(lapply(delta_lst, function(x) unlist(x)), use.names = FALSE)
    ylim <- grDevices::extendrange(all_vals[is.finite(all_vals)], f = 0.25)
  }

  # ---- Setup labels ----
  if (isTRUE(names)) {
    # Use item names from the model, cleaned up
    lab_list <- lapply(delta_lst, function(dl) {
      raw <- names(dl)
      # Remove common prefixes like "resp_wideMOS_" or "mos_resp_wideMOS_"
      cleaned <- gsub("^.*MOS_", "", raw)
      cleaned <- gsub("_BL$|_Pre$|_T1$", "", cleaned)
      cleaned
    })
  } else if (is.character(names)) {
    # User-provided labels
    lab_list <- lapply(node_ids, function(n) names)
    names(lab_list) <- as.character(node_ids)
  } else {
    # Numeric labels
    lab_list <- lapply(node_ids, function(n) {
      lab <- rep("", m)
      lab[c(1, m)] <- c(1, m)
      pr <- pretty(1:m, n = 4)
      pr <- pr[pr > 1 & pr < m]
      lab[pr] <- pr
      lab
    })
    names(lab_list) <- as.character(node_ids)
    abbreviate <- FALSE
  }

  # ---- Abbreviate if needed ----
  if (is.logical(abbreviate) && abbreviate) {
    nlab <- max(unlist(lapply(lab_list, nchar)))
    abbr_len <- as.numeric(cut(nlab, c(-Inf, 1.5, 4.5, 7.5, Inf)))
    lab_list <- lapply(lab_list, function(j) abbreviate(j, abbr_len))
  } else if (is.numeric(abbreviate)) {
    lab_list <- lapply(lab_list, function(j) abbreviate(j, abbreviate))
  }

  # ---- Panel function ----
  panelfun <- function(node) {
    id <- as.character(partykit::id_node(node))
    delta_unsorted <- delta_lst[[id]]
    namesi <- lab_list[[id]]
    lab <- paste("node", id, sep = "")

    # Sort thresholds within each item for display
    delta_sorted <- lapply(delta_unsorted, sort)

    # ---- Viewport setup ----
    top.vp <- grid::viewport(
      layout = grid::grid.layout(
        nrow = 2, ncol = 1,
        widths = grid::unit(1, "null"),
        heights = grid::unit(c(1, 1), c("lines", "null"))
      ),
      width = grid::unit(0.9, "npc"),
      height = grid::unit(1, "npc") - grid::unit(3, "lines"),
      name = paste(lab, "_effects", sep = "")
    )
    grid::pushViewport(top.vp)
    grid::grid.rect(gp = grid::gpar(fill = bg, col = 0),
                    name = paste(lab, "_border", sep = ""))

    # ---- Title ----
    grid::pushViewport(grid::viewport(
      layout.pos.col = 1, layout.pos.row = 1,
      name = paste(lab, "_title_vp", sep = "")
    ))
    grid::grid.text(
      paste("Node ", id, " (n = ", partykit::info_node(node)$nobs, ")", sep = ""),
      name = paste(lab, "_title", sep = "")
    )
    grid::upViewport()

    # ---- Plot area ----
    grid::pushViewport(grid::viewport(
      layout.pos.col = 1, layout.pos.row = 2,
      name = lab
    ))
    lab <- paste(lab, "_plot", sep = "")

    wcol <- c(ylines, 1, 1)
    hrow <- c(0.5, 1, 1)

    top.vp2 <- grid::viewport(
      layout = grid::grid.layout(
        nrow = 3, ncol = 3,
        widths = grid::unit(wcol, c("lines", "null", "lines")),
        heights = grid::unit(hrow, c("lines", "null", "lines"))
      ),
      name = paste(lab, "_top_vp", sep = "")
    )
    plot.vp <- grid::viewport(
      layout.pos.row = 2, layout.pos.col = 2,
      name = paste(lab, "_vp", sep = ""),
      xscale = xlim, yscale = ylim
    )
    grid::pushViewport(top.vp2)
    grid::pushViewport(plot.vp)

    # ---- Draw region rectangles ----
    for (j in seq_along(delta_sorted)) {
      ncat <- length(delta_sorted[[j]]) + 1
      grid::grid.rect(
        x = rep.int(xi[j], ncat),
        y = c(ylim[1], delta_sorted[[j]]),
        width = rep.int(1, ncat),
        height = diff.default(c(ylim[1], delta_sorted[[j]], ylim[2])),
        just = c("left", "bottom"),
        gp = grid::gpar(fill = col_fun(ncat)),
        default.units = "native",
        name = paste(lab, "_item", j, "_rect", sep = "")
      )
    }

    # ---- Axes and box ----
    grid::grid.rect(name = paste(lab, "_plot-box", sep = ""),
                    gp = grid::gpar(fill = NA))
    grid::grid.xaxis(
      at = (xi[-(m + 1)] + 0.5),
      label = namesi,
      main = TRUE,
      name = paste(lab, "_xaxis-bottom", sep = "")
    )
    grid::grid.yaxis(main = TRUE, name = paste(lab, "_yaxis-left", sep = ""))
    grid::upViewport()

    # ---- Clean up viewports ----
    grid::pushViewport(grid::viewport(
      layout.pos.row = 2, layout.pos.col = 1,
      name = paste(lab, "_left-margin_vp", sep = "")
    ))
    grid::upViewport(2)
    grid::upViewport(2)
  }

  return(panelfun)
}
class(node_regionplot_longitudinal) <- "grapcon_generator"
