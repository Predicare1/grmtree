################################################################################
##
## Response Shift Visualization Functions
##
## Plot 1: plot_rs_tree() — Annotated tree with RS summary panels
## Plot 2: plot_rs_heatmap() — Item-level RS heatmap across nodes
##
## Author: Olayinka Arimoro
## Date: March 2026
##
################################################################################


# ---- Helper: format p-value for display ----
.format_p <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 0.001) return("p < 0.001")
  if (p < 0.01) return(paste0("p = ", formatC(p, format = "f", digits = 3)))
  if (p < 0.1) return(paste0("p = ", formatC(p, format = "f", digits = 3)))
  return(paste0("p = ", formatC(p, format = "f", digits = 2)))
}

# ---- Standardize RS type: keep only the 3 real types + None ----
.clean_rs_type <- function(rs_type) {
  ifelse(rs_type %in% c("Recalibration", "Reprioritization", "Both"),
         rs_type, "None")
}

# ---- Color mapping (4 types only) ----
.rs_colors <- c(
  "Recalibration"    = "#3182BD",   # blue
  "Reprioritization" = "#E6550D",   # orange-red
  "Both"             = "#756BB1",   # purple
  "None"             = "#D9D9D9"    # light gray
)


#' Plot Response Shift Summary Tree
#'
#' Displays the Longitudinal GRMTree structure with RS characterization results
#' annotated in each terminal node panel. Each panel shows the omnibus test
#' result, latent trait parameters, and an item-level RS heatmap color-coded by
#' RS type.
#'
#' @param tree A \code{longitudinal_grmtree} object.
#' @param rs An \code{rs_characterization} object from
#'   \code{\link{rs_characterize}}.
#' @param item_labels Optional character vector of short item labels. If NULL,
#'   uses "Item 1", "Item 2", etc.
#' @param tnex Numeric scaling factor for terminal node panels (default: 2.5).
#' @param drop_terminal Logical (default: TRUE).
#' @param ... Additional arguments passed to \code{plot.modelparty}.
#'
#' @return Invisibly returns the tree object.
#'
#' @details Each terminal node panel contains:
#' \itemize{
#'   \item Omnibus RS result: chi-squared, adjusted p-value, and detection
#'     status
#'   \item Latent parameters: mean shift at T2 and test-retest correlation (mu_T2 and cor(T1,T2)
#'   \item Item-level RS bar: colored cells for each item indicating RS type
#'     (blue = recalibration, orange = reprioritization,
#'     purple = both, gray = none)
#' }
#'
#' @examplesIf interactive() library(grmtree)
#'
#'   # Load the synthetic longitudinal data data("grmtree_long_data", package =
#'   "grmtree")
#'
#'   # Prepare the wide-format response matrix items_t1 <- c("MOS_Listen",
#'   "MOS_Info", "MOS_Advice_Crisis", "MOS_Confide", "MOS_Advice_Want",
#'   "MOS_Fears", "MOS_Personal", "MOS_Understand") ld <-
#'   prepare_longitudinal_data( data = grmtree_long_data, items_t1 = items_t1,
#'   items_t2 = paste0(items_t1, "_year1"), covariates = c("sex", "age",
#'   "residency", "job", "education", "comorbidity_count", "ever_smoker") )
#'
#'   # Phase 1: fit the longitudinal GRM tree ltree <- longitudinal_grmtree(
#'   resp_wide ~ sex + age + residency + job + education + comorbidity_count +
#'   ever_smoker, data = ld, n_items = 8, control = grmtree.control(minbucket =
#'   200) )
#'
#'   # Phase 2: characterize response shift within each subgroup rs <-
#'   rs_characterize(ltree, p_adjust = "fdr", global_p_adjust = "bonferroni")
#'
#'   plot_rs_tree(ltree, rs, item_labels = c("Listen", "Info", "Crisis",
#'   "Confide", "Advice", "Fears", "Personal", "Understand"))
#'
#' @export
#' @import grid
plot_rs_tree <- function(tree, rs, item_labels = NULL, tnex = 2.5,
                         drop_terminal = TRUE, ...) {

  if (!inherits(tree, "longitudinal_grmtree")) {
    stop("'tree' must be a longitudinal_grmtree object")
  }
  if (!inherits(rs, "rs_characterization")) {
    stop("'rs' must be an rs_characterization object from rs_characterize()")
  }

  n_items <- rs$n_items
  if (is.null(item_labels)) {
    item_labels <- paste0("I", 1:n_items)
  }
  if (length(item_labels) != n_items) {
    stop("item_labels must have length ", n_items)
  }

  # Build panel function
  panelfun <- function(node) {
    id <- as.character(partykit::id_node(node))
    nobs <- partykit::info_node(node)$nobs
    g <- rs$global

    # Find this node's row in global results
    node_row <- which(g$Node == as.integer(id))

    if (length(node_row) == 0) return()

    node_g <- g[node_row, ]
    has_adj <- "LRT_p_adj" %in% names(g)

    # Get item-level results for this node
    if (!is.null(rs$item_level)) {
      node_items <- rs$item_level[rs$item_level$Node == as.integer(id), ]
    } else {
      node_items <- NULL
    }

    # ---- Setup viewport ----
    lab <- paste0("rs_node_", id)
    top.vp <- grid::viewport(
      layout = grid::grid.layout(nrow = 4, ncol = 1,
                                 heights = grid::unit(c(1.2, 1, 1, 1.5), c("lines", "lines", "lines", "null"))
      ),
      width = grid::unit(0.95, "npc"),
      height = grid::unit(1, "npc") - grid::unit(2, "lines"),
      name = paste0(lab, "_top")
    )
    grid::pushViewport(top.vp)
    grid::grid.rect(gp = grid::gpar(fill = "white", col = "gray80"))

    # ---- Row 1: Node title ----
    grid::pushViewport(grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
    grid::grid.text(
      paste0("Node ", id, " (n = ", nobs, ")"),
      gp = grid::gpar(fontface = "bold", cex = 0.85)
    )
    grid::upViewport()

    # ---- Row 2: Omnibus result ----
    grid::pushViewport(grid::viewport(layout.pos.row = 2, layout.pos.col = 1))

    p_raw <- if (has_adj && !is.na(node_g$LRT_p_adj)) {
      node_g$LRT_p_adj
    } else {
      node_g$LRT_p
    }
    p_display <- .format_p(p_raw)

    rs_detected <- isTRUE(node_g$RS_detected)
    rs_label <- if (rs_detected) "Response shift detected" else "No response shift"
    rs_col <- if (rs_detected) "#C0392B" else "#27AE60"

    grid::grid.text(
      paste0(rs_label, "  (", p_display, ")"),
      gp = grid::gpar(cex = 0.65, col = rs_col, fontface = "bold")
    )
    grid::upViewport()

    # ---- Row 3: Latent parameters ----
    grid::pushViewport(grid::viewport(layout.pos.row = 3, layout.pos.col = 1))
    grid::grid.text(
      paste0("Mean shift (T2): ", round(node_g$mu_T2, 2),
             "    Cor(T1, T2): ", round(node_g$cor_T1_T2, 2)),
      gp = grid::gpar(cex = 0.6, col = "gray30")
    )
    grid::upViewport()

    # ---- Row 4: Item RS heatmap bar ----
    grid::pushViewport(grid::viewport(layout.pos.row = 4, layout.pos.col = 1))

    # Determine RS type per item
    item_types <- rep("None", n_items)
    if (!is.null(node_items) && nrow(node_items) > 0) {
      has_p_adj <- "LRT_p_adj" %in% names(node_items)
      for (i in 1:nrow(node_items)) {
        item_idx <- node_items$Item[i]
        p_val <- if (has_p_adj) node_items$LRT_p_adj[i] else node_items$LRT_p[i]
        if (!is.na(p_val) && p_val < rs$alpha) {
          item_types[item_idx] <- .clean_rs_type(node_items$RS_type[i])
        }
      }
    }

    # Draw colored rectangles for each item
    cell_width <- 1 / n_items
    for (i in 1:n_items) {
      fill <- .rs_colors[item_types[i]]
      if (is.na(fill)) fill <- "#D9D9D9"

      grid::grid.rect(
        x = grid::unit((i - 0.5) * cell_width, "npc"),
        y = grid::unit(0.55, "npc"),
        width = grid::unit(cell_width * 0.85, "npc"),
        height = grid::unit(0.55, "npc"),
        gp = grid::gpar(fill = fill, col = "white", lwd = 0.5)
      )

      # Item label below
      grid::grid.text(
        item_labels[i],
        x = grid::unit((i - 0.5) * cell_width, "npc"),
        y = grid::unit(0.08, "npc"),
        gp = grid::gpar(cex = 0.45)
      )
    }

    grid::upViewport()  # row 4
    grid::upViewport()  # top
  }

  # Plot using partykit infrastructure
  partykit::plot.modelparty(
    tree,
    terminal_panel = panelfun,
    tnex = tnex,
    drop_terminal = drop_terminal,
    ...
  )

  # ---- Legend at bottom with colored squares ----
  legend_types <- c("Recalibration", "Reprioritization", "Both", "None")
  legend_cols  <- .rs_colors[legend_types]
  n_legend <- length(legend_types)

  legend_total_width <- 0.6
  legend_start_x <- 0.5 - legend_total_width / 2
  legend_spacing <- legend_total_width / n_legend
  legend_y <- 0.025

  for (k in seq_along(legend_types)) {
    x_pos <- legend_start_x + (k - 1) * legend_spacing

    # Colored square
    grid::grid.rect(
      x = grid::unit(x_pos, "npc"),
      y = grid::unit(legend_y, "npc"),
      width = grid::unit(0.018, "npc"),
      height = grid::unit(0.018, "npc"),
      gp = grid::gpar(fill = legend_cols[k], col = "gray50", lwd = 0.5)
    )

    # Label next to square
    grid::grid.text(
      legend_types[k],
      x = grid::unit(x_pos + 0.015, "npc"),
      y = grid::unit(legend_y, "npc"),
      just = "left",
      gp = grid::gpar(cex = 0.55, col = "gray30")
    )
  }

  invisible(tree)
}


#' Plot Item-Level Response Shift Heatmap
#'
#' Creates a standalone heatmap showing RS type for each item across all
#' terminal nodes. Significant items are filled with RS-type colors;
#' non-significant items are white/light gray.
#'
#' @param rs An \code{rs_characterization} object from
#'   \code{\link{rs_characterize}}.
#' @param item_labels Optional character vector of item labels.
#'   If NULL, uses "Item 1", "Item 2", etc.
#' @param node_labels Optional character vector of node labels.
#'   If NULL, uses "Node X (n=Y)" from the global results.
#' @param show_chi2 Logical. If TRUE, display chi-squared values inside
#'   cells. Default is FALSE.
#' @param title Optional plot title. If NULL, uses a default title.
#' @param ... Additional arguments (unused).
#'
#' @return Invisibly returns the rs object. Called for its side effect of
#'   producing a plot.
#'
#' @details
#' The heatmap uses the following color coding:
#' \describe{
#'   \item{Blue}{Recalibration (threshold change)}
#'   \item{Red/Orange}{Reprioritization (discrimination change)}
#'   \item{Purple}{Both recalibration and reprioritization}
#'   \item{Light orange}{Significant but small effect}
#'   \item{White}{Not significant or not tested}
#' }
#'
#' Cells with significant RS (after p-value adjustment) are marked with
#' an asterisk (*). The omnibus RS result for each node is displayed at
#' the top of each column.
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
#'   # Phase 2: characterize response shift within each subgroup
#'   rs <- rs_characterize(ltree, p_adjust = "fdr",
#'     global_p_adjust = "bonferroni")
#'
#'   # Basic heatmap
#'   plot_rs_heatmap(rs)
#'
#'   # With custom labels
#'   plot_rs_heatmap(rs,
#'     item_labels = c("Listen", "Info", "Crisis", "Confide",
#'       "Advice", "Fears", "Personal", "Understand"))
#'
#'   # Show chi-squared values
#'   plot_rs_heatmap(rs, show_chi2 = TRUE)
#'
#' @export
plot_rs_heatmap <- function(rs, item_labels = NULL, node_labels = NULL,
                            show_chi2 = FALSE, title = NULL, ...) {

  if (!inherits(rs, "rs_characterization")) {
    stop("'rs' must be an rs_characterization object from rs_characterize()")
  }

  n_items <- rs$n_items
  g <- rs$global
  nodes <- g$Node
  n_nodes <- length(nodes)

  if (is.null(item_labels)) {
    item_labels <- paste("Item", 1:n_items)
  }
  if (is.null(node_labels)) {
    node_labels <- paste0("Node ", g$Node, "\n(n=", g$n, ")")
  }
  if (is.null(title)) {
    title <- "Item-Level Response Shift Characterization"
  }

  # Build the matrix of RS types and chi2 values
  rs_matrix <- matrix("Not tested", nrow = n_items, ncol = n_nodes)
  chi2_matrix <- matrix(NA, nrow = n_items, ncol = n_nodes)
  sig_matrix <- matrix(FALSE, nrow = n_items, ncol = n_nodes)

  if (!is.null(rs$item_level)) {
    has_p_adj <- "LRT_p_adj" %in% names(rs$item_level)

    for (i in 1:nrow(rs$item_level)) {
      row <- rs$item_level[i, ]
      node_col <- which(nodes == row$Node)
      item_row <- row$Item

      if (length(node_col) > 0 && item_row >= 1 && item_row <= n_items) {
        rs_matrix[item_row, node_col] <- .clean_rs_type(row$RS_type)
        chi2_matrix[item_row, node_col] <- row$LRT_chi2

        p_val <- if (has_p_adj) row$LRT_p_adj else row$LRT_p
        sig_matrix[item_row, node_col] <- !is.na(p_val) && p_val < rs$alpha
      }
    }
  }

  # Mark nodes where omnibus was not significant as "No RS"
  for (j in 1:n_nodes) {
    if (!isTRUE(g$RS_detected[j])) {
      rs_matrix[, j] <- "None"
    }
  }

  # ---- Plot setup ----
  # Margins: left for item labels, top for node labels + omnibus, right for legend
  left_margin <- 0.22
  right_margin <- 0.18
  top_margin <- 0.15
  bottom_margin <- 0.06
  plot_width <- 1 - left_margin - right_margin
  plot_height <- 1 - top_margin - bottom_margin

  cell_w <- plot_width / n_nodes
  cell_h <- plot_height / n_items

  # Start new page
  grid::grid.newpage()

  # Title
  grid::grid.text(
    title,
    x = grid::unit(0.5, "npc"),
    y = grid::unit(0.97, "npc"),
    gp = grid::gpar(fontface = "bold", cex = 1.1)
  )

  # ---- Draw omnibus results at top ----
  for (j in 1:n_nodes) {
    x_center <- left_margin + (j - 0.5) * cell_w

    # Node label
    grid::grid.text(
      node_labels[j],
      x = grid::unit(x_center, "npc"),
      y = grid::unit(1 - top_margin + 0.06, "npc"),
      gp = grid::gpar(cex = 0.7, fontface = "bold")
    )

    # Omnibus result
    has_adj <- "LRT_p_adj" %in% names(g)
    p_val <- if (has_adj && !is.na(g$LRT_p_adj[j])) g$LRT_p_adj[j] else g$LRT_p[j]
    rs_det <- isTRUE(g$RS_detected[j])

    omnibus_text <- if (rs_det) {
      paste0("RS detected (", .format_p(p_val), ")")
    } else {
      "No RS"
    }
    omnibus_col <- if (rs_det) "#C0392B" else "#27AE60"

    grid::grid.text(
      omnibus_text,
      x = grid::unit(x_center, "npc"),
      y = grid::unit(1 - top_margin + 0.015, "npc"),
      gp = grid::gpar(cex = 0.55, col = omnibus_col, fontface = "bold")
    )
  }

  # ---- Draw heatmap cells ----
  for (i in 1:n_items) {
    # Item label on left
    y_center <- 1 - top_margin - (i - 0.5) * cell_h
    grid::grid.text(
      item_labels[i],
      x = grid::unit(left_margin - 0.02, "npc"),
      y = grid::unit(y_center, "npc"),
      just = "right",
      gp = grid::gpar(cex = 0.65)
    )

    for (j in 1:n_nodes) {
      x_center <- left_margin + (j - 0.5) * cell_w

      # Cell color
      rs_type <- rs_matrix[i, j]
      is_sig <- sig_matrix[i, j]

      if (is_sig && rs_type %in% c("Recalibration", "Reprioritization", "Both")) {
        fill_col <- .rs_colors[rs_type]
      } else {
        fill_col <- "#F0F0F0"
      }

      # Draw cell
      grid::grid.rect(
        x = grid::unit(x_center, "npc"),
        y = grid::unit(y_center, "npc"),
        width = grid::unit(cell_w * 0.9, "npc"),
        height = grid::unit(cell_h * 0.85, "npc"),
        gp = grid::gpar(fill = fill_col, col = "gray70", lwd = 0.5)
      )

      # Add chi2 value or significance marker
      if (show_chi2 && !is.na(chi2_matrix[i, j])) {
        grid::grid.text(
          round(chi2_matrix[i, j], 1),
          x = grid::unit(x_center, "npc"),
          y = grid::unit(y_center, "npc"),
          gp = grid::gpar(cex = 0.5,
                          col = if (is_sig) "white" else "gray50")
        )
      } else if (is_sig) {
        grid::grid.text(
          "*",
          x = grid::unit(x_center, "npc"),
          y = grid::unit(y_center, "npc"),
          gp = grid::gpar(cex = 0.9, col = "white", fontface = "bold")
        )
      }
    }

  }

  # ---- Legend ----
  legend_types <- c("Recalibration", "Reprioritization", "Both", "None")
  legend_x_start <- 1 - right_margin + 0.02
  legend_y_start <- 0.85

  grid::grid.text(
    "RS Type",
    x = grid::unit(legend_x, "npc"),
    y = grid::unit(legend_y_start + 0.04, "npc"),
    just = "left",
    gp = grid::gpar(cex = 0.65, fontface = "bold")
  )

  for (k in seq_along(legend_types)) {
    y_pos <- legend_y_start - (k - 1) * 0.065

    grid::grid.rect(
      x = grid::unit(legend_x + 0.012, "npc"),
      y = grid::unit(y_pos, "npc"),
      width = grid::unit(0.03, "npc"),
      height = grid::unit(0.03, "npc"),
      gp = grid::gpar(fill = .rs_colors[legend_types[k]],
                      col = "gray50", lwd = 0.5)
    )

    grid::grid.text(
      legend_types[k],
      x = grid::unit(legend_x + 0.04, "npc"),
      y = grid::unit(y_pos, "npc"),
      just = "left",
      gp = grid::gpar(cex = 0.55)
    )
  }

  # Footnote
  adj_text <- paste0(
    "Omnibus: ", rs$global_p_adjust, " adjusted",
    "  |  Item-level: ", rs$p_adjust, " adjusted",
    "  |  * p < ", rs$alpha
  )
  grid::grid.text(
    adj_text,
    x = grid::unit(0.5, "npc"),
    y = grid::unit(0.01, "npc"),
    gp = grid::gpar(cex = 0.5, col = "gray50")
  )

  invisible(rs)
}
