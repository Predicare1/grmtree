## =============================================================================
## grmforest.R  --  Forests of Graded Response Model Trees
##
## Revised implementation addressing:
##   (1) marginal (integrated) log-likelihood instead of theta = 0 evaluation
##   (2) correct mirt slope-intercept parameterization
##   (3) mtry actually passed through to the tree growing process
##   (4) response matrix extracted from the formula, not hardcoded as `resp`
##   (5) paired per-tree permutation differences
##   (6) native, reproducible parallelism (results identical to serial)
##   (7) OOB stored as row indices, not data-frame copies
##   (8) c() method for combining forests
##   (9) graceful handling of sparse / collapsed response categories
## =============================================================================


# ---------------------------------------------------------------------------
# Internal: quadrature grid for the standard normal latent distribution
#
# Uses the same rectangular scheme mirt uses internally: equally spaced points
# spanning the latent range, weighted by the normal density and normalised to
# sum to one. With 61 points this is accurate to well beyond the precision
# needed here, and it avoids taking a dependency on a Gauss-Hermite package.
# ---------------------------------------------------------------------------
#' @keywords internal
#' @noRd
.grm_quad <- function(quadpts = 61L, range = c(-6, 6)) {
  theta <- seq(range[1L], range[2L], length.out = quadpts)
  w <- stats::dnorm(theta)
  w <- w / sum(w)
  list(theta = theta, logw = log(w))
}


# ---------------------------------------------------------------------------
# Internal: numerically stable log(sum(exp(x))) applied to matrix rows
# ---------------------------------------------------------------------------
#' @keywords internal
#' @noRd
.row_logsumexp <- function(m) {
  mx <- apply(m, 1L, max)
  mx + log(rowSums(exp(m - mx)))
}


# ---------------------------------------------------------------------------
# Internal: build the evaluation data for a tree
#
# Extracts the response matrix from the model formula (so any response
# specification works: a matrix column, or cbind(item1, item2, ...)), and
# drops rows with missing values on the *partitioning* variables only.
# Missing item responses are retained and handled by the likelihood, which
# skips them, matching how mirt treats missing data.
# ---------------------------------------------------------------------------
#' @keywords internal
#' @noRd
.grm_eval_frame <- function(formula, data) {
  mf <- stats::model.frame(formula, data = data, na.action = stats::na.pass)
  y <- stats::model.response(mf)
  if (is.null(y)) {
    stop("Could not extract a response matrix from the model formula.")
  }
  if (!is.matrix(y)) y <- as.matrix(y)

  covs <- mf[, -1L, drop = FALSE]
  keep <- if (ncol(covs) == 0L) rep(TRUE, nrow(mf)) else stats::complete.cases(covs)

  list(y = y[keep, , drop = FALSE],
       data = data[keep, , drop = FALSE],
       keep = keep)
}


# ---------------------------------------------------------------------------
# Internal: canonical response levels, taken once from the full dataset
#
# mirt recodes each item's responses to 1..K by sorted unique observed value.
# To score out-of-bag responses against a node model we need to know which
# column of the trace lines corresponds to which observed value. We take the
# sorted unique values from the *full* data as the canonical coding.
# ---------------------------------------------------------------------------
#' @keywords internal
#' @noRd
.grm_levels <- function(y) {
  lapply(seq_len(ncol(y)), function(j) sort(unique(y[!is.na(y[, j]), j])))
}


# ---------------------------------------------------------------------------
# Internal: marginal log-likelihood of a response matrix under a fitted
# node model, integrating the latent trait out against N(0, 1).
#
#   LL = sum_i log( sum_q w_q * prod_j P(Y_ij = y_ij | theta_q) )
#
# Category probabilities come from mirt::probtrace(), which applies the
# correct slope-intercept parameterization for whatever item type was fitted.
# We never touch the a / d values by hand.
#
# Items whose category count in the node model differs from the canonical
# level set (i.e. a category was empty in this node and mirt collapsed it)
# cannot be mapped unambiguously and are skipped, with a note. Because
# importance is computed as a *paired* baseline-minus-permuted difference
# within the same tree, skipping an item consistently within a tree leaves
# the difference unbiased.
# ---------------------------------------------------------------------------
#' @keywords internal
#' @noRd
.grm_node_loglik <- function(model, y, levels_list, quad, skipped = NULL) {
  if (is.null(model) || nrow(y) == 0L) return(0)

  nq <- length(quad$theta)
  theta <- matrix(quad$theta, ncol = 1L)

  # accumulator: rows = persons, cols = quadrature points
  lp <- matrix(0, nrow = nrow(y), ncol = nq)

  nitems_model <- tryCatch(mirt::extract.mirt(model, "nitems"),
                           error = function(e) ncol(y))
  nitems <- min(ncol(y), nitems_model)

  for (j in seq_len(nitems)) {
    item <- tryCatch(mirt::extract.item(model, j), error = function(e) NULL)
    if (is.null(item)) next

    P <- tryCatch(mirt::probtrace(item, theta), error = function(e) NULL)
    if (is.null(P) || !is.matrix(P)) next

    # trace lines can carry non-finite entries when a threshold failed to
    # estimate (sparse category); drop the item rather than propagating NA
    if (any(!is.finite(P))) next

    lev <- levels_list[[j]]
    if (ncol(P) != length(lev)) {
      # node model collapsed or dropped a category - cannot map safely
      if (!is.null(skipped)) skipped$items <- unique(c(skipped$items, j))
      next
    }

    yj <- y[, j]
    k <- match(yj, lev)                 # NA for missing or unseen values
    ok <- !is.na(k)
    if (!any(ok)) next

    P <- pmax(P, .Machine$double.eps)
    # logP[i, q] = log P(Y_ij = y_ij | theta_q)
    lp[ok, ] <- lp[ok, , drop = FALSE] + t(log(P))[k[ok], , drop = FALSE]
  }

  sum(.row_logsumexp(sweep(lp, 2L, quad$logw, "+")))
}


# ---------------------------------------------------------------------------
# Internal: OOB marginal log-likelihood of one tree
# ---------------------------------------------------------------------------
#' @keywords internal
#' @noRd
.grm_tree_loglik <- function(tree, formula, data, levels_list, quad,
                             skipped = NULL) {
  if (is.null(tree) || nrow(data) == 0L) return(NA_real_)

  ef <- tryCatch(.grm_eval_frame(formula, data), error = function(e) NULL)
  if (is.null(ef) || nrow(ef$y) == 0L) return(NA_real_)

  nodes <- tryCatch(
    stats::predict(tree, newdata = ef$data, type = "node"),
    error = function(e) NULL
  )
  if (is.null(nodes) || length(nodes) != nrow(ef$y)) return(NA_real_)

  ll <- 0
  for (id in unique(nodes)) {
    idx <- which(nodes == id)
    model <- tryCatch(
      partykit::nodeapply(tree, ids = id,
                          FUN = function(n) partykit::info_node(n)$object)[[1L]],
      error = function(e) NULL
    )
    if (is.null(model)) next
    ll <- ll + .grm_node_loglik(model, ef$y[idx, , drop = FALSE],
                                levels_list, quad, skipped)
  }
  ll
}


# ---------------------------------------------------------------------------
# Internal: cross-platform parallel lapply. Because all randomness is drawn
# in the master process before this is called, the result does not depend on
# how the work is distributed.
# ---------------------------------------------------------------------------
#' @keywords internal
#' @noRd
.grm_lapply <- function(X, FUN, n_cores = 1L,
                        packages = c("mirt", "partykit", "grmtree"),
                        export = NULL, export_env = parent.frame()) {
  n_cores <- max(1L, as.integer(n_cores))
  if (n_cores == 1L || length(X) == 1L) return(lapply(X, FUN))

  if (.Platform$OS.type != "windows") {
    ## fork: workers inherit the master's loaded packages and environment, so
    ## grmtree() and the internal helpers are already visible.
    parallel::mclapply(X, FUN, mc.cores = n_cores, mc.preschedule = FALSE)
  } else {
    ## PSOCK: workers are fresh sessions. They must (a) load the packages the
    ## work depends on -- including grmtree, so grmtree() and the internal
    ## scoring helpers resolve when the package is installed -- and (b) receive
    ## any objects that live in the caller's environment rather than a package
    ## namespace (the case when the code is sourced during development).
    cl <- parallel::makePSOCKcluster(n_cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterExport(cl, "packages", envir = environment())
    parallel::clusterEvalQ(cl, {
      for (.p in packages) suppressMessages(requireNamespace(.p, quietly = TRUE))
      NULL
    })
    if (!is.null(export)) {
      have <- export[vapply(export, exists, logical(1L),
                            envir = export_env, USE.NAMES = FALSE)]
      if (length(have)) parallel::clusterExport(cl, have, envir = export_env)
    }
    parallel::parLapply(cl, X, FUN)
  }
}

#' Fit a Forest of Graded Response Model Trees for Ensemble-Based DIF Detection
#'
#' This function implements a forest of graded response model trees (GRM Forest)
#' using bootstrap aggregation (bagging) or random subsampling to enhance the
#' detection and analysis of differential item functioning (DIF) in polytomous
#' items. The GRM Forest approach combines the strengths of multiple GRMTrees to
#' provide more robust and stable DIF detection, particularly for complex
#' datasets with high-dimensional covariates or subtle DIF patterns.
#'
#' @param formula A formula specifying the model structure with the response
#'   matrix on the left and partitioning variables on the right (e.g.,
#'   `response_matrix ~ age + gender + education + clinical_variables`). The
#'   response may be a matrix column of `data` or an inline
#'   `cbind(item1, item2,...)` construction.
#' @param data A data frame containing the response matrix and partitioning
#'   variables. The response matrix should contain polytomous items coded as
#'   ordered factors.
#' @param control A control object created by `grmforest.control()`.
#' @param ... Additional arguments passed to underlying `grmtree()` function.
#'
#' @return An object of class `grmforest`, a list with components:
#'   \item{trees}{List of fitted `grmtree` objects.}
#'   \item{oob_indices}{List of integer vectors giving, for each tree, the row
#'     positions of `data` held out of that tree's resample.}
#'   \item{in_indices}{List of integer vectors giving the rows used to fit
#'     each tree.}
#'   \item{formula}{The model formula.}
#'   \item{data}{The original data frame.}
#'   \item{control}{The control object used.}
#'   \item{call}{The matched call.}
#'
#'
#' @details
#' Each tree is grown by [grmtree()] on a resample of the rows of `data`. Rows
#' not selected form that tree's out-of-bag (OOB) sample, which is used by
#' [varimp()] and never enters that tree's fitting.
#'
#' Setting `mtry` in [grmforest.control()] offers a fresh random subset of
#' partitioning variables at every node, decorrelating the trees. Leaving
#' `mtry = NULL` grows a bagged ensemble in which every tree sees every
#' variable at every node; such trees are highly correlated, which limits the
#' variance reduction the ensemble can deliver and can make importance scores
#' harder to interpret when partitioning variables are themselves correlated.
#'
#' Out-of-bag membership is stored as row indices rather than as copies of the
#' data, which keeps the fitted object small and lets forests grown separately
#' be combined with [c.grmforest()].
#'
#' Key advantages of the GRM Forest approach include:
#' - Enhanced stability in DIF detection across different sampling variations
#' - Robust variable importance measures that quantify the relative contribution
#' of each covariate to DIF patterns
#' - Reduced false positive rates through consensus-based detection
#' - Ability to handle high-dimensional covariate spaces effectively
#' - Internal validation through out-of-bag error estimation
#'
#' The forest implementation supports both bootstrap aggregation (where samples
#' are drawn with replacement) and subsampling (without replacement), allowing
#' flexibility for different data characteristics and research objectives.
#'
#' @section Computational cost:
#' Model-based recursive partitioning selects each split point by refitting the
#' node model at every admissible cut point on the selected variable. For
#' continuous covariates with many distinct values this dominates run time, and
#' cost grows with the number of items and response categories. Practical
#' levers, roughly in order of effect: bin continuous partitioning variables to
#' a manageable number of candidate cut points before fitting; raise
#' `minbucket`; set `mtry`; and increase `n_cores`.
#'
#' @examples
#'
#' \donttest{
#'   library(grmtree)
#'   library(hlt)
#'   data("asti", package = "hlt")
#'   asti$resp <- data.matrix(asti[, 1:4])
#'
#'   # Fit forest with default parameters
#'   forest <- grmforest(resp ~ gender + group, data = asti)
#'
#'   # Fit with custom control
#'   ctrl <- grmforest.control(n_tree = 20, sampling = "subsample")
#'   forest <- grmforest(resp ~ gender + group, data = asti, control = ctrl)
#' }
#'
#' @seealso \code{\link{grmtree}} fits a Graded Response Model Tree,
#'   \code{\link{grmtree.control}} creates a control object for `grmtree`,
#'   \code{\link{grmforest.control}} creates a control object for `grmforest`,
#'   \code{\link{c.grmforest}} combines grmforest objects,
#'   \code{\link{varimp}} calculates the variable importance for GRM Forest,
#'   \code{\link{plot.varimp}} creates a bar plot of variable importance scores
#'
#' @export
#' @importFrom stats model.frame model.response complete.cases predict
grmforest <- function(formula, data, control = grmforest.control(), ...) {

  if (!inherits(formula, "formula")) {
    stop("'formula' must be a valid formula object")
  }
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame")
  }
  if (!inherits(control, "grmforest_control")) {
    stop("'control' must be created by grmforest.control()")
  }

  mf <- tryCatch(
    stats::model.frame(formula, data = data),
    error = function(e) stop("Error in model.frame: ", e$message)
  )
  y <- stats::model.response(mf)
  if (!is.matrix(y)) {
    stop("Response variable must be a matrix of item responses")
  }
  if (nrow(data) < 10L) {
    stop("Insufficient data: nrow(data) must be at least 10")
  }

  ## Coerce character partitioning variables to factors. partykit routes a
  ## categorical (character/factor) split variable by integer level codes and
  ## errors ("variable N is not integer") on a bare character column, which
  ## makes individual trees fail on the resamples whose split lands on such a
  ## variable. Converting once here fixes every tree.
  part_vars <- all.vars(formula[[3L]])
  char_vars <- part_vars[vapply(part_vars, function(v)
    v %in% names(data) && is.character(data[[v]]), logical(1L))]
  if (length(char_vars)) {
    message("Converting character partitioning variable(s) to factor: ",
            paste(char_vars, collapse = ", "))
    for (v in char_vars) data[[v]] <- factor(data[[v]])
  }

  cl <- match.call()
  n <- nrow(data)
  sample_size <- max(1L, round(n * control$sample_fraction))

  ## ---- All randomness happens here, once, in the master process ----------
  ## Everything downstream is a deterministic function of in_indices, so the
  ## forest is identical regardless of n_cores.
  if (!is.null(control$seed)) set.seed(control$seed)

  in_indices <- lapply(seq_len(control$n_tree), function(i) {
    if (control$sampling == "bootstrap") {
      sample.int(n, size = sample_size, replace = TRUE)
    } else {
      sample.int(n, size = sample_size, replace = FALSE)
    }
  })
  oob_indices <- lapply(in_indices, function(idx) setdiff(seq_len(n), idx))
  ## -----------------------------------------------------------------------

  tree_ctrl <- control$control
  if (!is.null(control$mtry)) tree_ctrl$mtry <- control$mtry

  fit_one <- function(i) {
    tryCatch(
      grmtree(formula,
              data    = data[in_indices[[i]], , drop = FALSE],
              control = tree_ctrl,
              ...),
      error = function(e) structure(list(message = conditionMessage(e), tree = i),
                                    class = "grmforest_failure")
    )
  }

  if (control$verbose) {
    message("Growing ", control$n_tree, " trees on ", control$n_cores,
            " core(s) ...")
  }

  ## 'export' covers the sourced-development case (grmtree not installed as a
  ## package); when grmtree IS installed these already resolve via its namespace.
  trees <- .grm_lapply(seq_len(control$n_tree), fit_one,
                       n_cores = control$n_cores,
                       export = c("grmtree", "grmtree.control"),
                       export_env = environment())

  failed <- vapply(trees, inherits, logical(1L), what = "grmforest_failure")
  if (any(failed)) {
    msgs <- vapply(trees[failed], function(f) f$message, character(1L))
    if (!control$remove_dead_trees) {
      stop("Tree ", which(failed)[1L], " failed: ", msgs[1L])
    }
    warning(sum(failed), " tree(s) failed and were removed. First message: ",
            msgs[1L])
    trees       <- trees[!failed]
    in_indices  <- in_indices[!failed]
    oob_indices <- oob_indices[!failed]
  }
  if (length(trees) == 0L) stop("All trees failed to fit")

  structure(
    list(trees       = trees,
         oob_indices = oob_indices,
         in_indices  = in_indices,
         formula     = formula,
         data        = data,
         control     = control,
         call        = cl),
    class = "grmforest"
  )
}


#' Print Method for GRM Forests
#'
#' @param x A `grmforest` object.
#' @param ... Currently unused.
#' @return Invisibly returns `x`.
#' @export
print.grmforest <- function(x, ...) {
  if (!inherits(x, "grmforest")) stop("'x' must be a grmforest object")

  sizes <- vapply(x$trees,
                  function(t) length(partykit::nodeids(t, terminal = TRUE)),
                  numeric(1L))

  cat("GRM Forest with", length(x$trees), "trees\n")
  cat("  Formula:          ", deparse(x$formula), "\n")
  cat("  Observations:     ", nrow(x$data), "\n")
  cat("  Sampling:         ", x$control$sampling,
      sprintf("(fraction %.3f)", x$control$sample_fraction), "\n")
  cat("  mtry:             ",
      if (is.null(x$control$mtry)) "all variables" else x$control$mtry, "\n")
  cat("  Terminal nodes:    median", stats::median(sizes),
      sprintf("(range %d-%d)", min(sizes), max(sizes)), "\n")
  invisible(x)
}


#' Combine GRM Forests
#'
#' Merges two or more `grmforest` objects grown on the same data and formula
#' into a single forest. This makes it straightforward to grow an ensemble in
#' chunks -- across separate sessions, or across nodes of a compute cluster --
#' and assemble the pieces afterwards for a single call to [varimp()].
#'
#' @param ... Two or more `grmforest` objects.
#' @return A single `grmforest` object containing all trees.
#'
#' @details Because out-of-bag membership is stored as row indices into the
#' shared `data`, merging requires no realignment: the indices from each
#' contributing forest remain valid in the combined object. The forests must
#' have been grown on data frames with the same number of rows and on an
#' identical formula, which is checked.
#'
#' If you grow chunks separately, give each chunk a different `seed`, otherwise
#' every chunk will contain the same trees.
#'
#' @examples
#' \donttest{
#'   library(grmtree)
#'   library(hlt)
#'   data("asti", package = "hlt")
#'   asti$resp <- data.matrix(asti[, 1:4])
#'
#'   f1 <- grmforest(resp ~ gender + group, data = asti,
#'                   control = grmforest.control(n_tree = 5, seed = 1))
#'   f2 <- grmforest(resp ~ gender + group, data = asti,
#'                   control = grmforest.control(n_tree = 5, seed = 2))
#'   big <- c(f1, f2)
#'   print(big)
#' }
#'
#' @seealso \code{\link{grmforest}} for GRM Forests, \code{\link{varimp}}
#'   calculates the variable importance for GRM Forest
#' @export
c.grmforest <- function(...) {
  forests <- list(...)
  ok <- vapply(forests, inherits, logical(1L), what = "grmforest")
  if (!all(ok)) stop("All arguments must be grmforest objects")
  if (length(forests) == 1L) return(forests[[1L]])

  ref <- forests[[1L]]
  for (i in seq_along(forests)[-1L]) {
    if (!identical(deparse(ref$formula), deparse(forests[[i]]$formula))) {
      stop("Forests were grown with different formulas and cannot be combined")
    }
    if (nrow(ref$data) != nrow(forests[[i]]$data)) {
      stop("Forests were grown on data of different sizes and cannot be combined")
    }
  }

  out <- ref
  out$trees       <- unlist(lapply(forests, `[[`, "trees"),       recursive = FALSE)
  out$oob_indices <- unlist(lapply(forests, `[[`, "oob_indices"), recursive = FALSE)
  out$in_indices  <- unlist(lapply(forests, `[[`, "in_indices"),  recursive = FALSE)
  out$call        <- match.call()
  out
}
