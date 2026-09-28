#' Intraclass correlation and design effect from a random-intercept model
#'
#' Computes the intraclass correlation (ICC) and, optionally, a design effect
#' from a random-intercept multilevel model fitted with \code{lme4::lmer()}.
#'
#' The ICC is a model-based share of variance, not a causal statement about
#' clusters and not a fixed property of a population. With
#' \code{cluster_size = "auto"} the observed cluster sizes are taken from the
#' model frame and two design effects are reported: the familiar
#' \eqn{1 + (\bar m - 1)ICC} using the mean cluster size, and an unequal-size
#' version that replaces \eqn{\bar m} with \eqn{\sum n_j^2 / \sum n_j}. The
#' second is the more appropriate teaching approximation when cluster sizes
#' vary, and both are approximations rather than substitutes for fitting and
#' diagnosing a multilevel model.
#'
#' @param model A fitted \code{lmerMod} object with at least one random
#'   intercept.
#' @param cluster_size Optional. A scalar average cluster size used to compute
#'   the design effect, or the string \code{"auto"} to take cluster sizes from
#'   the fitted model. If \code{NULL}, no design effect is computed.
#'
#' @return An object of class \code{hlm_icc} with components:
#'   \item{icc}{Estimated intraclass correlation.}
#'   \item{deff}{Design effect based on the supplied or mean cluster size.}
#'   \item{deff_unequal}{Design effect adjusted for unequal cluster sizes
#'     (only when \code{cluster_size = "auto"}).}
#'   \item{re_var}{Random intercept variance.}
#'   \item{resid_var}{Residual variance.}
#'   \item{n_clusters}{Number of clusters (when available).}
#'   \item{mean_cluster_size}{Mean cluster size (when available).}
#'
#' @examples
#' set.seed(3)
#' toy <- data.frame(
#'   math_score = rnorm(80, 50, 10),
#'   SES = rnorm(80),
#'   school_id = rep(seq_len(8), each = 10)
#' )
#' m <- lme4::lmer(math_score ~ SES + (1 | school_id), data = toy)
#' hlm_icc(m, cluster_size = 10)
#' hlm_icc(m, cluster_size = "auto")
#' @export
hlm_icc <- function(model, cluster_size = NULL) {
  if (!inherits(model, "lmerMod")) {
    stop("hlm_icc() currently supports models fitted with lme4::lmer().")
  }

  vc <- lme4::VarCorr(model)
  if (length(vc) == 0L) {
    stop("The model has no random effects.")
  }
  if (length(vc) > 1L) {
    warning(
      "The model has more than one grouping factor; the ICC is computed for '",
      names(vc)[1L], "' only.",
      call. = FALSE
    )
  }

  first_term <- vc[[1L]]
  if (!identical(colnames(first_term)[1L], "(Intercept)")) {
    warning(
      "The first random-effect term does not begin with an intercept; ",
      "interpret the reported ICC with care.",
      call. = FALSE
    )
  }
  if (ncol(first_term) > 1L) {
    warning(
      "The model contains a random slope. The ICC reported here uses the ",
      "random-intercept variance only, so it refers to clusters at the point ",
      "where the Level-1 predictor equals zero and is not a single variance ",
      "share for the whole model.",
      call. = FALSE
    )
  }

  re_var    <- as.numeric(first_term)[1L]
  resid_var <- attr(vc, "sc")^2
  icc       <- re_var / (re_var + resid_var)

  # Observed cluster sizes, when the model frame can be reconstructed.
  cluster_counts <- NULL
  mf <- tryCatch(stats::model.frame(model), error = function(e) NULL)
  group_name <- names(vc)[1L]
  if (!is.null(mf) && group_name %in% names(mf)) {
    cluster_counts <- as.numeric(table(as.character(mf[[group_name]])))
  }

  deff         <- NA_real_
  deff_unequal <- NA_real_
  m_bar        <- if (is.null(cluster_counts)) NA_real_ else mean(cluster_counts)

  if (!is.null(cluster_size)) {
    if (is.character(cluster_size) && identical(cluster_size[1L], "auto")) {
      if (is.null(cluster_counts)) {
        warning(
          "Cluster sizes could not be recovered from the model; supply ",
          "'cluster_size' as a number.",
          call. = FALSE
        )
      } else {
        deff <- 1 + (m_bar - 1) * icc
        m_effective <- sum(cluster_counts^2) / sum(cluster_counts)
        deff_unequal <- 1 + (m_effective - 1) * icc
      }
    } else if (is.numeric(cluster_size) && length(cluster_size) == 1L &&
               is.finite(cluster_size)) {
      deff <- 1 + (cluster_size - 1) * icc
    } else {
      stop("'cluster_size' must be a single finite number, \"auto\", or NULL.")
    }
  }

  out <- list(
    icc               = icc,
    deff              = deff,
    deff_unequal      = deff_unequal,
    re_var            = re_var,
    resid_var         = resid_var,
    n_clusters        = if (is.null(cluster_counts)) NA_integer_ else length(cluster_counts),
    mean_cluster_size = m_bar
  )
  class(out) <- c("hlm_icc", class(out))
  out
}

#' @export
print.hlm_icc <- function(x, ...) {
  cat("Intraclass correlation (ICC) and design effect\n")
  cat(sprintf("  ICC           : %.3f\n", x$icc))
  cat(sprintf("  RE variance   : %.3f\n", x$re_var))
  cat(sprintf("  Residual var. : %.3f\n", x$resid_var))
  if (!is.null(x$n_clusters) && !is.na(x$n_clusters)) {
    cat(sprintf("  Clusters      : %d (mean size %.2f)\n",
                as.integer(x$n_clusters), x$mean_cluster_size))
  }
  if (!is.na(x$deff)) {
    cat(sprintf("  Design effect : %.3f\n", x$deff))
  } else {
    cat("  Design effect : (not computed; supply 'cluster_size' to hlm_icc)\n")
  }
  if (!is.null(x$deff_unequal) && !is.na(x$deff_unequal)) {
    cat(sprintf("  Design effect (unequal cluster sizes): %.3f\n",
                x$deff_unequal))
  }
  invisible(x)
}
