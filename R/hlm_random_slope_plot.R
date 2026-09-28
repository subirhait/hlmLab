#' Cluster-specific slopes from a random-slope model
#'
#' Draws one fitted line per cluster from a model containing a random slope,
#' together with the average fitted line. The display communicates unexplained
#' heterogeneity in a Level-1 association across clusters.
#'
#' A random slope is not a cross-level interaction. It shows that the Level-1
#' association varies across clusters without saying why. A cross-level
#' interaction requires an observed Level-2 moderator; see
#' \code{\link{hlm_cross_level_plot}}.
#'
#' All fixed-effect columns other than the intercept and \code{x_within} are
#' held at their sample means when \code{hold_other = "mean"}, so the lines are
#' plotted on the scale of the observed outcome. Cluster-specific intercepts
#' and slopes are conditional (empirical Bayes) quantities and are already
#' shrunken toward the average; they should not be treated as precise rankings
#' of individual clusters.
#'
#' @param model A fitted \code{lmerMod} model with a random slope term of the
#'   form \code{(x_within | cluster)}.
#' @param x_within Name of the Level-1 predictor with a random slope (string).
#' @param cluster Name of the clustering factor (string).
#' @param n_points Number of points along the x-axis. Defaults to 40.
#' @param n_clusters Maximum number of clusters to display. Defaults to 20.
#' @param select How clusters are chosen when there are more than
#'   \code{n_clusters}: \code{"spread"} (the default) selects clusters evenly
#'   across the distribution of conditional slopes, \code{"random"} samples
#'   them, and \code{"all"} displays every cluster. \code{"random"} uses the
#'   current state of the random number generator; call \code{set.seed()}
#'   beforehand for a reproducible figure.
#' @param x_range Optional length-2 numeric vector giving the plotting range of
#'   \code{x_within}. Defaults to the 0.02 and 0.98 quantiles of the observed
#'   predictor.
#' @param hold_other Either \code{"mean"} (the default) or \code{"zero"},
#'   controlling the value at which the remaining fixed-effect terms are held.
#' @param show_average Logical; draw the average fitted line. Defaults to
#'   \code{TRUE}.
#' @param title,subtitle Optional plot title and subtitle. \code{NULL} uses
#'   informative defaults; \code{NA} omits the element.
#'
#' @return A ggplot object. The plotted line data are attached as the
#'   attribute \code{"data"}.
#'
#' @seealso \code{\link{hlm_cross_level_plot}}, \code{\link{hlm_shrinkage_plot}}
#'
#' @examples
#' set.seed(42)
#' n_schools <- 10
#' n_students <- 15
#' school_id <- rep(seq_len(n_schools), each = n_students)
#' SES_c <- rnorm(n_schools * n_students)
#' u0 <- rep(rnorm(n_schools, sd = 0.5), each = n_students)
#' u1 <- rep(rnorm(n_schools, sd = 0.3), each = n_students)
#' math_score <- 50 + 2 * SES_c + u0 + u1 * SES_c +
#'   rnorm(n_schools * n_students, sd = 1)
#' toy <- data.frame(math_score = math_score, SES_c = SES_c,
#'                   school_id = school_id)
#'
#' m <- lme4::lmer(math_score ~ SES_c + (SES_c | school_id), data = toy)
#' hlm_random_slope_plot(m, x_within = "SES_c", cluster = "school_id")
#' @export
hlm_random_slope_plot <- function(model, x_within, cluster,
                                  n_points = 40, n_clusters = 20,
                                  select = c("spread", "random", "all"),
                                  x_range = NULL,
                                  hold_other = c("mean", "zero"),
                                  show_average = TRUE,
                                  title = NULL, subtitle = NULL) {
  if (!inherits(model, "lmerMod")) {
    stop("hlm_random_slope_plot() currently supports models fitted with lme4::lmer().")
  }
  select     <- match.arg(select)
  hold_other <- match.arg(hold_other)

  re_list <- lme4::ranef(model)
  if (!cluster %in% names(re_list)) {
    stop("Random effects for the specified 'cluster' were not found in the model.")
  }
  re <- re_list[[cluster]]

  if (!"(Intercept)" %in% colnames(re)) {
    stop("The clustering factor must have a random intercept.")
  }
  if (!x_within %in% colnames(re)) {
    stop(
      "No random slope for '", x_within, "' was found. ",
      "Fit a model of the form (", x_within, " | ", cluster, ")."
    )
  }

  fe <- lme4::fixef(model)
  if (!"(Intercept)" %in% names(fe)) {
    stop("The model must include a fixed intercept.")
  }
  if (!x_within %in% names(fe)) {
    stop("'", x_within, "' must also be a fixed effect in the model.")
  }

  beta0 <- unname(fe["(Intercept)"])
  beta1 <- unname(fe[x_within])

  X  <- tryCatch(stats::model.matrix(model), error = function(e) NULL)
  mf <- tryCatch(stats::model.frame(model), error = function(e) NULL)

  # Hold the remaining fixed-effect columns at their sample means (or at zero).
  offset_other <- 0
  if (identical(hold_other, "mean") && !is.null(X)) {
    keep <- setdiff(intersect(colnames(X), names(fe)),
                    c("(Intercept)", x_within))
    if (length(keep) > 0L) {
      offset_other <- sum(fe[keep] * colMeans(X[, keep, drop = FALSE]))
    }
  }

  # Plotting range for the Level-1 predictor.
  if (is.null(x_range)) {
    xvals <- NULL
    if (!is.null(mf) && x_within %in% names(mf) && is.numeric(mf[[x_within]])) {
      xvals <- mf[[x_within]]
    } else if (!is.null(X) && x_within %in% colnames(X)) {
      xvals <- X[, x_within]
    }
    x_range <- if (is.null(xvals)) {
      c(-2, 2)
    } else {
      unname(stats::quantile(xvals, c(0.02, 0.98), na.rm = TRUE))
    }
  }
  if (length(x_range) != 2L || !all(is.finite(x_range))) {
    stop("'x_range' must be a finite numeric vector of length two.")
  }

  cluster_ids  <- rownames(re)
  intercept_j  <- beta0 + re[["(Intercept)"]] + offset_other
  slope_j      <- beta1 + re[[x_within]]
  n_available  <- length(cluster_ids)

  if (identical(select, "all") || n_available <= n_clusters) {
    keep_index <- seq_len(n_available)
  } else if (identical(select, "spread")) {
    ordered_index <- order(slope_j)
    keep_index <- ordered_index[
      unique(round(seq(1, n_available, length.out = n_clusters)))
    ]
  } else {
    keep_index <- sample(seq_len(n_available), n_clusters)
  }

  x_grid <- seq(x_range[1L], x_range[2L], length.out = n_points)

  line_list <- lapply(keep_index, function(j) {
    data.frame(
      cluster_id = rep(cluster_ids[j], n_points),
      x = x_grid,
      y = intercept_j[j] + slope_j[j] * x_grid,
      stringsAsFactors = FALSE
    )
  })
  line_df <- dplyr::bind_rows(line_list)

  average_df <- data.frame(
    x = x_grid,
    y = beta0 + offset_other + beta1 * x_grid
  )

  # Standard deviation of the random slope, when available.
  slope_sd <- NA_real_
  vc <- tryCatch(as.data.frame(lme4::VarCorr(model)), error = function(e) NULL)
  if (!is.null(vc)) {
    hit <- vc$grp == cluster & vc$var1 %in% x_within & is.na(vc$var2)
    if (any(hit, na.rm = TRUE)) slope_sd <- vc$sdcor[which(hit)[1L]]
  }

  response <- tryCatch(
    deparse(stats::formula(model)[[2L]]),
    error = function(e) "outcome"
  )

  if (is.null(title)) {
    title <- "Variation in the within-cluster association across clusters"
  }
  if (is.null(subtitle)) {
    subtitle <- sprintf(
      "%d of %d clusters shown; average slope = %.2f%s",
      length(keep_index), n_available, beta1,
      if (is.finite(slope_sd)) sprintf("; slope SD = %.2f", slope_sd) else ""
    )
  }

  p <- ggplot2::ggplot(
    line_df,
    ggplot2::aes(x = x, y = y, group = cluster_id)
  ) +
    ggplot2::geom_line(colour = "#6C9ABB", alpha = 0.32, linewidth = 0.65)

  if (isTRUE(show_average)) {
    p <- p + ggplot2::geom_line(
      data = average_df,
      ggplot2::aes(x = x, y = y),
      inherit.aes = FALSE,
      colour = "#D97706",
      linewidth = 1.25
    )
  }

  p <- p +
    ggplot2::labs(
      x = x_within,
      y = paste("Predicted", response),
      title = if (is.na(title)) NULL else title,
      subtitle = if (is.na(subtitle)) NULL else subtitle
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())

  attr(p, "data") <- line_df
  p
}

#' Geometry of a cross-level interaction (deprecated)
#'
#' @description
#' Deprecated in hlmLab 0.2.0. The display produced by this function shows
#' random-slope heterogeneity, which is not the same thing as a cross-level
#' interaction, so the function was renamed. Use
#' \code{\link{hlm_random_slope_plot}} instead; use
#' \code{\link{hlm_cross_level_plot}} when the model contains an observed
#' Level-2 moderator.
#'
#' @param model A fitted \code{lmerMod} model with a random slope term.
#' @param x_within Name of the Level-1 predictor with a random slope (string).
#' @param cluster Name of the clustering factor (string).
#' @param n_points Number of points to plot along the x-axis. Defaults to 20.
#' @param n_clusters Maximum number of clusters to display (sampled).
#'
#' @return A ggplot object, produced by \code{\link{hlm_random_slope_plot}}.
#'
#' @examples
#' set.seed(42)
#' school_id <- rep(seq_len(10), each = 15)
#' SES_c <- rnorm(150)
#' u0 <- rep(rnorm(10, sd = 0.5), each = 15)
#' u1 <- rep(rnorm(10, sd = 0.3), each = 15)
#' math_score <- 50 + 2 * SES_c + u0 + u1 * SES_c + rnorm(150, sd = 1)
#' toy <- data.frame(math_score = math_score, SES_c = SES_c,
#'                   school_id = school_id)
#' m <- lme4::lmer(math_score ~ SES_c + (SES_c | school_id), data = toy)
#' suppressWarnings(
#'   hlm_xint_geom(m, x_within = "SES_c", cluster = "school_id")
#' )
#' @export
hlm_xint_geom <- function(model, x_within, cluster,
                          n_points = 20, n_clusters = 20) {
  .Deprecated(
    new = "hlm_random_slope_plot",
    package = "hlmLab",
    msg = paste(
      "hlm_xint_geom() is deprecated in hlmLab 0.2.0 and will be removed in a",
      "future release. A random slope is unexplained slope heterogeneity, not",
      "a cross-level interaction. Use hlm_random_slope_plot(), or",
      "hlm_cross_level_plot() when an observed Level-2 moderator is in the",
      "model."
    )
  )
  hlm_random_slope_plot(
    model      = model,
    x_within   = x_within,
    cluster    = cluster,
    n_points   = n_points,
    n_clusters = n_clusters,
    select     = "random"
  )
}
