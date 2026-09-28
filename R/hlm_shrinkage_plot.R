#' Partial pooling: raw cluster means and multilevel estimates
#'
#' Compares each cluster's raw mean with its multilevel (empirical Bayes)
#' estimate and draws an arrow between them. The figure makes partial pooling
#' visible: small clusters are pulled further toward the overall mean than
#' large clusters, and extreme raw means are moderated most.
#'
#' The multilevel estimate for cluster \eqn{j} is
#' \eqn{\hat\beta_0 + \hat u_{0j}}, which for an unconditional random-intercept
#' model equals \eqn{\lambda_j \bar Y_j + (1 - \lambda_j)\hat\beta_0} with
#' reliability \eqn{\lambda_j = \tau^2 / (\tau^2 + \sigma^2 / n_j)}. The
#' comparison is interpretable in that unconditional case; a warning is issued
#' when the model contains additional fixed effects, because a raw cluster mean
#' and an adjusted intercept are then no longer the same quantity.
#'
#' @param model A fitted \code{lmerMod} model with a random intercept, normally
#'   an unconditional model of the form \code{y ~ 1 + (1 | cluster)}.
#' @param n_clusters Maximum number of clusters to display. Defaults to 25.
#' @param select How clusters are chosen when there are more than
#'   \code{n_clusters}: \code{"spread"} (the default) selects clusters evenly
#'   across the distribution of raw means, \code{"random"} samples them, and
#'   \code{"all"} displays every cluster. \code{"random"} uses the current state
#'   of the random number generator.
#' @param style \code{"arrows"} (the default) places clusters on the vertical
#'   axis ordered by raw mean; \code{"size"} places cluster size on the
#'   horizontal axis, which shows directly that smaller clusters shrink more.
#' @param title,subtitle Optional plot title and subtitle. \code{NULL} uses
#'   informative defaults; \code{NA} omits the element.
#'
#' @return A ggplot object. A data frame with the raw mean, multilevel
#'   estimate, cluster size, and reliability for every displayed cluster is
#'   attached as the attribute \code{"data"}.
#'
#' @seealso \code{\link{hlm_icc}}, \code{\link{hlm_icc_demo}}
#'
#' @examples
#' set.seed(11)
#' cluster_size <- sample(3:40, 25, replace = TRUE)
#' school_id <- rep(seq_along(cluster_size), times = cluster_size)
#' u0 <- rep(rnorm(length(cluster_size), sd = 5), times = cluster_size)
#' math_score <- 50 + u0 + rnorm(length(school_id), sd = 10)
#' toy <- data.frame(math_score = math_score, school_id = school_id)
#'
#' m0 <- lme4::lmer(math_score ~ 1 + (1 | school_id), data = toy)
#' hlm_shrinkage_plot(m0)
#' @export
hlm_shrinkage_plot <- function(model, n_clusters = 25,
                               select = c("spread", "random", "all"),
                               style = c("arrows", "size"),
                               title = NULL, subtitle = NULL) {
  if (!inherits(model, "lmerMod")) {
    stop("hlm_shrinkage_plot() currently supports models fitted with lme4::lmer().")
  }
  select <- match.arg(select)
  style  <- match.arg(style)

  re_list <- lme4::ranef(model)
  if (length(re_list) == 0L) {
    stop("The model has no random effects.")
  }
  if (length(re_list) > 1L) {
    warning(
      "The model has more than one grouping factor; only '",
      names(re_list)[1L], "' is displayed.",
      call. = FALSE
    )
  }
  cluster <- names(re_list)[1L]
  re <- re_list[[cluster]]

  if (!"(Intercept)" %in% colnames(re)) {
    stop("The clustering factor must have a random intercept.")
  }

  fe <- lme4::fixef(model)
  if (!"(Intercept)" %in% names(fe)) {
    stop("The model must include a fixed intercept.")
  }
  if (length(fe) > 1L) {
    warning(
      "The model contains fixed effects other than the intercept. Raw cluster ",
      "means and adjusted multilevel intercepts are then not the same ",
      "quantity; fit an unconditional model for the partial-pooling display.",
      call. = FALSE
    )
  }

  overall <- unname(fe["(Intercept)"])

  mf <- tryCatch(stats::model.frame(model), error = function(e) NULL)
  if (is.null(mf) || !cluster %in% names(mf)) {
    stop("The model frame could not be reconstructed from the fitted model.")
  }
  y <- stats::model.response(mf)
  g <- as.character(mf[[cluster]])

  ids <- rownames(re)
  raw_tab  <- tapply(y, g, mean)
  size_tab <- table(g)

  raw_mean    <- as.numeric(raw_tab[ids])
  cluster_n   <- as.numeric(size_tab[ids])
  pooled_mean <- overall + re[["(Intercept)"]]

  # Reliability (shrinkage weight) for each cluster.
  vc <- as.data.frame(lme4::VarCorr(model))
  tau2 <- vc$vcov[vc$grp == cluster & vc$var1 %in% "(Intercept)" &
                    is.na(vc$var2)][1L]
  sigma2 <- stats::sigma(model)^2
  lambda <- if (is.finite(tau2) && tau2 > 0) {
    tau2 / (tau2 + sigma2 / cluster_n)
  } else {
    rep(0, length(cluster_n))
  }

  shrink_df <- data.frame(
    cluster_id  = ids,
    raw_mean    = raw_mean,
    pooled_mean = pooled_mean,
    cluster_n   = cluster_n,
    reliability = lambda,
    stringsAsFactors = FALSE
  )
  shrink_df <- shrink_df[stats::complete.cases(shrink_df), , drop = FALSE]

  n_available <- nrow(shrink_df)
  if (n_available == 0L) {
    stop("No clusters with complete information were found.")
  }

  if (identical(select, "all") || n_available <= n_clusters) {
    keep_index <- seq_len(n_available)
  } else if (identical(select, "spread")) {
    ordered_index <- order(shrink_df$raw_mean)
    keep_index <- ordered_index[
      unique(round(seq(1, n_available, length.out = n_clusters)))
    ]
  } else {
    keep_index <- sample(seq_len(n_available), n_clusters)
  }

  plot_df <- shrink_df[keep_index, , drop = FALSE]
  plot_df <- plot_df[order(plot_df$raw_mean), , drop = FALSE]
  plot_df$cluster_id <- factor(plot_df$cluster_id, levels = plot_df$cluster_id)

  response <- tryCatch(
    deparse(stats::formula(model)[[2L]]),
    error = function(e) "outcome"
  )

  if (is.null(title)) {
    title <- "Multilevel estimates partially pool cluster means"
  }
  if (is.null(subtitle)) {
    subtitle <- sprintf(
      "%d of %d clusters shown; arrows run from the raw mean to the multilevel estimate",
      nrow(plot_df), n_available
    )
  }

  arrow_spec <- grid::arrow(
    length = grid::unit(0.07, "inches"), type = "closed"
  )

  if (identical(style, "arrows")) {
    p <- ggplot2::ggplot(plot_df) +
      ggplot2::geom_vline(
        xintercept = overall, linetype = 2, colour = "#D97706",
        linewidth = 0.7
      ) +
      ggplot2::geom_segment(
        ggplot2::aes(x = raw_mean, xend = pooled_mean,
                     y = cluster_id, yend = cluster_id),
        arrow = arrow_spec, colour = "#6C9ABB", linewidth = 0.7
      ) +
      ggplot2::geom_point(
        ggplot2::aes(x = raw_mean, y = cluster_id, size = cluster_n),
        shape = 21, fill = "white", colour = "#4F5964"
      ) +
      ggplot2::geom_point(
        ggplot2::aes(x = pooled_mean, y = cluster_id),
        colour = "#326E99", size = 2
      ) +
      ggplot2::scale_size_continuous(range = c(1.2, 4)) +
      ggplot2::labs(
        x = paste("Estimated cluster mean of", response),
        y = "Cluster",
        size = "Cluster size",
        title = if (is.na(title)) NULL else title,
        subtitle = if (is.na(subtitle)) NULL else subtitle
      )
  } else {
    p <- ggplot2::ggplot(plot_df) +
      ggplot2::geom_hline(
        yintercept = overall, linetype = 2, colour = "#D97706",
        linewidth = 0.7
      ) +
      ggplot2::geom_segment(
        ggplot2::aes(x = cluster_n, xend = cluster_n,
                     y = raw_mean, yend = pooled_mean),
        arrow = arrow_spec, colour = "#6C9ABB", linewidth = 0.7
      ) +
      ggplot2::geom_point(
        ggplot2::aes(x = cluster_n, y = raw_mean),
        shape = 21, fill = "white", colour = "#4F5964", size = 2
      ) +
      ggplot2::geom_point(
        ggplot2::aes(x = cluster_n, y = pooled_mean),
        colour = "#326E99", size = 2
      ) +
      ggplot2::labs(
        x = "Cluster size",
        y = paste("Estimated cluster mean of", response),
        title = if (is.na(title)) NULL else title,
        subtitle = if (is.na(subtitle)) NULL else subtitle
      )
  }

  p <- p +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())

  attr(p, "data") <- plot_df
  p
}
