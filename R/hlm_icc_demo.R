#' What different intraclass correlations look like
#'
#' Simulates clustered data at several target intraclass correlations, holding
#' the total variance and the overall mean fixed, and displays them side by
#' side. The figure is a teaching device: it shows that the ICC is a share of
#' variance rather than a test of whether clustering exists, and that the same
#' outcome scale can look very different at different ICC values.
#'
#' For each target value the between-cluster standard deviation is
#' \eqn{\tau = \sigma_{total}\sqrt{ICC}} and the within-cluster standard
#' deviation is \eqn{\sigma = \sigma_{total}\sqrt{1 - ICC}}. With
#' \code{exact = TRUE} the simulated cluster effects and residuals are rescaled
#' so that their realized sample standard deviations match those targets, so
#' each panel is generated at exactly the ICC in its label.
#'
#' That is a statement about the data-generating values, not about what a model
#' fitted to one panel will report. An estimated ICC carries estimation error,
#' and that error is proportionally largest where the ICC is small: with a
#' dozen clusters and a target of 0.05, the between-cluster variance is of the
#' same order as the sampling variance of a cluster mean, and a single fitted
#' panel can easily return 0.03 or 0.08. The panel labels are therefore the
#' values used to generate the data. Fitting a model to the attached data is a
#' useful follow-up exercise, but the estimate should be expected to differ.
#'
#' @param icc Numeric vector of target intraclass correlations, each strictly
#'   between 0 and 1. Defaults to \code{c(0.05, 0.25, 0.60)}.
#' @param n_clusters Number of clusters per panel. Defaults to 12.
#' @param cluster_size Number of observations per cluster. Defaults to 20.
#' @param grand_mean Overall mean of the simulated outcome. Defaults to 50.
#' @param total_sd Total standard deviation of the simulated outcome, held
#'   constant across panels. Defaults to 10.
#' @param exact Logical; rescale the simulated components so that their
#'   realized sample standard deviations match the targets, making the
#'   data-generating ICC exactly the value in each panel label. Defaults to
#'   \code{TRUE}.
#' @param seed Optional integer seed. When supplied, the state of the random
#'   number generator is restored on exit.
#' @param show_cluster_means Logical; draw a horizontal marker at each cluster
#'   mean. Defaults to \code{TRUE}.
#' @param sort_clusters Logical; order the clusters within each panel by their
#'   mean. Defaults to \code{TRUE}, which makes the contrast between panels
#'   legible: the sequence of cluster means is nearly flat at a low ICC and
#'   rises steeply at a high one. Set to \code{FALSE} to keep the simulated
#'   order.
#'
#' @return A ggplot object. The simulated data are attached as the attribute
#'   \code{"data"}.
#'
#' @seealso \code{\link{hlm_icc}}, \code{\link{hlm_icc_plot}}
#'
#' @examples
#' hlm_icc_demo(icc = c(0.05, 0.25, 0.60), n_clusters = 8,
#'              cluster_size = 15, seed = 2026)
#' @export
hlm_icc_demo <- function(icc = c(0.05, 0.25, 0.60),
                         n_clusters = 12, cluster_size = 20,
                         grand_mean = 50, total_sd = 10,
                         exact = TRUE, seed = NULL,
                         show_cluster_means = TRUE,
                         sort_clusters = TRUE) {
  if (!is.numeric(icc) || length(icc) == 0L ||
      any(!is.finite(icc)) || any(icc <= 0) || any(icc >= 1)) {
    stop("'icc' must be numeric with every value strictly between 0 and 1.")
  }
  if (n_clusters < 3L) {
    stop("'n_clusters' must be at least 3.")
  }
  if (cluster_size < 2L) {
    stop("'cluster_size' must be at least 2.")
  }
  if (!is.finite(total_sd) || total_sd <= 0) {
    stop("'total_sd' must be a positive number.")
  }

  if (!is.null(seed)) {
    if (exists(".Random.seed", envir = globalenv())) {
      old_seed <- get(".Random.seed", envir = globalenv())
      on.exit(assign(".Random.seed", old_seed, envir = globalenv()), add = TRUE)
    }
    set.seed(seed)
  }

  n_obs <- n_clusters * cluster_size
  panel_labels <- sprintf("ICC = %.2f", icc)

  standardize <- function(z) {
    s <- stats::sd(z)
    if (!is.finite(s) || s <= 0) return(z - mean(z))
    (z - mean(z)) / s
  }

  panel_list <- lapply(seq_along(icc), function(k) {
    tau   <- total_sd * sqrt(icc[k])
    sigma <- total_sd * sqrt(1 - icc[k])

    u <- stats::rnorm(n_clusters)
    e <- stats::rnorm(n_obs)
    if (isTRUE(exact)) {
      u <- standardize(u)
      e <- standardize(e)
    }

    cluster_effect <- tau * u
    y <- grand_mean + rep(cluster_effect, each = cluster_size) + sigma * e

    data.frame(
      panel_label = rep(panel_labels[k], n_obs),
      cluster_id  = factor(rep(seq_len(n_clusters), each = cluster_size)),
      y           = y,
      stringsAsFactors = FALSE
    )
  })

  sim <- dplyr::bind_rows(panel_list)
  sim$panel_label <- factor(sim$panel_label, levels = panel_labels)

  cluster_means <- stats::aggregate(
    y ~ panel_label + cluster_id,
    data = sim,
    FUN = mean
  )
  names(cluster_means)[names(cluster_means) == "y"] <- "cluster_mean"

  # Position each cluster within its own panel. Ordering by cluster mean makes
  # the comparison across panels legible: a low ICC gives a nearly flat
  # sequence of cluster means, a high ICC a steep one.
  cluster_means <- do.call(rbind, lapply(
    split(cluster_means, cluster_means$panel_label),
    function(d) {
      d$cluster_position <- if (isTRUE(sort_clusters)) {
        rank(d$cluster_mean, ties.method = "first")
      } else {
        as.numeric(as.character(d$cluster_id))
      }
      d
    }
  ))
  rownames(cluster_means) <- NULL

  key <- paste(sim$panel_label, sim$cluster_id, sep = "\r")
  ref <- paste(cluster_means$panel_label, cluster_means$cluster_id, sep = "\r")
  sim$cluster_position <- cluster_means$cluster_position[match(key, ref)]

  p <- ggplot2::ggplot(
    sim,
    ggplot2::aes(x = cluster_position, y = y, group = cluster_position)
  ) +
    ggplot2::geom_point(
      position = ggplot2::position_jitter(width = 0.18, height = 0),
      alpha = 0.35, size = 1.1, colour = "#6C9ABB"
    )

  if (isTRUE(show_cluster_means)) {
    p <- p + ggplot2::geom_point(
      data = cluster_means,
      ggplot2::aes(x = cluster_position, y = cluster_mean),
      inherit.aes = FALSE,
      shape = 95, size = 9, colour = "#1F2937"
    )
  }

  p <- p +
    ggplot2::geom_hline(
      yintercept = grand_mean, linetype = 2, colour = "#D97706", linewidth = 0.7
    ) +
    ggplot2::facet_wrap(~ panel_label, nrow = 1) +
    ggplot2::labs(
      x = if (isTRUE(sort_clusters)) "Cluster (ordered by mean)" else "Cluster",
      y = "Simulated outcome",
      title = "What different ICC values look like",
      subtitle = paste0(
        "Same total variation in every panel; only its location changes. ",
        if (isTRUE(sort_clusters)) {
          "Clusters are ordered by their mean; the dashed line is the overall mean."
        } else {
          "Dashed line is the overall mean."
        }
      )
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank()
    )

  attr(p, "data") <- sim
  p
}
