#' Teaching plot for intraclass correlation (ICC)
#'
#' Visualizes the intraclass correlation by plotting the between- and
#' within-cluster variance components as a stacked bar. This is intended
#' as a teaching diagram to help students see how the ICC reflects the
#' share of variance that lies between clusters.
#'
#' @param model A fitted \code{lmerMod} model with a random intercept.
#' @param cluster_size Optional scalar giving the average cluster size, or
#'   \code{"auto"} to take cluster sizes from the fitted model; passed to
#'   \code{\link{hlm_icc}} to compute the design effect.
#' @param labels Logical; print the component labels and percentages inside the
#'   bar. Defaults to \code{TRUE}.
#'
#' @return A \code{ggplot} object.
#'
#' @seealso \code{\link{hlm_icc}}, \code{\link{hlm_icc_demo}}
#'
#' @examples
#' set.seed(3)
#' toy <- data.frame(
#'   math_score = rnorm(80, 50, 10),
#'   school_id = rep(seq_len(8), each = 10)
#' )
#' m0 <- lme4::lmer(math_score ~ 1 + (1 | school_id), data = toy)
#' hlm_icc_plot(m0, cluster_size = "auto")
#' @export
hlm_icc_plot <- function(model, cluster_size = NULL, labels = TRUE) {
  ic <- hlm_icc(model, cluster_size = cluster_size)

  df <- dplyr::tibble(
    component = c("Between-cluster variance", "Within-cluster variance"),
    variance  = c(ic$re_var, ic$resid_var)
  )
  df$share <- df$variance / sum(df$variance)

  subtitle_txt <- sprintf("ICC = %.3f", ic$icc)
  if (!is.na(ic$deff)) {
    subtitle_txt <- paste0(
      subtitle_txt,
      sprintf("   |   Design effect (approx.) = %.2f", ic$deff)
    )
  }
  if (!is.null(ic$deff_unequal) && !is.na(ic$deff_unequal)) {
    subtitle_txt <- paste0(
      subtitle_txt,
      sprintf("   |   unequal-size = %.2f", ic$deff_unequal)
    )
  }

  df$component <- factor(df$component, levels = df$component)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = "", y = share, fill = component)) +
    ggplot2::geom_col(width = 0.5)

  if (isTRUE(labels)) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = paste0(component, "\n", sprintf("%.1f%%", 100 * share))),
      position = ggplot2::position_stack(vjust = 0.5),
      colour = c("white", "#1F2937"),
      fontface = "bold",
      size = 4
    )
  }

  p +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = c("#326E99", "#D7E5EE")) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      labels = scales::percent_format(accuracy = 1),
      expand = c(0, 0)
    ) +
    ggplot2::labs(
      x = NULL,
      y = "Share of model-based variance",
      title = "Intraclass correlation (ICC) as variance partitioning",
      subtitle = subtitle_txt,
      fill = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = if (isTRUE(labels)) "none" else "bottom"
    )
}
