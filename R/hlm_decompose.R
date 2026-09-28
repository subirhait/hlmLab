#' Multilevel variance decomposition (within/between/longitudinal)
#'
#' Decomposes a continuous variable into between-cluster, and within-cluster
#' components (2-level), or into between-cluster (B), between-person (P),
#' and within-person (W) components for longitudinal 3-level data.
#'
#' This is primarily a teaching tool: it shows how total variance is
#' partitioned across levels.
#'
#' @param data A data frame.
#' @param var Name of the focal variable (string).
#' @param cluster Name of the cluster ID variable (e.g., school/classroom).
#' @param id Optional person ID variable for longitudinal data.
#' @param time Optional time variable (not required for the algebra, but
#'   noted in the output for clarity).
#'
#' @return An object of class \code{hlm_decompose} with components:
#'   \item{data}{Original data with added decomposition columns.}
#'   \item{summary}{A tibble summarizing variance components and shares.}
#'
#' @examples
#' # 2-level example (students in schools)
#' set.seed(2)
#' toy <- data.frame(
#'   math_score = rnorm(80, mean = 50, sd = 10),
#'   school_id = rep(letters[1:8], each = 10)
#' )
#' hlm_decompose(toy, var = "math_score", cluster = "school_id")
#'
#' # 3-level longitudinal example (waves within students within schools)
#' toy3 <- data.frame(
#'   math_score = rnorm(120, 50, 10),
#'   school_id = rep(letters[1:4], each = 30),
#'   student_id = rep(seq_len(24), each = 5)
#' )
#' hlm_decompose(toy3, var = "math_score", cluster = "school_id",
#'               id = "student_id", time = NULL)
#' @export
hlm_decompose <- function(data, var, cluster, id = NULL, time = NULL) {
  stopifnot(is.data.frame(data))
  if (!all(c(var, cluster) %in% names(data))) {
    stop("Both 'var' and 'cluster' must be columns in 'data'.")
  }

  df <- data
  x  <- df[[var]]

  # 2-level: within/between cluster only
  if (is.null(id)) {
    cl_means <- tapply(x, df[[cluster]], mean, na.rm = TRUE)
    df[[paste0(var, "_B")]] <- cl_means[match(df[[cluster]], names(cl_means))]
    df[[paste0(var, "_W")]] <- x - df[[paste0(var, "_B")]]

    v_total <- stats::var(x, na.rm = TRUE)
    v_B     <- stats::var(df[[paste0(var, "_B")]], na.rm = TRUE)
    v_W     <- stats::var(df[[paste0(var, "_W")]], na.rm = TRUE)

    summary_tbl <- dplyr::tibble(
      component = c("Between clusters (B)", "Within clusters (W)", "Total"),
      variance  = c(v_B, v_W, v_total),
      share     = c(v_B / v_total, v_W / v_total, 1)
    )

  } else {
    # 3-level: cluster (B), person within cluster (P), within person over time (W)
    if (!id %in% names(df)) {
      stop("'id' must be a column in 'data' when supplied.")
    }

    # Cluster means
    cl_means <- tapply(x, df[[cluster]], mean, na.rm = TRUE)
    df[[paste0(var, "_B")]] <- cl_means[match(df[[cluster]], names(cl_means))]

    # Person means (within cluster)
    combo_id <- interaction(df[[cluster]], df[[id]], drop = TRUE)
    person_means <- tapply(x, combo_id, mean, na.rm = TRUE)
    df[[paste0(var, "_P")]] <- person_means[match(combo_id, names(person_means))]

    # Within-person deviations
    df[[paste0(var, "_W")]] <- x - df[[paste0(var, "_P")]]

    # Person-level deviation from cluster mean (P component)
    df[[paste0(var, "_Pdev")]] <- df[[paste0(var, "_P")]] - df[[paste0(var, "_B")]]

    v_total <- stats::var(x, na.rm = TRUE)
    v_B     <- stats::var(df[[paste0(var, "_B")]], na.rm = TRUE)
    v_P     <- stats::var(df[[paste0(var, "_Pdev")]], na.rm = TRUE)
    v_W     <- stats::var(df[[paste0(var, "_W")]], na.rm = TRUE)

    summary_tbl <- dplyr::tibble(
      component = c("Between clusters (B)",
                    "Between persons within clusters (P)",
                    "Within persons over time (W)",
                    "Total"),
      variance  = c(v_B, v_P, v_W, v_total),
      share     = c(v_B, v_P, v_W, v_total) / v_total
    )
  }

  out <- list(
    data    = df,
    summary = summary_tbl,
    var     = var,
    cluster = cluster,
    id      = id,
    time    = time
  )
  class(out) <- c("hlm_decompose", class(out))
  out
}

#' @export
print.hlm_decompose <- function(x, ...) {
  cat("HLM variance decomposition for:", x$var, "\n")
  print(x$summary, n = nrow(x$summary))
  invisible(x)
}
#' Plot method for hlm_decompose objects
#'
#' Produces a simple bar chart of variance shares across components,
#' suitable for teaching how variance is partitioned across levels.
#'
#' @param x An object of class \code{hlm_decompose}.
#' @param ... Not used.
#'
#' @return A ggplot object.
#' @export
plot.hlm_decompose <- function(x, ...) {
  df <- x$summary
  # ensure ordering: B, P, W, Total
  if ("component" %in% names(df)) {
    df$component <- factor(df$component, levels = df$component)
  }

  ggplot2::ggplot(df, ggplot2::aes(x = component, y = share)) +
    ggplot2::geom_col() +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::labs(
      x = NULL,
      y = "Share of total variance",
      title = paste("Variance partitioning for", x$var)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, hjust = 1))
}

