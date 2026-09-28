#' Cross-level interaction with an observed Level-2 moderator
#'
#' Plots the fitted Level-1 association at selected values of an observed
#' Level-2 moderator. Unlike \code{\link{hlm_random_slope_plot}}, which shows
#' unexplained slope heterogeneity, this display shows moderation by a measured
#' cluster characteristic, which is what the term cross-level interaction
#' denotes.
#'
#' Fitted values are obtained from \code{predict()} with the random effects
#' excluded, so the lines describe the average cluster at each moderator value.
#' Remaining numeric predictors are held at their sample means and remaining
#' categorical predictors at their most frequent level. Variables must appear
#' in the model formula under their own names; inline transformations such as
#' \code{log(x)} are not supported.
#'
#' @param model A fitted \code{lmerMod} model that includes a fixed-effect
#'   interaction between \code{x_within} and \code{moderator}.
#' @param x_within Name of the Level-1 predictor (string).
#' @param moderator Name of the Level-2 moderator (string). May be numeric or
#'   a factor.
#' @param at Optional values of the moderator at which lines are drawn. For a
#'   numeric moderator the default is the mean and one standard deviation on
#'   either side; for a factor the default is every level.
#' @param labels Optional character labels for the moderator values, used in
#'   the legend.
#' @param n_points Number of points along the x-axis. Defaults to 40.
#' @param x_range Optional length-2 numeric vector giving the plotting range of
#'   \code{x_within}. Defaults to the 0.02 and 0.98 quantiles of the observed
#'   predictor.
#' @param title,subtitle Optional plot title and subtitle. \code{NULL} uses
#'   informative defaults; \code{NA} omits the element.
#'
#' @return A ggplot object. The plotted line data are attached as the
#'   attribute \code{"data"}.
#'
#' @seealso \code{\link{hlm_random_slope_plot}}
#'
#' @examples
#' set.seed(7)
#' n_schools <- 20
#' n_students <- 20
#' school_id <- rep(seq_len(n_schools), each = n_students)
#' school_resources <- rep(rnorm(n_schools), each = n_students)
#' SES_c <- rnorm(n_schools * n_students)
#' math_score <- 50 + 2 * SES_c + 1.5 * school_resources +
#'   0.8 * SES_c * school_resources +
#'   rep(rnorm(n_schools, sd = 1), each = n_students) +
#'   rnorm(n_schools * n_students)
#' toy <- data.frame(math_score = math_score, SES_c = SES_c,
#'                   school_resources = school_resources,
#'                   school_id = school_id)
#'
#' m <- lme4::lmer(
#'   math_score ~ SES_c * school_resources + (1 | school_id),
#'   data = toy
#' )
#' hlm_cross_level_plot(m, x_within = "SES_c",
#'                      moderator = "school_resources")
#' @export
hlm_cross_level_plot <- function(model, x_within, moderator,
                                 at = NULL, labels = NULL,
                                 n_points = 40, x_range = NULL,
                                 title = NULL, subtitle = NULL) {
  if (!inherits(model, "lmerMod")) {
    stop("hlm_cross_level_plot() currently supports models fitted with lme4::lmer().")
  }

  mf <- tryCatch(stats::model.frame(model), error = function(e) NULL)
  if (is.null(mf)) {
    stop("The model frame could not be reconstructed from the fitted model.")
  }
  if (!all(c(x_within, moderator) %in% names(mf))) {
    stop(
      "Both 'x_within' and 'moderator' must appear in the model frame under ",
      "their own names. Create transformed variables in the data frame before ",
      "fitting the model."
    )
  }
  if (!is.numeric(mf[[x_within]])) {
    stop("'x_within' must be a numeric Level-1 predictor.")
  }

  fixed_names <- names(lme4::fixef(model))
  has_interaction <- any(vapply(
    strsplit(fixed_names, ":", fixed = TRUE),
    function(parts) {
      length(parts) > 1L &&
        any(grepl(x_within, parts, fixed = TRUE)) &&
        any(grepl(moderator, parts, fixed = TRUE))
    },
    logical(1)
  ))
  if (!has_interaction) {
    warning(
      "No fixed-effect interaction between '", x_within, "' and '", moderator,
      "' was found; the fitted lines will be parallel. Add ", x_within, ":",
      moderator, " to the model to display a cross-level interaction.",
      call. = FALSE
    )
  }

  # Moderator values at which to draw lines.
  mod_values <- at
  mod_var <- mf[[moderator]]
  if (is.null(mod_values)) {
    if (is.numeric(mod_var)) {
      mod_mean <- mean(mod_var, na.rm = TRUE)
      mod_sd   <- stats::sd(mod_var, na.rm = TRUE)
      mod_values <- c(mod_mean - mod_sd, mod_mean, mod_mean + mod_sd)
      if (is.null(labels)) {
        labels <- c("Mean - 1 SD", "Mean", "Mean + 1 SD")
      }
    } else {
      mod_values <- levels(factor(mod_var))
    }
  }
  if (is.null(labels)) {
    labels <- if (is.numeric(mod_values)) {
      sprintf("%s = %.2f", moderator, mod_values)
    } else {
      as.character(mod_values)
    }
  }
  if (length(labels) != length(mod_values)) {
    stop("'labels' must have the same length as the moderator values.")
  }

  # Plotting range for the Level-1 predictor.
  if (is.null(x_range)) {
    x_range <- unname(
      stats::quantile(mf[[x_within]], c(0.02, 0.98), na.rm = TRUE)
    )
  }
  if (length(x_range) != 2L || !all(is.finite(x_range))) {
    stop("'x_range' must be a finite numeric vector of length two.")
  }
  x_grid <- seq(x_range[1L], x_range[2L], length.out = n_points)

  # Template row: numeric variables at their means, others at the modal level.
  template <- mf[1L, , drop = FALSE]
  for (nm in names(mf)) {
    column <- mf[[nm]]
    if (is.numeric(column) && !is.matrix(column)) {
      template[[nm]] <- mean(column, na.rm = TRUE)
    } else if (is.factor(column) || is.character(column)) {
      counts <- table(column)
      modal <- names(counts)[which.max(counts)]
      template[[nm]] <- if (is.factor(column)) {
        factor(modal, levels = levels(column))
      } else {
        modal
      }
    }
  }

  # Keep grouping factors at an observed value; random effects are excluded
  # from the prediction, so only the column type matters.
  group_names <- tryCatch(names(lme4::ranef(model)), error = function(e) character(0))
  for (nm in intersect(group_names, names(mf))) {
    template[[nm]] <- mf[[nm]][1L]
  }

  line_list <- lapply(seq_along(mod_values), function(k) {
    nd <- template[rep(1L, n_points), , drop = FALSE]
    rownames(nd) <- NULL
    nd[[x_within]] <- x_grid
    nd[[moderator]] <- if (is.factor(mf[[moderator]])) {
      factor(mod_values[k], levels = levels(mf[[moderator]]))
    } else {
      mod_values[k]
    }
    data.frame(
      x = x_grid,
      y = as.numeric(stats::predict(model, newdata = nd, re.form = NA)),
      moderator_label = rep(labels[k], n_points),
      stringsAsFactors = FALSE
    )
  })

  line_df <- dplyr::bind_rows(line_list)
  line_df$moderator_label <- factor(line_df$moderator_label, levels = labels)

  response <- tryCatch(
    deparse(stats::formula(model)[[2L]]),
    error = function(e) "outcome"
  )

  if (is.null(title)) {
    title <- sprintf("Cross-level interaction: %s moderates the %s association",
                     moderator, x_within)
  }
  if (is.null(subtitle)) {
    subtitle <- paste(
      "Fitted lines with random effects excluded; other predictors held at",
      "their sample means"
    )
  }

  p <- ggplot2::ggplot(
    line_df,
    ggplot2::aes(x = x, y = y, colour = moderator_label)
  ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_colour_manual(
      values = grDevices::colorRampPalette(
        c("#BCD2DF", "#326E99", "#1F2937")
      )(length(labels))
    ) +
    ggplot2::labs(
      x = x_within,
      y = paste("Predicted", response),
      colour = moderator,
      title = if (is.na(title)) NULL else title,
      subtitle = if (is.na(subtitle)) NULL else subtitle
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())

  attr(p, "data") <- line_df
  p
}
