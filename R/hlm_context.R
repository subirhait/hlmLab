#' Contextual effect for a Level-1 predictor (Mundlak decomposition)
#'
#' Given a multilevel model containing a within-cluster centered predictor and
#' its cluster-mean counterpart (Mundlak specification), this function extracts
#' the within- and between-cluster effects and computes the contextual effect
#' (between minus within) together with its standard error and a confidence
#' interval.
#'
#' The contextual effect is a linear contrast of two fixed-effect coefficients,
#' so its sampling variance is
#' \deqn{Var(\beta_B - \beta_W) = Var(\beta_B) + Var(\beta_W) -
#'   2 Cov(\beta_B, \beta_W).}
#' Since version 0.2.0 the covariance term is taken from the fixed-effect
#' covariance matrix returned by \code{stats::vcov()} rather than assumed to be
#' zero. In a random-intercept model with an exactly group-mean centered
#' \code{x_within}, that covariance is zero by construction, so results are
#' unchanged; it is generally nonzero when the model contains a random slope,
#' when \code{x_within} is centered in some other way, or when the Mundlak
#' specification uses the raw (uncentered) predictor.
#'
#' Intervals use a normal approximation, which is the usual convention for
#' \code{lme4} fixed effects because no denominator degrees of freedom are
#' supplied.
#'
#' @param model A fitted \code{lmerMod} model.
#' @param x_within Name of the within-cluster centered predictor (string).
#' @param x_between Name of the cluster-mean predictor (string).
#' @param level Confidence level for the reported intervals. Defaults to 0.95.
#' @param check_centering Logical; if \code{TRUE} (the default), warn when
#'   \code{x_within} does not appear to be group-mean centered, because the
#'   difference between the two coefficients is then not the contextual effect.
#'
#' @return An object of class \code{hlm_context}, which is a tibble with:
#'   \item{effect_type}{Within, between, or contextual.}
#'   \item{estimate}{Estimate of the effect.}
#'   \item{se}{Standard error; exact for the contextual contrast.}
#'   \item{conf_low}{Lower confidence limit.}
#'   \item{conf_high}{Upper confidence limit.}
#'   The covariance of the two coefficients is stored in the attribute
#'   \code{"cov_within_between"} and the confidence level in \code{"level"}.
#'
#' @references
#' Mundlak, Y. (1978). On the pooling of time series and cross section data.
#' \emph{Econometrica}, 46, 69-85. \doi{10.2307/1913646}
#'
#' @examples
#' # Build a small toy dataset (no external data needed)
#' set.seed(1)
#' n_schools <- 8
#' n_students <- 12
#' school_id <- rep(seq_len(n_schools), each = n_students)
#' SES_raw <- rnorm(n_schools * n_students)
#' SES_mean <- rep(tapply(SES_raw, school_id, mean), each = n_students)
#' SES_c <- SES_raw - SES_mean
#' math_score <- 50 + 2 * SES_c + 3 * SES_mean +
#'   rep(rnorm(n_schools, sd = 2), each = n_students) +
#'   rnorm(n_schools * n_students)
#' toy <- data.frame(math_score = math_score, SES_c = SES_c,
#'                   SES_mean = SES_mean, school_id = school_id)
#'
#' m <- lme4::lmer(math_score ~ SES_c + SES_mean + (1 | school_id), data = toy)
#' ctx <- hlm_context(m, x_within = "SES_c", x_between = "SES_mean")
#' ctx
#' hlm_context_plot(ctx)
#' @export
hlm_context <- function(model, x_within, x_between, level = 0.95,
                        check_centering = TRUE) {
  if (!inherits(model, "lmerMod")) {
    stop("hlm_context() currently supports models fitted with lme4::lmer().")
  }
  if (!is.numeric(level) || length(level) != 1L ||
      !is.finite(level) || level <= 0 || level >= 1) {
    stop("'level' must be a single number strictly between 0 and 1.")
  }

  b <- lme4::fixef(model)

  if (!all(c(x_within, x_between) %in% names(b))) {
    stop("Both 'x_within' and 'x_between' must be fixed effects in the model.")
  }

  V <- as.matrix(stats::vcov(model))

  bW <- unname(b[x_within])
  bB <- unname(b[x_between])

  var_W  <- V[x_within, x_within]
  var_B  <- V[x_between, x_between]
  cov_WB <- V[x_within, x_between]

  # Exact variance of the linear contrast (between - within).
  var_ctx <- var_B + var_W - 2 * cov_WB
  if (var_ctx < 0) {
    if (var_ctx > -1e-8 * max(var_B, var_W)) {
      var_ctx <- 0
    } else {
      stop(
        "The contextual contrast has a negative estimated variance. ",
        "Inspect the fixed-effect covariance matrix of the model."
      )
    }
  }

  est_ctx <- bB - bW
  se_ctx  <- sqrt(var_ctx)
  se_W    <- sqrt(var_W)
  se_B    <- sqrt(var_B)

  crit <- stats::qnorm(1 - (1 - level) / 2)

  estimate <- c(bW, bB, est_ctx)
  se       <- c(se_W, se_B, se_ctx)

  out <- dplyr::tibble(
    effect_type = c("Within-cluster", "Between-cluster", "Contextual (B - W)"),
    estimate    = estimate,
    se          = se,
    conf_low    = estimate - crit * se,
    conf_high   = estimate + crit * se
  )

  class(out) <- c("hlm_context", class(out))
  attr(out, "cov_within_between") <- cov_WB
  attr(out, "level") <- level
  attr(out, "x_within") <- x_within
  attr(out, "x_between") <- x_between

  if (isTRUE(check_centering)) {
    msg <- hlm_centering_message(model, x_within, x_between)
    if (!is.null(msg)) warning(msg, call. = FALSE)
  }

  out
}

# Internal: returns a warning message when 'x_within' does not look like a
# group-mean centered variable, and NULL otherwise.
hlm_centering_message <- function(model, x_within, x_between) {
  mf <- tryCatch(stats::model.frame(model), error = function(e) NULL)
  if (is.null(mf) || !x_within %in% names(mf)) return(NULL)

  groups <- tryCatch(names(lme4::ranef(model)), error = function(e) character(0))
  groups <- groups[groups %in% names(mf)]
  if (length(groups) == 0L) return(NULL)

  x <- mf[[x_within]]
  if (!is.numeric(x)) return(NULL)

  s <- stats::sd(x, na.rm = TRUE)
  if (!is.finite(s) || s <= 0) return(NULL)

  cluster_means <- tapply(x, mf[[groups[1L]]], mean, na.rm = TRUE)
  max_dev <- max(abs(cluster_means), na.rm = TRUE)
  if (!is.finite(max_dev) || max_dev <= 1e-6 * s) return(NULL)

  paste0(
    "'", x_within, "' does not appear to be group-mean centered ",
    "(largest cluster mean = ", format(max_dev, digits = 3), "). ",
    "With an uncentered Level-1 predictor the coefficient on '", x_between,
    "' is already the contextual effect, and the reported difference ",
    "between the two coefficients is not. Set check_centering = FALSE to ",
    "suppress this message."
  )
}

#' @export
print.hlm_context <- function(x, ...) {
  cat("Contextual effect decomposition (Mundlak)\n")
  print.data.frame(as.data.frame(x), row.names = FALSE)

  cov_wb <- attr(x, "cov_within_between")
  lev    <- attr(x, "level")
  if (!is.null(lev)) {
    cat(sprintf("\nIntervals: %.0f%% normal approximation\n", 100 * lev))
  }
  if (!is.null(cov_wb)) {
    cat(sprintf(
      "Cov(within, between) = %s; contextual SE uses the full fixed-effect covariance matrix.\n",
      format(cov_wb, digits = 3)
    ))
  }
  invisible(x)
}

#' Plot method for hlm_context objects
#'
#' Produces an error-bar plot of within, between, and contextual effects with
#' confidence intervals. Intended as a teaching diagram.
#'
#' @param x An object of class \code{hlm_context}.
#' @param ... Not used.
#'
#' @return A ggplot object.
#' @method plot hlm_context
#' @export
plot.hlm_context <- function(x, ...) {
  df <- as.data.frame(x)

  if (!all(c("conf_low", "conf_high") %in% names(df))) {
    df$conf_low  <- df$estimate - 1.96 * df$se
    df$conf_high <- df$estimate + 1.96 * df$se
  }

  df$effect_type <- factor(
    df$effect_type,
    levels = c("Between-cluster", "Contextual (B - W)", "Within-cluster")
  )

  lev <- attr(x, "level")
  subtitle_txt <- if (is.null(lev)) {
    "Points are estimates; bars are confidence intervals"
  } else {
    sprintf("Points are estimates; bars are %.0f%% confidence intervals",
            100 * lev)
  }

  ggplot2::ggplot(df, ggplot2::aes(x = effect_type, y = estimate)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                        colour = "#6B7280") +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = conf_low, ymax = conf_high),
      width = 0.1,
      linewidth = 0.8,
      colour = "#326E99"
    ) +
    ggplot2::geom_point(size = 3, colour = "#326E99") +
    ggplot2::labs(
      x = NULL,
      y = "Estimated effect",
      title = "Within, between, and contextual effects",
      subtitle = subtitle_txt
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
}

#' Convenience wrapper to plot contextual effects
#'
#' @param object An object of class \code{hlm_context}.
#'
#' @return A ggplot object.
#'
#' @examples
#' set.seed(1)
#' school_id <- rep(seq_len(8), each = 12)
#' SES_raw <- rnorm(96)
#' SES_mean <- rep(tapply(SES_raw, school_id, mean), each = 12)
#' SES_c <- SES_raw - SES_mean
#' math_score <- 50 + 2 * SES_c + 3 * SES_mean +
#'   rep(rnorm(8, sd = 2), each = 12) + rnorm(96)
#' toy <- data.frame(math_score = math_score, SES_c = SES_c,
#'                   SES_mean = SES_mean, school_id = school_id)
#' m <- lme4::lmer(math_score ~ SES_c + SES_mean + (1 | school_id), data = toy)
#' hlm_context_plot(hlm_context(m, "SES_c", "SES_mean"))
#' @export
hlm_context_plot <- function(object) {
  plot(object)
}
