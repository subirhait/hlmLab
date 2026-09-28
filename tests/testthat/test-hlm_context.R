test_that("group-mean centering makes the within-between covariance zero", {
  dat <- make_toy_two_level()
  m1 <- lme4::lmer(
    math ~ ses_within + ses_between + (1 | school_id),
    data = dat, REML = TRUE
  )

  ctx <- hlm_context(m1, x_within = "ses_within", x_between = "ses_between")
  cov_wb <- attr(ctx, "cov_within_between")

  V <- as.matrix(stats::vcov(m1))
  scale_ref <- sqrt(V["ses_within", "ses_within"] *
                      V["ses_between", "ses_between"])

  # Zero by construction, not by numerical accident.
  expect_lt(abs(cov_wb), 1e-8 * scale_ref)

  # With zero covariance the old and new formulas agree.
  naive <- sqrt(V["ses_within", "ses_within"] + V["ses_between", "ses_between"])
  expect_equal(ctx$se[3], naive, tolerance = 1e-6)
})

test_that("the contextual SE equals the exact linear-contrast SE", {
  dat <- make_toy_two_level()

  # A random slope makes Cov(within, between) nonzero.
  m2 <- lme4::lmer(
    math ~ ses_within + ses_between + (ses_within | school_id),
    data = dat, REML = TRUE,
    control = lme4::lmerControl(optimizer = "bobyqa")
  )

  ctx <- hlm_context(m2, x_within = "ses_within", x_between = "ses_between")

  b <- lme4::fixef(m2)
  V <- as.matrix(stats::vcov(m2))
  L <- rep(0, length(b))
  names(L) <- names(b)
  L["ses_within"] <- -1
  L["ses_between"] <- 1

  expect_equal(ctx$estimate[3], as.numeric(L %*% b), tolerance = 1e-10)
  expect_equal(ctx$se[3], sqrt(as.numeric(L %*% V %*% L)), tolerance = 1e-10)
})

test_that("a nonzero covariance changes the contextual SE", {
  dat <- make_toy_two_level()

  # Mundlak specification with the raw predictor: the two estimates are
  # strongly correlated, so omitting the covariance is materially wrong.
  m_raw <- lme4::lmer(
    math ~ ses_raw + ses_between + (1 | school_id),
    data = dat, REML = TRUE
  )

  ctx <- suppressWarnings(
    hlm_context(m_raw, x_within = "ses_raw", x_between = "ses_between")
  )

  V <- as.matrix(stats::vcov(m_raw))
  cov_wb <- V["ses_raw", "ses_between"]
  naive <- sqrt(V["ses_raw", "ses_raw"] + V["ses_between", "ses_between"])
  exact <- sqrt(V["ses_raw", "ses_raw"] + V["ses_between", "ses_between"] -
                  2 * cov_wb)

  expect_gt(abs(cov_wb), 0)
  expect_equal(ctx$se[3], exact, tolerance = 1e-10)
  expect_false(isTRUE(all.equal(exact, naive, tolerance = 1e-6)))
})

test_that("uncentered Level-1 predictors trigger a centering warning", {
  dat <- make_toy_two_level()
  m_raw <- lme4::lmer(
    math ~ ses_raw + ses_between + (1 | school_id),
    data = dat, REML = TRUE
  )
  expect_warning(
    hlm_context(m_raw, x_within = "ses_raw", x_between = "ses_between"),
    "group-mean centered"
  )
  expect_silent(
    hlm_context(m_raw, x_within = "ses_raw", x_between = "ses_between",
                check_centering = FALSE)
  )
})

test_that("confidence limits follow the requested level", {
  dat <- make_toy_two_level()
  m1 <- lme4::lmer(
    math ~ ses_within + ses_between + (1 | school_id),
    data = dat, REML = TRUE
  )
  ctx90 <- hlm_context(m1, "ses_within", "ses_between", level = 0.90)
  crit <- stats::qnorm(0.95)
  expect_equal(ctx90$conf_low, ctx90$estimate - crit * ctx90$se,
               tolerance = 1e-10)
  expect_equal(attr(ctx90, "level"), 0.90)
  expect_error(hlm_context(m1, "ses_within", "ses_between", level = 1.2))
})

test_that("missing fixed effects are reported", {
  dat <- make_toy_two_level()
  m1 <- lme4::lmer(
    math ~ ses_within + ses_between + (1 | school_id),
    data = dat, REML = TRUE
  )
  expect_error(hlm_context(m1, "not_a_variable", "ses_between"),
               "must be fixed effects")
})

test_that("plot method returns a ggplot", {
  dat <- make_toy_two_level()
  m1 <- lme4::lmer(
    math ~ ses_within + ses_between + (1 | school_id),
    data = dat, REML = TRUE
  )
  ctx <- hlm_context(m1, "ses_within", "ses_between")
  expect_s3_class(plot(ctx), "ggplot")
  expect_s3_class(hlm_context_plot(ctx), "ggplot")
})
