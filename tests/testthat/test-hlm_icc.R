test_that("the ICC matches a direct calculation", {
  dat <- make_toy_two_level()
  m0 <- lme4::lmer(math ~ 1 + (1 | school_id), data = dat, REML = TRUE)

  ic <- hlm_icc(m0, cluster_size = "auto")

  vc <- as.data.frame(lme4::VarCorr(m0))
  tau2 <- vc$vcov[vc$grp == "school_id" & vc$var1 == "(Intercept)"]
  sigma2 <- stats::sigma(m0)^2

  expect_equal(ic$icc, tau2 / (tau2 + sigma2), tolerance = 1e-10)
  expect_equal(ic$n_clusters, 30L)
  expect_equal(ic$mean_cluster_size, 20)
  expect_equal(ic$deff, 1 + (20 - 1) * ic$icc, tolerance = 1e-10)
})

test_that("the unequal-size design effect exceeds the mean-size version", {
  dat <- make_toy_unbalanced()
  m0 <- lme4::lmer(math ~ 1 + (1 | school_id), data = dat, REML = TRUE)

  ic <- hlm_icc(m0, cluster_size = "auto")
  n_j <- as.numeric(table(dat$school_id))
  m_eff <- sum(n_j^2) / sum(n_j)

  expect_equal(ic$deff_unequal, 1 + (m_eff - 1) * ic$icc, tolerance = 1e-10)
  expect_gt(ic$deff_unequal, ic$deff)
})

test_that("a supplied cluster size behaves as before", {
  dat <- make_toy_two_level()
  m0 <- lme4::lmer(math ~ 1 + (1 | school_id), data = dat, REML = TRUE)
  ic <- hlm_icc(m0, cluster_size = 10)
  expect_equal(ic$deff, 1 + 9 * ic$icc, tolerance = 1e-10)
  expect_true(is.na(ic$deff_unequal))
  expect_error(hlm_icc(m0, cluster_size = "mean"))
})

test_that("random-slope models produce a warning about the ICC", {
  dat <- make_toy_two_level()
  m2 <- lme4::lmer(
    math ~ ses_within + (ses_within | school_id),
    data = dat, REML = TRUE,
    control = lme4::lmerControl(optimizer = "bobyqa")
  )
  expect_warning(hlm_icc(m2), "random slope")
})

test_that("the ICC plot is a ggplot", {
  dat <- make_toy_two_level()
  m0 <- lme4::lmer(math ~ 1 + (1 | school_id), data = dat, REML = TRUE)
  expect_s3_class(hlm_icc_plot(m0, cluster_size = "auto"), "ggplot")
})
