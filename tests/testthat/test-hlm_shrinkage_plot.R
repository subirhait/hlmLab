test_that("pooled estimates lie between the raw mean and the overall mean", {
  dat <- make_toy_unbalanced()
  m0 <- lme4::lmer(math ~ 1 + (1 | school_id), data = dat, REML = TRUE)

  p <- hlm_shrinkage_plot(m0, select = "all")
  expect_s3_class(p, "ggplot")

  shrink <- attr(p, "data")
  overall <- unname(lme4::fixef(m0)["(Intercept)"])

  moved_toward <- abs(shrink$pooled_mean - overall) <=
    abs(shrink$raw_mean - overall) + 1e-8
  expect_true(all(moved_toward))
})

test_that("small clusters shrink proportionally more than large clusters", {
  dat <- make_toy_unbalanced()
  m0 <- lme4::lmer(math ~ 1 + (1 | school_id), data = dat, REML = TRUE)
  shrink <- attr(hlm_shrinkage_plot(m0, select = "all"), "data")

  # Reliability is strictly increasing in cluster size.
  expect_equal(
    stats::cor(shrink$cluster_n, shrink$reliability, method = "spearman"),
    1,
    tolerance = 1e-8
  )

  # Reliability matches the closed-form shrinkage weight.
  vc <- as.data.frame(lme4::VarCorr(m0))
  tau2 <- vc$vcov[vc$grp == "school_id" & vc$var1 == "(Intercept)"]
  sigma2 <- stats::sigma(m0)^2
  expect_equal(
    shrink$reliability,
    tau2 / (tau2 + sigma2 / shrink$cluster_n),
    tolerance = 1e-8
  )

  # The empirical Bayes estimate is the reliability-weighted average.
  overall <- unname(lme4::fixef(m0)["(Intercept)"])
  expect_equal(
    shrink$pooled_mean,
    shrink$reliability * shrink$raw_mean + (1 - shrink$reliability) * overall,
    tolerance = 1e-6
  )
})

test_that("conditional models warn", {
  dat <- make_toy_two_level()
  m1 <- lme4::lmer(math ~ ses_within + (1 | school_id), data = dat)
  expect_warning(hlm_shrinkage_plot(m1), "unconditional")
})

test_that("both display styles return a ggplot", {
  dat <- make_toy_unbalanced()
  m0 <- lme4::lmer(math ~ 1 + (1 | school_id), data = dat, REML = TRUE)
  expect_s3_class(hlm_shrinkage_plot(m0, style = "arrows"), "ggplot")
  expect_s3_class(hlm_shrinkage_plot(m0, style = "size"), "ggplot")
  expect_lte(nrow(attr(hlm_shrinkage_plot(m0, n_clusters = 10), "data")), 10L)
})
