test_that("simulated panels realize their target ICC values", {
  targets <- c(0.05, 0.25, 0.60)
  p <- hlm_icc_demo(icc = targets, n_clusters = 15, cluster_size = 25,
                    seed = 2026)
  expect_s3_class(p, "ggplot")

  sim <- attr(p, "data")
  expect_equal(nrow(sim), length(targets) * 15L * 25L)

  realized <- vapply(split(sim, sim$panel_label), function(d) {
    fit <- lme4::lmer(y ~ 1 + (1 | cluster_id), data = d, REML = TRUE)
    hlm_icc(fit)$icc
  }, numeric(1))

  expect_true(all(abs(unname(realized) - targets) < 0.08))
  expect_true(all(diff(unname(realized)) > 0))
})

test_that("the total variance is held constant across panels", {
  p <- hlm_icc_demo(icc = c(0.10, 0.50), n_clusters = 20, cluster_size = 30,
                    total_sd = 10, seed = 5)
  sim <- attr(p, "data")
  sds <- vapply(split(sim$y, sim$panel_label), stats::sd, numeric(1))
  expect_true(all(abs(sds - 10) < 1))
})

test_that("the random number stream is restored when a seed is supplied", {
  set.seed(77)
  before <- stats::runif(1)
  set.seed(77)
  invisible(hlm_icc_demo(seed = 2026))
  after <- stats::runif(1)
  expect_equal(before, after)
})

test_that("invalid arguments are rejected", {
  expect_error(hlm_icc_demo(icc = c(0, 0.5)))
  expect_error(hlm_icc_demo(icc = 0.3, n_clusters = 2))
  expect_error(hlm_icc_demo(icc = 0.3, total_sd = -1))
})
