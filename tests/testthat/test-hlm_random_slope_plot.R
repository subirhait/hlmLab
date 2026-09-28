fit_random_slope <- function() {
  dat <- make_toy_two_level()
  lme4::lmer(
    math ~ ses_within + ses_between + (ses_within | school_id),
    data = dat, REML = TRUE,
    control = lme4::lmerControl(optimizer = "bobyqa")
  )
}

test_that("one line is drawn per selected cluster", {
  m2 <- fit_random_slope()
  p <- hlm_random_slope_plot(m2, x_within = "ses_within",
                             cluster = "school_id",
                             n_points = 25, n_clusters = 12)
  expect_s3_class(p, "ggplot")

  line_data <- attr(p, "data")
  expect_equal(length(unique(line_data$cluster_id)), 12L)
  expect_equal(nrow(line_data), 12L * 25L)
})

test_that("select = 'all' shows every cluster", {
  m2 <- fit_random_slope()
  p <- hlm_random_slope_plot(m2, x_within = "ses_within",
                             cluster = "school_id", select = "all")
  expect_equal(length(unique(attr(p, "data")$cluster_id)), 30L)
})

test_that("the function does not disturb the random number stream", {
  m2 <- fit_random_slope()
  set.seed(999)
  before <- stats::runif(1)
  set.seed(999)
  invisible(hlm_random_slope_plot(m2, x_within = "ses_within",
                                  cluster = "school_id"))
  after <- stats::runif(1)
  expect_equal(before, after)
})

test_that("a missing random slope is an error", {
  dat <- make_toy_two_level()
  m1 <- lme4::lmer(math ~ ses_within + (1 | school_id), data = dat)
  expect_error(
    hlm_random_slope_plot(m1, x_within = "ses_within", cluster = "school_id"),
    "No random slope"
  )
})

test_that("hlm_xint_geom is deprecated but still works", {
  m2 <- fit_random_slope()
  expect_warning(
    p <- hlm_xint_geom(m2, x_within = "ses_within", cluster = "school_id"),
    "deprecated"
  )
  expect_s3_class(p, "ggplot")
})
