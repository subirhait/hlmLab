fit_cross_level <- function(seed = 303) {
  set.seed(seed)
  n_clusters <- 25
  cluster_size <- 20
  n <- n_clusters * cluster_size
  school_id <- rep(seq_len(n_clusters), each = cluster_size)
  resources <- rep(rnorm(n_clusters), each = cluster_size)
  ses_within <- rnorm(n)
  math <- 50 + 2 * ses_within + 1.5 * resources +
    0.8 * ses_within * resources +
    rep(rnorm(n_clusters, sd = 1), each = cluster_size) + rnorm(n)
  dat <- data.frame(math = math, ses_within = ses_within,
                    resources = resources, school_id = factor(school_id))
  lme4::lmer(math ~ ses_within * resources + (1 | school_id), data = dat)
}

test_that("one line is drawn per moderator value", {
  m <- fit_cross_level()
  p <- hlm_cross_level_plot(m, x_within = "ses_within",
                            moderator = "resources", n_points = 20)
  expect_s3_class(p, "ggplot")
  line_data <- attr(p, "data")
  expect_equal(nlevels(line_data$moderator_label), 3L)
  expect_equal(nrow(line_data), 60L)
})

test_that("lines are not parallel when an interaction is present", {
  m <- fit_cross_level()
  line_data <- attr(
    hlm_cross_level_plot(m, x_within = "ses_within", moderator = "resources"),
    "data"
  )
  slopes <- tapply(seq_len(nrow(line_data)), line_data$moderator_label,
                   function(i) stats::coef(stats::lm(y ~ x, data = line_data[i, ]))[2])
  expect_gt(stats::sd(unlist(slopes)), 0.1)
})

test_that("a model without the interaction warns", {
  set.seed(404)
  n_clusters <- 20
  cluster_size <- 15
  n <- n_clusters * cluster_size
  school_id <- rep(seq_len(n_clusters), each = cluster_size)
  resources <- rep(rnorm(n_clusters), each = cluster_size)
  ses_within <- rnorm(n)
  math <- 50 + 2 * ses_within + resources + rnorm(n)
  dat <- data.frame(math = math, ses_within = ses_within,
                    resources = resources, school_id = factor(school_id))
  m <- lme4::lmer(math ~ ses_within + resources + (1 | school_id), data = dat)

  expect_warning(
    hlm_cross_level_plot(m, x_within = "ses_within", moderator = "resources"),
    "No fixed-effect interaction"
  )
})
