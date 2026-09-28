# Shared simulated data sets for the test suite.

make_toy_two_level <- function(n_clusters = 30, cluster_size = 20, seed = 101) {
  set.seed(seed)
  n <- n_clusters * cluster_size
  school_id <- rep(seq_len(n_clusters), each = cluster_size)
  ses_raw <- rnorm(n) + rep(rnorm(n_clusters, sd = 0.8), each = cluster_size)
  ses_between <- rep(tapply(ses_raw, school_id, mean), each = cluster_size)
  ses_within <- ses_raw - ses_between
  u0 <- rep(rnorm(n_clusters, sd = 2), each = cluster_size)
  math <- 50 + 2 * ses_within + 6 * ses_between + u0 + rnorm(n, sd = 4)
  data.frame(
    math = math,
    ses_raw = ses_raw,
    ses_within = ses_within,
    ses_between = ses_between,
    school_id = factor(school_id)
  )
}

make_toy_unbalanced <- function(n_clusters = 25, seed = 202) {
  set.seed(seed)
  sizes <- sample(3:40, n_clusters, replace = TRUE)
  school_id <- rep(seq_len(n_clusters), times = sizes)
  u0 <- rep(rnorm(n_clusters, sd = 5), times = sizes)
  math <- 50 + u0 + rnorm(length(school_id), sd = 10)
  data.frame(math = math, school_id = factor(school_id))
}
