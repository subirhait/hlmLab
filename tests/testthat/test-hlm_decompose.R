test_that("the two-level decomposition reproduces the total variance", {
  dat <- make_toy_two_level()
  dec <- hlm_decompose(dat, var = "math", cluster = "school_id")
  s <- dec$summary

  v_b <- s$variance[s$component == "Between clusters (B)"]
  v_w <- s$variance[s$component == "Within clusters (W)"]
  v_t <- s$variance[s$component == "Total"]

  expect_equal(v_b + v_w, v_t, tolerance = 1e-8)
  expect_equal(sum(s$share[s$component != "Total"]), 1, tolerance = 1e-8)
})

test_that("the descriptive between share is close to the model-based ICC", {
  dat <- make_toy_two_level()
  dec <- hlm_decompose(dat, var = "math", cluster = "school_id")
  between_share <- dec$summary$share[
    dec$summary$component == "Between clusters (B)"
  ]

  m0 <- lme4::lmer(math ~ 1 + (1 | school_id), data = dat, REML = TRUE)
  icc <- hlm_icc(m0)$icc

  expect_lt(abs(between_share - icc), 0.10)
})

test_that("missing columns are reported", {
  dat <- make_toy_two_level()
  expect_error(hlm_decompose(dat, var = "nope", cluster = "school_id"))
})
