# hlmLab 0.2.0

## Correctness

* `hlm_context()` now computes the standard error of the contextual contrast
  from the full fixed-effect covariance matrix,
  `Var(bB - bW) = Var(bB) + Var(bW) - 2 Cov(bB, bW)`, instead of assuming that
  the covariance is zero. In a random-intercept model with an exactly
  group-mean centered Level-1 predictor that covariance is zero by
  construction, so previously published random-intercept results are
  unchanged; it is generally nonzero when the model contains a random slope,
  when the predictor is centered in some other way, or when the Mundlak
  specification uses the raw predictor.
* `hlm_context()` gains `level` and `check_centering` arguments, returns
  `conf_low` and `conf_high` columns, and stores the covariance of the two
  coefficients in the attribute `"cov_within_between"`. A warning is issued
  when `x_within` does not appear to be group-mean centered, because the
  difference between the two coefficients is then not the contextual effect.
* `hlm_icc()` warns when the model has more than one grouping factor, and when
  the first random-effect term contains a random slope, in which case the
  reported ICC is conditional on the Level-1 predictor being zero.

## Terminology

* `hlm_xint_geom()` is deprecated. A random slope is unexplained heterogeneity
  in a Level-1 association; it is not a cross-level interaction, which requires
  an observed Level-2 moderator. The function remains available as an alias
  that warns and forwards to `hlm_random_slope_plot()`, and will be removed in
  a future release.

## New functions

* `hlm_random_slope_plot()` displays cluster-specific fitted lines from a
  random-slope model with the average line overlaid. Clusters are selected
  evenly across the distribution of conditional slopes by default, the
  plotting range is taken from the observed predictor, and remaining fixed
  effects are held at their sample means.
* `hlm_cross_level_plot()` displays the Level-1 association at selected values
  of an observed Level-2 moderator, and warns when the model contains no such
  interaction.
* `hlm_icc_demo()` simulates clustered data at several target intraclass
  correlations with the total variance held constant, so that students can see
  what low, moderate, and high ICC values look like. Clusters are ordered by
  their mean within each panel (`sort_clusters = TRUE`), so the sequence of
  cluster means is nearly flat at a low ICC and steep at a high one.
* `hlm_shrinkage_plot()` compares raw cluster means with multilevel empirical
  Bayes estimates and draws an arrow between them, making partial pooling and
  its dependence on cluster size visible.

## Other changes

* `hlm_icc()` accepts `cluster_size = "auto"`, recovers cluster sizes from the
  fitted model, and reports an unequal-cluster-size design effect based on
  `sum(n^2) / sum(n)` alongside the usual mean-size approximation. The returned
  object gains `deff_unequal`, `n_clusters`, and `mean_cluster_size`.
* `hlm_icc_plot()` labels the components with their percentages and reports the
  unequal-size design effect when available.
* A `testthat` suite was added, including cases in which the within- and
  between-cluster estimates are deliberately correlated.
* Examples for `hlm_decompose()` and `hlm_decompose_long()` are self-contained
  and run without external data.

# hlmLab 0.1.0

* First CRAN release.
