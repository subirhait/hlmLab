## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* Local: Windows 11 x64, R 4.6.0
* win-builder: R-devel and R-release
* mac-builder: R-release (macOS)

## Submission notes

* This is an update of hlmLab 0.1.0.
* It corrects the standard error of the contextual contrast in
  `hlm_context()`, which previously omitted the covariance between the
  within- and between-cluster coefficients. Results for random-intercept
  models with an exactly group-mean centered predictor are unchanged,
  because that covariance is zero by construction.
* The random-slope display, previously labelled a cross-level interaction,
  is renamed. `hlm_xint_geom()` is retained as a deprecated alias that
  warns and forwards to the new `hlm_random_slope_plot()`.
* New functions: `hlm_cross_level_plot()`, `hlm_icc_demo()`, and
  `hlm_shrinkage_plot()`.
* Examples are now self-contained and run without user-supplied data.
* A `testthat` suite has been added.
* The package has no vignettes at this time.
* The GitHub repository moved to https://github.com/subirhait/hlmLab;
  URLs in DESCRIPTION and README have been updated.

## Reverse dependencies

There are currently no reverse dependencies.
