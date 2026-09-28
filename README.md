# hlmLab <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/hlmLab)](https://CRAN.R-project.org/package=hlmLab)
[![R-CMD-check](https://github.com/subirhait/hlmLab/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/subirhait/hlmLab/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

**hlmLab** provides tools for visualization and decomposition in hierarchical linear models (HLM), designed for researchers and students in education, psychology, and the social sciences. It offers a coherent set of functions for understanding how variance is distributed across levels, how predictors operate within and between clusters, and how random slopes vary across groups — all built on top of `lme4`.

---

## Installation

Install the released version from CRAN:

```r
install.packages("hlmLab")
```

Or install the development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("subirhait/hlmLab")
```

---

## Overview of Functions

| Function | What it does |
|---|---|
| `hlm_decompose()` | Decomposes variance into within- and between-cluster components (2-level or 3-level) |
| `hlm_decompose_long()` | Convenience wrapper for 3-level longitudinal B-P-W decomposition |
| `hlm_icc()` | Computes the intraclass correlation (ICC) and design effect from a fitted model |
| `hlm_icc_plot()` | Visualizes ICC as a stacked variance-partitioning bar chart |
| `hlm_context()` | Extracts within-cluster, between-cluster, and contextual effects (Mundlak specification) |
| `hlm_context_plot()` | Plots within, between, and contextual effects with 95% confidence intervals |
| `hlm_random_slope_plot()` | Draws cluster-specific fitted lines from a random-slope model, with the average line overlaid |
| `hlm_cross_level_plot()` | Draws the Level-1 association at selected values of an observed Level-2 moderator |
| `hlm_icc_demo()` | Simulates clustered data at several target ICC values to show what low, moderate, and high clustering look like |
| `hlm_shrinkage_plot()` | Compares raw cluster means with multilevel estimates to make partial pooling visible |
| `hlm_xint_geom()` | Deprecated in 0.2.0; alias for `hlm_random_slope_plot()` |

---

## What changed in 0.2.0

* `hlm_context()` computes the standard error of the contextual contrast from
  the full fixed-effect covariance matrix,
  `Var(bB - bW) = Var(bB) + Var(bW) - 2 Cov(bB, bW)`. Under exact group-mean
  centering in a random-intercept model that covariance is zero by
  construction, so earlier random-intercept results are unchanged; it is
  generally nonzero with random slopes, other centering choices, or the raw
  Mundlak parameterization.
* `hlm_xint_geom()` is deprecated. A random slope is unexplained slope
  heterogeneity, not a cross-level interaction, so the display was renamed
  `hlm_random_slope_plot()` and a separate `hlm_cross_level_plot()` was added
  for models with an observed Level-2 moderator.
* Two teaching figures were added: `hlm_icc_demo()` and `hlm_shrinkage_plot()`.
* Plotting functions no longer call `set.seed()` internally.

See `NEWS.md` for the full list.

---

## Usage

### 1. Variance Decomposition

`hlm_decompose()` partitions the total variance of a continuous variable into between-cluster and within-cluster components without fitting a model — useful as a first diagnostic step.

```r
library(hlmLab)

# 2-level: students nested in schools
result <- hlm_decompose(data = mydata,
                        var     = "math_score",
                        cluster = "school_id")
result
#> HLM variance decomposition for: math_score
#> # A tibble: 3 × 3
#>   component            variance share
#>   <chr>                   <dbl> <dbl>
#> 1 Between clusters (B)     42.1 0.312
#> 2 Within clusters (W)      92.8 0.688
#> 3 Total                   134.9 1.000

plot(result)
```

For 3-level longitudinal data (students measured repeatedly within schools):

```r
result_long <- hlm_decompose_long(data    = mydata_long,
                                  var     = "math_score",
                                  cluster = "school_id",
                                  id      = "student_id",
                                  time    = "wave")
plot(result_long)
```

The plot shows a bar chart of variance shares across Between-cluster (B), Between-person (P), and Within-person (W) components.

---

### 2. Intraclass Correlation (ICC) and Design Effect

`hlm_icc()` computes the ICC from a fitted `lme4` random-intercept model. Supplying `cluster_size` also returns the design effect, which quantifies how much clustering inflates standard errors relative to simple random sampling.

```r
library(lme4)

m0 <- lmer(math_score ~ 1 + (1 | school_id), data = mydata)

hlm_icc(m0, cluster_size = 25)
#> Intraclass correlation (ICC) and design effect
#>   ICC           : 0.312
#>   RE variance   : 42.1
#>   Residual var. : 92.8
#>   Design effect : 8.48
```

Visualize the ICC as a variance-partitioning diagram:

```r
hlm_icc_plot(m0, cluster_size = 25)
```

The plot displays a horizontal stacked bar with between- and within-cluster variance shares, with the ICC and design effect shown in the subtitle.

---

### 3. Contextual Effect Decomposition (Mundlak Specification)

`hlm_context()` separates the total effect of a Level-1 predictor into its within-cluster component (the pure individual-level effect) and its between-cluster component (the group-level effect). The contextual effect is their difference (between − within), following Mundlak (1978).

The model must include both the within-cluster centered predictor and the cluster mean:

```r
# Center SES within schools and compute school means
mydata$SES_c    <- mydata$SES - ave(mydata$SES, mydata$school_id)
mydata$SES_mean <- ave(mydata$SES, mydata$school_id)

m1 <- lmer(math_score ~ SES_c + SES_mean + (1 | school_id),
           data = mydata)

ctx <- hlm_context(m1,
                   x_within  = "SES_c",
                   x_between = "SES_mean")
ctx
#> Contextual effect decomposition
#>          effect_type estimate    se
#>       Within-cluster     2.31  0.18
#>      Between-cluster     5.84  0.62
#>   Contextual (B - W)     3.53  0.65
```

Plot the three effects with 95% confidence intervals:

```r
hlm_context_plot(ctx)
# or equivalently:
plot(ctx)
```

---

### 4. Random-Slope Heterogeneity

`hlm_random_slope_plot()` visualizes how a Level-1 association varies across clusters: each line is one cluster's predicted regression of the outcome on the Level-1 predictor, and the orange line is the average. Spread across the lines is unexplained slope heterogeneity. It is not a cross-level interaction, which requires an observed Level-2 moderator; use `hlm_cross_level_plot()` for that case.

```r
m2 <- lmer(math_score ~ SES_c + SES_mean + (SES_c | school_id),
           data = mydata)

hlm_random_slope_plot(m2,
                      x_within   = "SES_c",
                      cluster    = "school_id",
                      n_clusters = 20)
```

Use `n_clusters` to limit the number of lines displayed when you have many groups, and `select` to choose whether those clusters are spread across the slope distribution (the default) or sampled at random.

---

### 5. Partial Pooling and the Meaning of the ICC

```r
m0 <- lmer(math_score ~ 1 + (1 | school_id), data = mydata)

hlm_shrinkage_plot(m0)                       # raw means vs. multilevel estimates
hlm_icc_demo(icc = c(0.05, 0.25, 0.60))      # what low, moderate, high ICC look like
```

---

## Interactive Shiny application

A companion teaching application is included with the package.
It provides interactive two-level and three-level longitudinal demonstrations.

```r
shiny::runApp(system.file("app", package = "hlmLab"))
```

## Theoretical Background

hlmLab implements methods from the following foundational references:

- **Variance decomposition and ICC:** Snijders & Bosker (2012). *Multilevel Analysis*. SAGE. ISBN: 9781849202015
- **ICC and design effect:** Shrout & Fleiss (1979). *Psychological Bulletin*, 86(2), 420–428. [doi:10.1037/0033-2909.86.2.420](https://doi.org/10.1037/0033-2909.86.2.420)
- **Contextual effects / Mundlak specification:** Mundlak (1978). *Econometrica*, 46(1), 69–85. [doi:10.2307/1913646](https://doi.org/10.2307/1913646)
- **Random slopes and cross-level interactions:** Hofmann & Gavin (1998). *Journal of Management*, 24(5), 623–641. [doi:10.1177/014920639802400504](https://doi.org/10.1177/014920639802400504)
- **Cross-level interaction visualization:** Hamaker & Muthen (2020). *Psychological Methods*, 25(2), 157–173. [doi:10.1037/met0000239](https://doi.org/10.1037/met0000239)
- **Model estimation via lme4:** Bates et al. (2015). *Journal of Statistical Software*, 67(1), 1–48. [doi:10.18637/jss.v067.i01](https://doi.org/10.18637/jss.v067.i01)
- **General HLM framework:** Raudenbush & Bryk (2002). *Hierarchical Linear Models*. SAGE. ISBN: 9780761919049

---

## Citation

If you use hlmLab in your research, please cite it:

```r
citation("hlmLab")
```

```
Hait S (2026). hlmLab: Hierarchical Linear Modeling with Visualization
and Decomposition. R package version 0.2.0.
https://doi.org/10.32614/CRAN.package.hlmLab | Source: https://github.com/subirhait/hlmLab
```

---

## Contributing

Bug reports and feature requests are welcome at the [issue tracker](https://github.com/subirhait/hlmLab/issues). Please include a minimal reproducible example with any bug report.

---

## License

MIT © Subir Hait
