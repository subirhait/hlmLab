# =============================================================================
# 02_run_hlmlab_examples.R
#
# Purpose : Reproduce all hlmLab analyses reported in the manuscript.
#           Sections correspond to Section 5 of the paper.
#
# Requires: d_cross and d_clean produced by 01_build_teaching_data.R
#           hlmLab >= 0.1.0, lme4, dplyr
#
# Authors : [ANONYMOUS FOR REVIEW]
# Date    : 2025
# =============================================================================

library(hlmLab)
library(lme4)
library(dplyr)

# -----------------------------------------------------------------------------
# Load processed data
# -----------------------------------------------------------------------------
d_cross <- readRDS("scripts/data_processed/d_cross.rds")
d_clean <- readRDS("scripts/data_processed/d_clean.rds")

cat("Cross-sectional N =", nrow(d_cross), "| Schools =",
    nlevels(droplevels(d_cross$schid)), "\n")
cat("Longitudinal  N =", nrow(d_clean), "\n\n")

# =============================================================================
# Section 5.1 -- Two-Level Variance Decomposition (Table 3, Figure 1)
# =============================================================================
cat("=== 5.1 Two-Level Variance Decomposition ===\n")

dec <- hlm_decompose(
  data    = d_cross,
  var     = "math_score",
  cluster = "schid"
)
print(dec)

# Figure 1
fig1 <- plot(dec)
print(fig1)

# Numbers reported in Table 3
cat("\n--- Table 3 values ---\n")
cat(sprintf("Between schools : %s (%.1f%%)\n",
    format(round(dec$summary$variance[1]), big.mark=","),
    dec$summary$share[1] * 100))
cat(sprintf("Within schools  : %s (%.1f%%)\n",
    format(round(dec$summary$variance[2]), big.mark=","),
    dec$summary$share[2] * 100))
cat(sprintf("Total           : %s (100.0%%)\n",
    format(round(dec$summary$variance[3]), big.mark=",")))

# =============================================================================
# Section 5.2 -- ICC and Design Effect (Table 4, Figure 2)
# =============================================================================
cat("\n=== 5.2 Intraclass Correlation and Design Effect ===\n")

m0 <- lmer(math_score ~ 1 + (1 | schid), data = d_cross)

icc_result <- hlm_icc(m0, cluster_size = 4)
print(icc_result)

# Figure 2
fig2 <- hlm_icc_plot(m0, cluster_size = 4)
print(fig2)

# Numbers reported in Table 4
cat("\n--- Table 4 values ---\n")
cat(sprintf("Between-school variance (tau00) : %s\n",
    format(round(icc_result$re_var), big.mark=",")))
cat(sprintf("Within-school variance (sigma2) : %s\n",
    format(round(icc_result$resid_var), big.mark=",")))
cat(sprintf("ICC                             : %.3f\n", icc_result$icc))
cat(sprintf("Avg cluster size (m-bar)        : %.2f\n",
    nrow(d_cross) / nlevels(droplevels(d_cross$schid))))
cat(sprintf("Design effect (DEFF)            : %.3f\n", icc_result$deff))
cat(sprintf("Effective N                     : %.0f\n",
    nrow(d_cross) / icc_result$deff))

# =============================================================================
# Section 5.3 -- Contextual Effects / Mundlak Decomposition (Table 5, Figure 3)
# =============================================================================
cat("\n=== 5.3 Contextual Effects (Mundlak Specification) ===\n")

# Within-cluster centering and cluster-mean construction
# (already in d_cross from 01_build_teaching_data.R -- verify here)
if (!"SES_c" %in% names(d_cross) | !"SES_mean" %in% names(d_cross)) {
  d_cross <- d_cross %>%
    group_by(schid) %>%
    mutate(
      SES_mean = mean(SES_z, na.rm = TRUE),
      SES_c    = SES_z - SES_mean
    ) %>%
    ungroup()
}

# Mundlak model: within + between predictors
m1 <- lmer(
  math_score ~ SES_c + SES_mean + (1 | schid),
  data = d_cross
)

ctx <- hlm_context(m1, x_within = "SES_c", x_between = "SES_mean")
print(ctx)

# Figure 3
fig3 <- plot(ctx)
print(fig3)

# Numbers reported in Table 5
cat("\n--- Table 5 values ---\n")
bW  <- ctx$estimate[ctx$effect_type == "Within-cluster"]
seW <- ctx$se[ctx$effect_type == "Within-cluster"]
bB  <- ctx$estimate[ctx$effect_type == "Between-cluster"]
seB <- ctx$se[ctx$effect_type == "Between-cluster"]
bC  <- ctx$estimate[ctx$effect_type == "Contextual (B - W)"]
seC <- ctx$se[ctx$effect_type == "Contextual (B - W)"]

cat(sprintf("Within-cluster  : %.1f (SE = %.1f, 95%% CI [%.1f, %.1f])\n",
    bW, seW, bW - 1.96*seW, bW + 1.96*seW))
cat(sprintf("Between-cluster : %.1f (SE = %.1f, 95%% CI [%.1f, %.1f])\n",
    bB, seB, bB - 1.96*seB, bB + 1.96*seB))
cat(sprintf("Contextual      : %.1f (SE = %.1f, 95%% CI [%.1f, %.1f])\n",
    bC, seC, bC - 1.96*seC, bC + 1.96*seC))

# =============================================================================
# Section 5.4 -- Random Slopes Fan Plot (Figure 4)
# =============================================================================
cat("\n=== 5.4 Random Slopes Fan Plot ===\n")

m2 <- lmer(
  math_score ~ SES_c + SES_mean + (SES_c | schid),
  data = d_cross
)

cat("Random slope model summary:\n")
print(VarCorr(m2))

# Figure 4 -- Random-slope heterogeneity (the figure embedded in the paper)
fig4 <- hlm_random_slope_plot(
  model      = m2,
  x_within   = "SES_c",
  cluster    = "schid",
  n_clusters = 20,
  n_points   = 40
)
print(fig4)

# Random slope variance (tau11)
vc <- VarCorr(m2)
tau11 <- as.numeric(vc$schid)[4]   # slope variance (2x2 matrix, [2,2])
cat(sprintf("\nRandom slope variance (tau11) : %.1f\n", tau11))
cat(sprintf("Random slope SD               : %.2f\n", sqrt(tau11)))

# =============================================================================
# Section 5.5 -- Three-Level Variance Decomposition (Table 6, Figure 5)
# =============================================================================
cat("\n=== 5.5 Three-Level Variance Decomposition ===\n")

dec_long <- hlm_decompose_long(
  data    = d_clean,
  var     = "math_score",
  cluster = "schid",
  id      = "id",
  time    = "wave"
)
print(dec_long)

# Figure 5
fig5 <- plot(dec_long)
print(fig5)

# Numbers reported in Table 6
cat("\n--- Table 6 values ---\n")
smry <- dec_long$summary
for (i in 1:nrow(smry)) {
  cat(sprintf("%-45s : %s (%.1f%%)\n",
      smry$component[i],
      format(round(smry$variance[i]), big.mark=","),
      smry$share[i] * 100))
}

# =============================================================================
# Session information (for reproducibility)
# =============================================================================
cat("\n=== Session Information ===\n")
sessionInfo()
