# =============================================================================
# 03_make_figures_tables.R
#
# Purpose : Generate and save all publication-quality figures and tables
#           for the hlmLab manuscript.
#
#           Figures are saved as high-resolution PNG files (300 dpi).
#           Tables are printed to the console in a copy-paste ready format.
#
# Requires: d_cross and d_clean produced by 01_build_teaching_data.R
#           hlmLab >= 0.1.0, lme4, dplyr, ggplot2
#
# Authors : [ANONYMOUS FOR REVIEW]
# Date    : 2025
# =============================================================================

library(hlmLab)
library(lme4)
library(dplyr)
library(ggplot2)

# -----------------------------------------------------------------------------
# Setup
# -----------------------------------------------------------------------------
d_cross <- readRDS("scripts/data_processed/d_cross.rds")
d_clean <- readRDS("scripts/data_processed/d_clean.rds")

# Create output directory for figures
dir.create("figures", showWarnings = FALSE)

# Shared ggplot2 theme for all figures (clean, publication-ready)
theme_paper <- theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    axis.title    = element_text(size = 11),
    axis.text     = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

# Ensure Mundlak variables exist
if (!"SES_c" %in% names(d_cross)) {
  d_cross <- d_cross %>%
    group_by(schid) %>%
    mutate(SES_mean = mean(SES_z, na.rm = TRUE),
           SES_c    = SES_z - SES_mean) %>%
    ungroup()
}

# Fit all models once
m0 <- lmer(math_score ~ 1 + (1 | schid), data = d_cross)
m1 <- lmer(math_score ~ SES_c + SES_mean + (1 | schid), data = d_cross)
m2 <- lmer(math_score ~ SES_c + SES_mean + (SES_c | schid), data = d_cross)

# =============================================================================
# FIGURE 1 -- Two-level variance decomposition bar chart
# =============================================================================
dec <- hlm_decompose(d_cross, var = "math_score", cluster = "schid")
df1 <- dec$summary
df1$component <- factor(df1$component, levels = df1$component)

fig1 <- ggplot(df1, aes(x = component, y = share)) +
  geom_col(fill = "steelblue", width = 0.55) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1), expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x        = NULL,
    y        = "Share of total variance",
    title    = "Figure 1. Two-level variance decomposition of mathematics achievement",
    subtitle = "ECLS-K:2011, Fall 2010, N = 8,492 students in 2,151 schools"
  ) +
  theme_paper +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

ggsave("figures/fig1_variance_decomposition.png", fig1,
       width = 6, height = 4, dpi = 300)
cat("Figure 1 saved: figures/fig1_variance_decomposition.png\n")

# =============================================================================
# FIGURE 2 -- ICC stacked bar chart
# =============================================================================
icc_result <- hlm_icc(m0, cluster_size = 4)

df2 <- data.frame(
  component = c("Between-school variance", "Within-school variance"),
  variance  = c(icc_result$re_var, icc_result$resid_var)
)
df2$share <- df2$variance / sum(df2$variance)

subtitle2 <- sprintf(
  "ICC = %.3f   |   Design effect \u2248 %.2f   |   Effective N \u2248 %s",
  icc_result$icc,
  icc_result$deff,
  format(round(nrow(d_cross) / icc_result$deff), big.mark = ",")
)

fig2 <- ggplot(df2, aes(x = "", y = share, fill = component)) +
  geom_col(width = 0.5) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("steelblue", "lightsteelblue")) +
  labs(
    x        = NULL,
    y        = "Share of total variance",
    fill     = NULL,
    title    = "Figure 2. Intraclass correlation as variance partitioning",
    subtitle = subtitle2
  ) +
  theme_paper +
  theme(
    legend.position = "bottom",
    axis.text.y     = element_blank(),
    axis.ticks.y    = element_blank()
  )

ggsave("figures/fig2_icc_plot.png", fig2,
       width = 6, height = 3, dpi = 300)
cat("Figure 2 saved: figures/fig2_icc_plot.png\n")

# =============================================================================
# FIGURE 3 -- Contextual effects error-bar plot
# =============================================================================
ctx <- hlm_context(m1, x_within = "SES_c", x_between = "SES_mean")
df3 <- as.data.frame(ctx)
df3$effect_type <- factor(
  df3$effect_type,
  levels = c("Between-cluster", "Contextual (B - W)", "Within-cluster")
)

fig3 <- ggplot(df3, aes(x = effect_type, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 3, color = "steelblue") +
  geom_errorbar(
    aes(ymin = estimate - 1.96 * se,
        ymax = estimate + 1.96 * se),
    width = 0.12, color = "steelblue"
  ) +
  geom_text(
    aes(label = sprintf("%.1f", estimate)),
    vjust = -1.2, size = 3.5
  ) +
  labs(
    x        = NULL,
    y        = "Effect on mathematics achievement\n(IRT score points per SD of SES)",
    title    = "Figure 3. Within-cluster, between-cluster, and contextual effects of SES",
    subtitle = "ECLS-K:2011, Fall 2010. Error bars are 95% confidence intervals."
  ) +
  theme_paper

ggsave("figures/fig3_contextual_effects.png", fig3,
       width = 6, height = 4.5, dpi = 300)
cat("Figure 3 saved: figures/fig3_contextual_effects.png\n")

# =============================================================================
# FIGURE 4 -- Random-slope heterogeneity
# =============================================================================
fig4 <- hlm_random_slope_plot(
  model      = m2,
  x_within   = "SES_c",
  cluster    = "schid",
  n_clusters = 20,
  n_points   = 40
) +
  labs(
    title    = "Figure 4. The within-school SES association varies across schools",
    subtitle = "20 schools spread across the slope distribution. Each line = one school's predicted SES slope.",
    x        = "Within-school SES (SES_c, standardized)",
    y        = "Predicted mathematics achievement (IRT score)"
  ) +
  theme_paper

ggsave("figures/fig4_fan_plot.png", fig4,
       width = 6.5, height = 4.5, dpi = 300)
cat("Figure 4 saved: figures/fig4_fan_plot.png\n")

# =============================================================================
# FIGURE 5 -- Three-level variance decomposition bar chart
# =============================================================================
dec_long <- hlm_decompose_long(
  data    = d_clean,
  var     = "math_score",
  cluster = "schid",
  id      = "id",
  time    = "wave"
)

df5 <- dec_long$summary
df5$component <- factor(df5$component, levels = df5$component)

fig5 <- ggplot(df5, aes(x = component, y = share)) +
  geom_col(fill = "steelblue", width = 0.55) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1), expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x        = NULL,
    y        = "Share of total variance",
    title    = "Figure 5. Three-level variance decomposition of mathematics achievement",
    subtitle = "ECLS-K:2011, all 9 waves, N = 76,428 student-wave observations"
  ) +
  theme_paper +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

ggsave("figures/fig5_bpw_decomposition.png", fig5,
       width = 6.5, height = 4.5, dpi = 300)
cat("Figure 5 saved: figures/fig5_bpw_decomposition.png\n")

# =============================================================================
# TABLES -- Print to console (copy into manuscript)
# =============================================================================
cat("\n")
cat(strrep("=", 60), "\n")
cat("TABLE 3: Two-Level Variance Decomposition\n")
cat(strrep("=", 60), "\n")
smry <- dec$summary
cat(sprintf("%-35s %15s %12s\n", "Component", "Variance", "% of Total"))
cat(strrep("-", 62), "\n")
for (i in 1:nrow(smry)) {
  cat(sprintf("%-35s %15s %11.1f%%\n",
      smry$component[i],
      format(round(smry$variance[i]), big.mark = ","),
      smry$share[i] * 100))
}

cat("\n")
cat(strrep("=", 60), "\n")
cat("TABLE 4: ICC and Design Effect\n")
cat(strrep("=", 60), "\n")
cat(sprintf("%-42s %15s\n", "Parameter", "Estimate"))
cat(strrep("-", 57), "\n")
cat(sprintf("%-42s %15s\n", "Between-school variance (tau00)",
    format(round(icc_result$re_var), big.mark = ",")))
cat(sprintf("%-42s %15s\n", "Within-school variance (sigma2)",
    format(round(icc_result$resid_var), big.mark = ",")))
cat(sprintf("%-42s %15.3f\n", "Intraclass correlation (ICC)", icc_result$icc))
cat(sprintf("%-42s %15.2f\n", "Average cluster size (m-bar)",
    nrow(d_cross) / nlevels(droplevels(d_cross$schid))))
cat(sprintf("%-42s %15.3f\n", "Design effect (DEFF)", icc_result$deff))

cat("\n")
cat(strrep("=", 60), "\n")
cat("TABLE 5: Mundlak Contextual Effect Decomposition\n")
cat(strrep("=", 60), "\n")
df_ctx <- as.data.frame(ctx)
cat(sprintf("%-30s %10s %8s %20s\n", "Effect", "Estimate", "SE", "95% CI"))
cat(strrep("-", 70), "\n")
for (i in 1:nrow(df_ctx)) {
  b  <- df_ctx$estimate[i]
  se <- df_ctx$se[i]
  cat(sprintf("%-30s %10.1f %8.1f  [%7.1f, %7.1f]\n",
      df_ctx$effect_type[i], b, se, b - 1.96*se, b + 1.96*se))
}

cat("\n")
cat(strrep("=", 60), "\n")
cat("TABLE 6: Three-Level Variance Decomposition\n")
cat(strrep("=", 60), "\n")
smry6 <- dec_long$summary
cat(sprintf("%-45s %15s %12s\n", "Component", "Variance", "% of Total"))
cat(strrep("-", 72), "\n")
for (i in 1:nrow(smry6)) {
  cat(sprintf("%-45s %15s %11.1f%%\n",
      smry6$component[i],
      format(round(smry6$variance[i]), big.mark = ","),
      smry6$share[i] * 100))
}

cat("\n")
cat(strrep("=", 60), "\n")
cat("All figures saved to: figures/\n")
cat("Run complete.\n")

# =============================================================================
# Session information
# =============================================================================
cat("\n=== Session Information ===\n")
sessionInfo()
