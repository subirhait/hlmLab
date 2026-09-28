if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    # shared plotting aesthetics
    "x", "y", "cluster_id",
    # hlm_context
    "effect_type", "estimate", "se", "conf_low", "conf_high",
    # hlm_decompose / hlm_icc_plot
    "component", "share", "variance",
    # hlm_icc_demo
    "panel_label", "cluster_mean", "cluster_position",
    # hlm_cross_level_plot
    "moderator_label",
    # hlm_shrinkage_plot
    "raw_mean", "pooled_mean", "cluster_n", "reliability"
  ))
}
