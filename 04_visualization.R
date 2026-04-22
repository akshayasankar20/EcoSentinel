# =============================================================================
# EcoSentinel – Module 4: Full Visualization Suite
# Generates all publication-quality plots saved as PNG files
# =============================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(gridExtra)
  library(scales)
  library(pROC)
})

# Custom theme for all plots
theme_eco <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.background  = element_rect(fill="#0d1117", color=NA),
      panel.background = element_rect(fill="#161b22", color=NA),
      panel.grid.major = element_line(color="#30363d", linewidth=0.4),
      panel.grid.minor = element_line(color="#21262d", linewidth=0.2),
      text             = element_text(color="#e6edf3", family="sans"),
      axis.text        = element_text(color="#8b949e"),
      axis.title       = element_text(color="#c9d1d9"),
      plot.title       = element_text(color="#58a6ff", size=15, face="bold",
                                      margin=margin(b=8)),
      plot.subtitle    = element_text(color="#8b949e", size=11,
                                      margin=margin(b=12)),
      legend.background= element_rect(fill="#161b22", color=NA),
      legend.text      = element_text(color="#c9d1d9"),
      legend.title     = element_text(color="#58a6ff"),
      strip.text       = element_text(color="#79c0ff", face="bold"),
      plot.caption     = element_text(color="#484f58", size=9),
      plot.margin      = margin(16,16,16,16)
    )
}

# Colors
COL_STABLE   <- "#238636"
COL_NEARTIP  <- "#f85149"
COL_WARN     <- "#e3b341"
COL_BLUE     <- "#58a6ff"
COL_PURPLE   <- "#bc8cff"
COL_TEAL     <- "#39d353"
COLORS_4     <- c("#58a6ff","#e3b341","#f85149","#39d353")

# ── Load data ────────────────────────────────────────────────────────────────
eco_raw   <- readRDS("eco_data_raw.rds")
eco_norm  <- readRDS("eco_data_norm.rds")
rf_model  <- readRDS("rf_model.rds")
perm_df   <- readRDS("perm_importance.rds")
lime_out  <- readRDS("lime_explanation.rds")
roc_obj   <- readRDS("roc_object.rds")
kendall   <- readRDS("kendall_results.rds")
ind_cont  <- readRDS("indicator_contribution.rds")

dir.create("plots", showWarnings=FALSE)

# =============================================================================
# PLOT 1: Four Ecosystem Indicators with Tipping Point Annotation
# =============================================================================
cat("Generating Plot 1: Ecosystem time series...\n")

tp_date <- eco_raw$date[241]

p1_data <- eco_raw %>%
  select(date, sea_ice, amazon_rain, coral_stress, soil_carbon) %>%
  pivot_longer(-date, names_to="Indicator", values_to="Value") %>%
  mutate(Indicator = factor(Indicator,
    levels = c("sea_ice","amazon_rain","coral_stress","soil_carbon"),
    labels = c("Sea Ice Extent (Mkm²)",
               "Amazon Rainfall (mm/mo)",
               "Coral Bleaching Stress",
               "Soil Carbon Index")))

p1 <- ggplot(p1_data, aes(x=date, y=Value)) +
  geom_line(color=COL_BLUE, linewidth=0.7, alpha=0.85) +
  geom_smooth(method="loess", span=0.15, color=COL_WARN,
              linewidth=0.9, se=FALSE, linetype="dashed") +
  geom_vline(xintercept=as.numeric(tp_date),
             color=COL_NEARTIP, linewidth=1.1, linetype="solid", alpha=0.8) +
  annotate("label", x=tp_date, y=-Inf, vjust=-0.5,
           label=" ← Tipping Point ", fill="#f8514930",
           color=COL_NEARTIP, size=3.2, fontface="bold") +
  facet_wrap(~Indicator, scales="free_y", ncol=2) +
  labs(title="Ecosystem Indicator Time Series (1994–2024)",
       subtitle="Vertical red line marks the ecological regime shift (tipping point)",
       x="Year", y="Value",
       caption="EcoSentinel | Simulated from real-world statistical patterns") +
  theme_eco()

ggsave("plots/01_time_series.png", p1, width=13, height=8, dpi=150)
cat("  ✓ plots/01_time_series.png\n")

# =============================================================================
# PLOT 2: Early Warning Signals — Variance & AC-1 Rising Before Tipping Point
# =============================================================================
cat("Generating Plot 2: EWS rising before tipping point...\n")

ews_plot_data <- eco_norm %>%
  select(date, ice_var, ice_ac1, rain_var, rain_ac1,
         coral_var, coral_ac1, soil_var, soil_ac1) %>%
  pivot_longer(-date, names_to="metric", values_to="value") %>%
  filter(!is.na(value)) %>%
  mutate(
    Indicator = case_when(
      grepl("^ice",   metric) ~ "Sea Ice",
      grepl("^rain",  metric) ~ "Amazon Rain",
      grepl("^coral", metric) ~ "Coral Stress",
      grepl("^soil",  metric) ~ "Soil Carbon"
    ),
    EWS = ifelse(grepl("var", metric), "Variance (CSD)", "Autocorrelation AC-1"),
    Indicator = factor(Indicator,
      levels=c("Sea Ice","Amazon Rain","Coral Stress","Soil Carbon"))
  )

p2 <- ggplot(ews_plot_data, aes(x=date, y=value, color=EWS)) +
  geom_line(linewidth=0.7, alpha=0.8) +
  geom_vline(xintercept=as.numeric(tp_date),
             color=COL_NEARTIP, linewidth=1, linetype="dashed", alpha=0.7) +
  scale_color_manual(values=c("Variance (CSD)"=COL_BLUE,
                               "Autocorrelation AC-1"=COL_WARN)) +
  facet_wrap(~Indicator, ncol=2) +
  labs(title="Early Warning Signals (EWS) – Critical Slowing Down",
       subtitle="Both variance and autocorrelation RISE before the tipping point — a universal pre-collapse signature",
       x="Year", y="Normalized EWS Value (0–1)", color="Signal",
       caption="EcoSentinel | Bifurcation theory predicts rising EWS before regime shifts") +
  theme_eco() +
  theme(legend.position="bottom")

ggsave("plots/02_ews_signals.png", p2, width=13, height=8, dpi=150)
cat("  ✓ plots/02_ews_signals.png\n")

# =============================================================================
# PLOT 3: RF Prediction Probability Over Time
# =============================================================================
cat("Generating Plot 3: Model prediction probability...\n")

feature_cols <- readRDS("feature_cols.rds")
eco_model_all <- eco_norm %>%
  filter(!is.na(ice_var)) %>%
  mutate(label = factor(tipping_point))

pred_all <- predict(rf_model, eco_model_all[, feature_cols], type="prob")[,"NearTip"]

pred_timeline <- eco_model_all %>%
  select(date, tipping_point) %>%
  mutate(
    NearTip_Prob = pred_all,
    Phase        = ifelse(tipping_point == 1, "Post-Tipping", "Pre-Tipping")
  )

p3 <- ggplot(pred_timeline, aes(x=date, y=NearTip_Prob)) +
  geom_ribbon(aes(ymin=0, ymax=NearTip_Prob,
                  fill=ifelse(NearTip_Prob > 0.5, "Alert", "Safe")),
              alpha=0.35) +
  geom_line(color="white", linewidth=0.8, alpha=0.9) +
  geom_hline(yintercept=0.5, color=COL_WARN, linewidth=0.9,
             linetype="dashed") +
  geom_vline(xintercept=as.numeric(tp_date),
             color=COL_NEARTIP, linewidth=1.2, linetype="solid") +
  scale_fill_manual(values=c("Alert"=COL_NEARTIP, "Safe"=COL_STABLE),
                    guide="none") +
  annotate("text", x=tp_date, y=0.95,
           label="Tipping\nPoint", color=COL_NEARTIP,
           hjust=-0.1, size=3.5, fontface="bold") +
  annotate("text", x=as.Date("2009-01-01"), y=0.53,
           label="Alert threshold (0.5)", color=COL_WARN, size=3.2) +
  scale_y_continuous(labels=percent_format(), limits=c(0,1)) +
  labs(title="Tipping Point Prediction Probability Over Time",
       subtitle="Random Forest predicts nearness to ecological regime shift. Red zone = high danger.",
       x="Year", y="Probability of Imminent Tipping Point",
       caption="EcoSentinel | 12-month lead prediction") +
  theme_eco()

ggsave("plots/03_prediction_timeline.png", p3, width=13, height=6, dpi=150)
cat("  ✓ plots/03_prediction_timeline.png\n")

# =============================================================================
# PLOT 4: Feature Importance (Global XAI)
# =============================================================================
cat("Generating Plot 4: Feature importance...\n")

top15 <- perm_df %>%
  head(15) %>%
  mutate(
    Feature_clean = gsub("_", " ", Feature) %>% tools::toTitleCase(),
    Indicator = factor(Indicator,
      levels=c("Sea Ice","Amazon Rain","Coral Stress","Soil Carbon"))
  )

p4 <- ggplot(top15, aes(x=reorder(Feature_clean, Importance),
                          y=Importance, fill=Indicator)) +
  geom_col(alpha=0.9, width=0.7) +
  geom_text(aes(label=sprintf("%.4f", Importance)),
            hjust=-0.1, color="#c9d1d9", size=3.3) +
  coord_flip() +
  scale_fill_manual(values=c("Sea Ice"=COLORS_4[1],
                              "Amazon Rain"=COLORS_4[2],
                              "Coral Stress"=COLORS_4[3],
                              "Soil Carbon"=COLORS_4[4])) +
  scale_y_continuous(expand=expansion(mult=c(0, 0.25))) +
  labs(title="Global Feature Importance (Permutation Method)",
       subtitle="Drop in accuracy when feature is randomly shuffled — larger drop = more important",
       x=NULL, y="Permutation Importance (Accuracy Drop)", fill="Ecosystem",
       caption="EcoSentinel | Model-agnostic global explainability") +
  theme_eco() +
  theme(legend.position="bottom")

ggsave("plots/04_feature_importance.png", p4, width=12, height=8, dpi=150)
cat("  ✓ plots/04_feature_importance.png\n")

# =============================================================================
# PLOT 5: LIME Local Explanation
# =============================================================================
cat("Generating Plot 5: LIME local explanation...\n")

lime_plot_df <- lime_out$result %>%
  head(12) %>%
  mutate(
    Feature_clean = gsub("_", " ", Feature) %>% tools::toTitleCase(),
    BarColor = ifelse(Coefficient > 0, COL_NEARTIP, COL_STABLE)
  )

p5 <- ggplot(lime_plot_df,
             aes(x=reorder(Feature_clean, abs(Coefficient)),
                 y=Coefficient, fill=Direction)) +
  geom_col(alpha=0.9, width=0.65) +
  geom_hline(yintercept=0, color="#484f58") +
  coord_flip() +
  scale_fill_manual(values=c("Pushes to NearTip"=COL_NEARTIP,
                              "Pushes to Stable" =COL_STABLE)) +
  labs(title=sprintf("Local Explanation (LIME) for Instance at Row %d",
                     lime_out$instance_idx),
       subtitle=sprintf("True Label: %s  |  Predicted NearTip Probability: %.1f%%",
                        lime_out$true_label,
                        lime_out$pred_prob * 100),
       x=NULL, y="Local Coefficient (effect on NearTip probability)",
       fill="Effect Direction",
       caption="EcoSentinel | Locally-Interpretable Model-Agnostic Explanations") +
  theme_eco() +
  theme(legend.position="bottom")

ggsave("plots/05_lime_explanation.png", p5, width=12, height=7, dpi=150)
cat("  ✓ plots/05_lime_explanation.png\n")

# =============================================================================
# PLOT 6: ROC Curve
# =============================================================================
cat("Generating Plot 6: ROC curve...\n")

roc_df <- data.frame(
  Sensitivity  = rev(roc_obj$sensitivities),
  Specificity  = rev(1 - roc_obj$specificities)
)

p6 <- ggplot(roc_df, aes(x=Specificity, y=Sensitivity)) +
  geom_ribbon(aes(ymin=Specificity, ymax=Sensitivity),
              fill=COL_BLUE, alpha=0.15) +
  geom_line(color=COL_BLUE, linewidth=1.4) +
  geom_abline(slope=1, intercept=0, color="#484f58",
              linetype="dashed", linewidth=0.8) +
  annotate("text", x=0.6, y=0.25,
           label=sprintf("AUC = %.4f", as.numeric(auc(roc_obj))),
           color=COL_BLUE, size=5.5, fontface="bold") +
  scale_x_continuous(labels=percent_format()) +
  scale_y_continuous(labels=percent_format()) +
  labs(title="ROC Curve — Tipping Point Early Warning",
       subtitle="Higher AUC = better discrimination between Stable and Near-Tipping states",
       x="1 – Specificity (False Positive Rate)",
       y="Sensitivity (True Positive Rate)",
       caption="EcoSentinel | Out-of-sample test set evaluation") +
  theme_eco()

ggsave("plots/06_roc_curve.png", p6, width=8, height=7, dpi=150)
cat("  ✓ plots/06_roc_curve.png\n")

# =============================================================================
# PLOT 7: Kendall Tau Results (EWS Statistical Validity)
# =============================================================================
cat("Generating Plot 7: Kendall tau significance...\n")

k_df <- kendall %>%
  mutate(
    Label     = paste(Indicator, EWS, sep="\n"),
    Sig_color = ifelse(Significant == "YES ✓", COL_TEAL, COL_NEARTIP)
  )

p7 <- ggplot(k_df, aes(x=reorder(Label, Tau), y=Tau,
                         fill=Significant)) +
  geom_col(alpha=0.85, width=0.65) +
  geom_hline(yintercept=0, color="#484f58") +
  coord_flip() +
  scale_fill_manual(values=c("YES ✓"=COL_TEAL, "no"=COL_NEARTIP)) +
  labs(title="Kendall τ Trend Test — EWS Rising Before Tipping Point",
       subtitle="Positive τ with p<0.05 confirms Early Warning Signals are statistically valid",
       x=NULL, y="Kendall's Tau (monotonic trend strength)",
       fill="Significant (p<0.05)?",
       caption="EcoSentinel | Non-parametric test of EWS monotonicity") +
  theme_eco() +
  theme(legend.position="bottom")

ggsave("plots/07_kendall_tau.png", p7, width=12, height=7, dpi=150)
cat("  ✓ plots/07_kendall_tau.png\n")

# =============================================================================
# PLOT 8: Indicator Contribution Pie / Donut
# =============================================================================
cat("Generating Plot 8: Indicator contribution donut...\n")

ind_plot <- ind_cont %>%
  mutate(
    ymax  = cumsum(Percentage / 100),
    ymin  = lag(ymax, default=0),
    label_pos = (ymin + ymax) / 2,
    label = sprintf("%s\n%.1f%%", Indicator, Percentage)
  )

p8 <- ggplot(ind_plot, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=2.5,
                             fill=Indicator)) +
  geom_rect(alpha=0.9) +
  geom_text(aes(x=3.25, y=label_pos, label=label),
            color="white", size=3.8, fontface="bold") +
  scale_fill_manual(values=setNames(COLORS_4,
    c("Sea Ice","Amazon Rain","Coral Stress","Soil Carbon"))) +
  coord_polar(theta="y") +
  xlim(c(0, 4)) +
  labs(title="Ecosystem Indicator Contribution to Prediction",
       subtitle="Which ecosystem drives the tipping point signal the most?",
       caption="EcoSentinel | Aggregated permutation importance by indicator") +
  theme_void() +
  theme(
    plot.background = element_rect(fill="#0d1117", color=NA),
    plot.title      = element_text(color="#58a6ff", size=15, face="bold",
                                   hjust=0.5, margin=margin(b=4)),
    plot.subtitle   = element_text(color="#8b949e", size=11, hjust=0.5,
                                   margin=margin(b=8)),
    plot.caption    = element_text(color="#484f58", size=9, hjust=0.5),
    legend.position = "none",
    plot.margin     = margin(16,16,16,16)
  )

ggsave("plots/08_indicator_donut.png", p8, width=9, height=8, dpi=150)
cat("  ✓ plots/08_indicator_donut.png\n")

cat("\n✓ All 8 plots generated in plots/ directory\n")
