# =============================================================================
# EcoSentinel – MASTER RUN SCRIPT
# Runs all modules in order, then launches the Shiny dashboard
# =============================================================================
# HOW TO RUN THIS PROJECT:
#   1. Open RStudio
#   2. Set working directory: setwd("path/to/ecosentinel/")
#   3. Run this file: source("00_run_all.R")
# =============================================================================

cat("╔══════════════════════════════════════════════════════════════╗\n")
cat("║           🌍 EcoSentinel — Ecological Tipping Point         ║\n")
cat("║              Detection System  |  Data Science R             ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n\n")

# ── Step 0: Install required packages ─────────────────────────────────────────
required_pkgs <- c(
  "changepoint", "randomForest", "caret", "dplyr", "tidyr",
  "lubridate", "ggplot2", "gridExtra", "scales", "pROC",
  "shiny", "shinydashboard", "plotly", "DT", "zoo"
)

missing_pkgs <- required_pkgs[!required_pkgs %in% rownames(installed.packages())]
if (length(missing_pkgs) > 0) {
  cat("Installing missing packages:", paste(missing_pkgs, collapse=", "), "\n")
  install.packages(missing_pkgs, repos="https://cran.rstudio.com/",
                   quiet=TRUE)
  cat("✓ Packages installed\n\n")
} else {
  cat("✓ All packages already installed\n\n")
}

# ── Step 1: Data Simulation & Preprocessing ──────────────────────────────────
cat("═══ Step 1/4 · Data Simulation & Preprocessing ═══\n")
source("01_data_simulation.R")
cat("\n")

# ── Step 2: Model Building & Changepoint Detection ───────────────────────────
cat("═══ Step 2/4 · Bayesian Changepoint + Random Forest ═══\n")
source("02_model_and_detection.R")
cat("\n")

# ── Step 3: Explainability Module ─────────────────────────────────────────────
cat("═══ Step 3/4 · XAI — Permutation + LIME ═══\n")
source("03_explainability.R")
cat("\n")

# ── Step 4: Generate All Plots ────────────────────────────────────────────────
cat("═══ Step 4/4 · Generating Publication-Quality Plots ═══\n")
source("04_visualization.R")
cat("\n")

# ── Summary ──────────────────────────────────────────────────────────────────
cat("╔══════════════════════════════════════════════════════════════╗\n")
cat("║                  ✅ ALL MODULES COMPLETE                     ║\n")
cat("╠══════════════════════════════════════════════════════════════╣\n")
cat("║  Generated files:                                            ║\n")
cat("║  • eco_data_raw.rds      – 360-month time series            ║\n")
cat("║  • rf_model.rds          – Trained Random Forest            ║\n")
cat("║  • changepoints.rds      – PELT detection results           ║\n")
cat("║  • perm_importance.rds   – Global XAI                       ║\n")
cat("║  • lime_explanation.rds  – Local XAI                        ║\n")
cat("║  • kendall_results.rds   – EWS statistical tests            ║\n")
cat("║  • plots/01–08_*.png     – 8 publication-quality plots      ║\n")
cat("╠══════════════════════════════════════════════════════════════╣\n")
cat("║  Launching Shiny Dashboard...                                ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n\n")

shiny::runApp("app.R", port=3838, launch.browser=TRUE)
