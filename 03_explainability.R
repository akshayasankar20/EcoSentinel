# =============================================================================
# EcoSentinel – Module 3: Explainability (XAI)
# Implements SHAP-equivalent permutation importance + local explanation
# =============================================================================
# WHY XAI FOR ECOLOGY?
#   A model saying "tipping point in 3 months" is useless to a scientist.
#   They need to know: IS IT THE ICE MELTING? THE RAINFALL? THE SOIL?
#   XAI answers that — making AI useful for real policy decisions.
# =============================================================================

suppressPackageStartupMessages({
  library(randomForest)
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(gridExtra)
})

rf_model     <- readRDS("rf_model.rds")
test_set     <- readRDS("test_set_labeled.rds")
imp_df       <- readRDS("feature_importance.rds")
feature_cols <- readRDS("feature_cols.rds")

# =============================================================================
# PART A: GLOBAL EXPLAINABILITY — Permutation Feature Importance
#   For each feature: shuffle it, measure accuracy drop.
#   Big drop = that feature matters a lot.
#   This is the principled "model-agnostic SHAP approximation."
# =============================================================================
cat("── Part A: Permutation Feature Importance (Global XAI) ──\n\n")

base_pred   <- predict(rf_model, test_set[, feature_cols])
base_acc    <- mean(base_pred == test_set$label)

perm_importance <- numeric(length(feature_cols))
names(perm_importance) <- feature_cols

set.seed(99)
for (f in feature_cols) {
  perm_data      <- test_set[, feature_cols]
  perm_data[[f]] <- sample(perm_data[[f]])   # shuffle one feature
  perm_pred      <- predict(rf_model, perm_data)
  perm_acc       <- mean(perm_pred == test_set$label)
  perm_importance[f] <- base_acc - perm_acc  # accuracy DROP = importance
}

perm_df <- data.frame(
  Feature    = names(perm_importance),
  Importance = as.numeric(perm_importance)
) %>%
  arrange(desc(Importance)) %>%
  mutate(
    Rank      = row_number(),
    Indicator = case_when(
      grepl("^ice",   Feature) ~ "Sea Ice",
      grepl("^rain",  Feature) ~ "Amazon Rain",
      grepl("^coral", Feature) ~ "Coral Stress",
      grepl("^soil",  Feature) ~ "Soil Carbon",
      TRUE ~ "Unknown"
    ),
    EWS_Type  = case_when(
      grepl("var",  Feature) ~ "Variance",
      grepl("ac1",  Feature) ~ "Autocorrelation",
      grepl("skew", Feature) ~ "Skewness",
      grepl("cv",   Feature) ~ "Coeff. of Variation",
      TRUE ~ "Other"
    )
  )

cat("Top 10 most important features (permutation method):\n")
print(perm_df[1:min(10, nrow(perm_df)),
              c("Rank","Feature","Indicator","EWS_Type","Importance")])

saveRDS(perm_df, "perm_importance.rds")

# =============================================================================
# PART B: LOCAL EXPLAINABILITY — LIME-style explanation
#   For a SPECIFIC prediction (e.g., "Month 235 — NearTip at 0.89 probability"):
#   • Train a simple linear model on the neighborhood of that point
#   • Show which features LOCALLY pushed the prediction toward NearTip
# =============================================================================
cat("\n── Part B: Local Explanation (LIME-style) for Single Prediction ──\n\n")

lime_explain <- function(model, data, feature_cols, instance_idx,
                         n_perturb = 200, kernel_width = 0.75) {

  instance  <- data[instance_idx, feature_cols]
  true_label <- data$label[instance_idx]
  pred_prob  <- predict(model, instance, type="prob")[, "NearTip"]

  cat(sprintf("Instance: row %d | True label: %s | Predicted NearTip prob: %.3f\n",
              instance_idx, as.character(true_label), pred_prob))

  # Perturb: add Gaussian noise around the instance
  set.seed(42)
  perturbed <- matrix(NA, nrow = n_perturb, ncol = length(feature_cols))
  colnames(perturbed) <- feature_cols
  for (j in seq_along(feature_cols)) {
    col_sd <- sd(data[, feature_cols[j]], na.rm = TRUE)
    perturbed[, j] <- instance[[feature_cols[j]]] +
                      rnorm(n_perturb, 0, col_sd * 0.1)
    # Clamp to [0,1] since features are normalized
    perturbed[, j] <- pmin(pmax(perturbed[, j], 0), 1)
  }
  perturbed_df  <- as.data.frame(perturbed)

  # Kernel weights (closer = higher weight)
  dists   <- sqrt(rowSums((perturbed_df - matrix(
                  unlist(instance), nrow=n_perturb,
                  ncol=length(feature_cols), byrow=TRUE))^2))
  weights <- exp(-(dists^2) / (kernel_width^2))

  # Get RF predictions on perturbed data
  perturbed_preds <- predict(model, perturbed_df, type="prob")[,"NearTip"]

  # Fit weighted linear model (the "local surrogate")
  lime_df <- as.data.frame(perturbed)
  lime_df$y <- perturbed_preds
  lm_fit   <- lm(y ~ ., data = lime_df, weights = weights)

  # Extract coefficients (= local feature importance)
  coefs <- coef(lm_fit)[-1]  # remove intercept
  lime_result <- data.frame(
    Feature    = names(coefs),
    Coefficient = as.numeric(coefs),
    Direction  = ifelse(as.numeric(coefs) > 0, "Pushes to NearTip",
                                                "Pushes to Stable"),
    stringsAsFactors = FALSE
  ) %>%
    mutate(AbsCoef = abs(Coefficient)) %>%
    arrange(desc(AbsCoef))

  return(list(
    result       = lime_result,
    instance_idx = instance_idx,
    true_label   = true_label,
    pred_prob    = pred_prob
  ))
}

# Explain the highest-confidence NearTip prediction in test set
test_preds      <- predict(rf_model, test_set[,feature_cols], type="prob")
near_tip_probs  <- test_preds[, "NearTip"]
best_idx        <- which.max(near_tip_probs)  # most confident near-tip

lime_out <- lime_explain(rf_model, test_set, feature_cols, best_idx)

cat("\nLIME Local Explanation (top 10 features):\n")
print(lime_out$result[1:min(10, nrow(lime_out$result)),
                      c("Feature","Coefficient","Direction")])

saveRDS(lime_out, "lime_explanation.rds")

# =============================================================================
# PART C: INDICATOR CONTRIBUTION SUMMARY
#   Aggregate permutation importance by ECOSYSTEM INDICATOR
#   → tells scientists: "The ice system is driving 42% of the warning signal"
# =============================================================================
cat("\n── Part C: Ecosystem Indicator Contribution ──\n\n")

indicator_contrib <- perm_df %>%
  group_by(Indicator) %>%
  summarise(
    Total_Importance = sum(pmax(Importance, 0)),
    .groups = "drop"
  ) %>%
  mutate(
    Percentage = 100 * Total_Importance / sum(Total_Importance),
    Percentage = round(Percentage, 1)
  ) %>%
  arrange(desc(Percentage))

cat("Indicator contribution to tipping point prediction:\n")
print(indicator_contrib)

ews_type_contrib <- perm_df %>%
  group_by(EWS_Type) %>%
  summarise(
    Total_Importance = sum(pmax(Importance, 0)),
    .groups = "drop"
  ) %>%
  mutate(
    Percentage = 100 * Total_Importance / sum(Total_Importance),
    Percentage = round(Percentage, 1)
  ) %>%
  arrange(desc(Percentage))

cat("\nEWS metric contribution to prediction:\n")
print(ews_type_contrib)

saveRDS(indicator_contrib, "indicator_contribution.rds")
saveRDS(ews_type_contrib,  "ews_type_contribution.rds")

cat("\n✓ XAI modules complete. Files saved:\n")
cat("   perm_importance.rds | lime_explanation.rds\n")
cat("   indicator_contribution.rds | ews_type_contribution.rds\n")
