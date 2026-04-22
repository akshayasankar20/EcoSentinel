# =============================================================================
# EcoSentinel – Module 2: Bayesian Changepoint Detection + RF Classification
# =============================================================================
# CONCEPT:
#   Bayesian changepoint detection finds WHERE the regime shift happened
#   without knowing it in advance — purely from the signal statistics.
#   A Random Forest then learns to PREDICT tipping points BEFORE they happen
#   using Early Warning Signals (EWS) as features.
# =============================================================================

suppressPackageStartupMessages({
  library(changepoint)      # Bayesian/PELT changepoint detection
  library(randomForest)     # Ensemble classifier
  library(caret)            # Training utilities & confusion matrix
  library(dplyr)
  library(ggplot2)
  library(pROC)             # ROC curve & AUC
})

# ── Load preprocessed data ───────────────────────────────────────────────────
eco_raw      <- readRDS("eco_data_raw.rds")
eco_norm     <- readRDS("eco_data_norm.rds")
train_data   <- readRDS("train_data.rds")
test_data    <- readRDS("test_data.rds")
feature_cols <- readRDS("feature_cols.rds")

# =============================================================================
# PART A: BAYESIAN CHANGEPOINT DETECTION (unsupervised)
#   Uses PELT (Pruned Exact Linear Time) algorithm with BIC penalty
#   Detects structural breaks in mean AND variance of each indicator
# =============================================================================
cat("── Part A: Bayesian Changepoint Detection ──\n\n")

detect_changepoints <- function(signal, var_name, penalty = "BIC") {
  # Change in mean
  cp_mean <- cpt.mean(signal, penalty = penalty, method = "PELT",
                       minseglen = 12)
  # Change in variance
  cp_var  <- cpt.var(signal, penalty = penalty, method = "PELT",
                      minseglen = 12)
  # Combined (mean + variance)
  cp_both <- cpt.meanvar(signal, penalty = penalty, method = "PELT",
                          minseglen = 12)

  cat(sprintf("%-20s | Mean CPs: %s | Var CPs: %s | Combined CPs: %s\n",
              var_name,
              paste(cpts(cp_mean), collapse=","),
              paste(cpts(cp_var),  collapse=","),
              paste(cpts(cp_both), collapse=",")))

  list(mean = cp_mean, var = cp_var, both = cp_both)
}

cp_ice   <- detect_changepoints(eco_raw$sea_ice,      "Sea Ice Extent")
cp_rain  <- detect_changepoints(eco_raw$amazon_rain,  "Amazon Rainfall")
cp_coral <- detect_changepoints(eco_raw$coral_stress, "Coral Stress")
cp_soil  <- detect_changepoints(eco_raw$soil_carbon,  "Soil Carbon")

cat("\n[True tipping point is at month 240 — compare with detections above]\n\n")

# Extract combined changepoints for each indicator
all_cps <- list(
  sea_ice      = cpts(cp_ice$both),
  amazon_rain  = cpts(cp_rain$both),
  coral_stress = cpts(cp_coral$both),
  soil_carbon  = cpts(cp_soil$both)
)
saveRDS(all_cps, "changepoints.rds")

# =============================================================================
# PART B: RANDOM FOREST CLASSIFIER (supervised tipping point prediction)
#   Input : 16 EWS features (variance, AC-1, skewness, CV per indicator)
#   Output: Probability that the NEXT regime shift will occur within 12 months
# =============================================================================
cat("── Part B: Random Forest Tipping Point Predictor ──\n\n")

# Create 12-month lead label: 1 if tipping point occurs within next 12 months
lead_window <- 12
n_total     <- nrow(eco_norm)
eco_norm$label <- 0L

for (i in 1:(n_total - lead_window)) {
  future_window <- eco_norm$tipping_point[(i+1):(i+lead_window)]
  if (any(future_window == 1)) eco_norm$label[i] <- 1L
}

eco_model <- eco_norm %>%
  filter(!is.na(ice_var)) %>%   # remove rows with NA (first 30 months)
  mutate(label = factor(label, levels = c(0,1),
                        labels = c("Stable","NearTip")))

# Train/test split (time-ordered, no shuffle — respects temporal structure)
split_idx  <- floor(0.8 * nrow(eco_model))
train_set  <- eco_model[1:split_idx, ]
test_set   <- eco_model[(split_idx+1):nrow(eco_model), ]

cat(sprintf("Training samples : %d  |  Near-tip : %d  |  Stable : %d\n",
            nrow(train_set),
            sum(train_set$label == "NearTip"),
            sum(train_set$label == "Stable")))
cat(sprintf("Test samples     : %d  |  Near-tip : %d  |  Stable : %d\n\n",
            nrow(test_set),
            sum(test_set$label == "NearTip"),
            sum(test_set$label == "Stable")))

# Handle class imbalance with sampsize (balanced sampling per tree)
n_min   <- min(table(train_set$label))
n_maj   <- max(table(train_set$label))
ss      <- c(NearTip = n_min, Stable = min(n_min * 3, n_maj))

set.seed(2024)
rf_model <- randomForest(
  x          = train_set[, feature_cols],
  y          = train_set$label,
  ntree      = 500,
  mtry       = floor(sqrt(length(feature_cols))),
  importance = TRUE,
  sampsize   = ss,
  nodesize   = 5
)

cat("── Random Forest Summary ──\n")
print(rf_model)

# ── Test Set Evaluation ───────────────────────────────────────────────────────
pred_class  <- predict(rf_model, test_set[, feature_cols], type = "response")
pred_prob   <- predict(rf_model, test_set[, feature_cols], type = "prob")[, "NearTip"]

cm <- confusionMatrix(pred_class, test_set$label, positive = "NearTip")
cat("\n── Confusion Matrix (Test Set) ──\n")
print(cm)

# ROC & AUC
roc_obj <- roc(response  = ifelse(test_set$label == "NearTip", 1, 0),
               predictor = pred_prob,
               quiet     = TRUE)
cat(sprintf("\nAUC (Area Under ROC Curve): %.4f\n", auc(roc_obj)))

# ── Variable Importance ───────────────────────────────────────────────────────
imp_df <- as.data.frame(importance(rf_model))
imp_df$Feature <- rownames(imp_df)
imp_df <- imp_df %>%
  arrange(desc(MeanDecreaseGini)) %>%
  mutate(Rank = row_number())

cat("\n── Top 10 Most Important Features ──\n")
print(imp_df[1:10, c("Feature","MeanDecreaseGini","MeanDecreaseAccuracy","Rank")])

# ── Save everything ─────────────────────────────────────────────────────────
saveRDS(rf_model,  "rf_model.rds")
saveRDS(pred_prob, "test_pred_prob.rds")
saveRDS(test_set,  "test_set_labeled.rds")
saveRDS(imp_df,    "feature_importance.rds")
saveRDS(roc_obj,   "roc_object.rds")
saveRDS(cm,        "confusion_matrix.rds")

cat("\n✓ Model saved: rf_model.rds\n")
cat("✓ Feature importance saved: feature_importance.rds\n")

# =============================================================================
# PART C: EARLY WARNING SIGNAL TREND TEST (Kendall τ)
#   Statistically tests whether EWS metrics are monotonically increasing
#   before a tipping point — key proof from bifurcation theory
# =============================================================================
cat("\n── Part C: Kendall Tau Trend Test (EWS Rising Before Tipping Point) ──\n")

# Use data before tipping point only
pre_tip <- eco_raw %>% filter(tipping_point == 0)

ews_features_raw <- c("ice_var","ice_ac1","rain_var","rain_ac1",
                       "coral_var","coral_ac1","soil_var","soil_ac1")

# Recalculate on raw (non-normalized) data for interpretable statistics
rolling_ews_simple <- function(x, win=30) {
  n <- length(x); rv <- ac <- rep(NA, n)
  for(i in win:n) {
    w <- x[(i-win+1):i]
    rv[i] <- var(w, na.rm=TRUE)
    if(length(w)>1 && sd(w)>0) ac[i] <- cor(w[-win], w[-1])
  }
  list(var=rv, ac=ac)
}

kendall_results <- data.frame(
  Indicator = character(), EWS = character(),
  Tau = numeric(), P_value = numeric(), Significant = character(),
  stringsAsFactors = FALSE
)

indicators <- list(
  "Sea Ice"     = eco_raw$sea_ice[eco_raw$tipping_point == 0],
  "Amazon Rain" = eco_raw$amazon_rain[eco_raw$tipping_point == 0],
  "Coral Stress"= eco_raw$coral_stress[eco_raw$tipping_point == 0],
  "Soil Carbon" = eco_raw$soil_carbon[eco_raw$tipping_point == 0]
)

for (ind_name in names(indicators)) {
  sig <- indicators[[ind_name]]
  ews <- rolling_ews_simple(sig, win=30)
  x_var <- na.omit(ews$var)
  x_ac  <- na.omit(ews$ac)
  t_var <- cor.test(1:length(x_var), x_var, method="kendall")
  t_ac  <- cor.test(1:length(x_ac),  x_ac,  method="kendall")
  kendall_results <- rbind(kendall_results, data.frame(
    Indicator  = ind_name, EWS = "Variance",
    Tau = round(t_var$estimate, 3), P_value = round(t_var$p.value, 4),
    Significant = ifelse(t_var$p.value < 0.05, "YES ✓", "no"),
    stringsAsFactors = FALSE
  ))
  kendall_results <- rbind(kendall_results, data.frame(
    Indicator  = ind_name, EWS = "Autocorrelation",
    Tau = round(t_ac$estimate, 3), P_value = round(t_ac$p.value, 4),
    Significant = ifelse(t_ac$p.value < 0.05, "YES ✓", "no"),
    stringsAsFactors = FALSE
  ))
}

cat("\nKendall Tau Monotonicity Test (EWS before tipping point):\n")
print(kendall_results)
cat("\nInterpretation: Positive tau + p<0.05 → EWS rising significantly → tipping point warning valid\n")

saveRDS(kendall_results, "kendall_results.rds")
cat("\n✓ Kendall test results saved: kendall_results.rds\n")
