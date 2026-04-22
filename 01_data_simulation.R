# =============================================================================
# EcoSentinel – Module 1: Data Simulation & Preprocessing
# Simulates realistic ecological time-series data with embedded tipping points
# =============================================================================
# Author  : Team EcoSentinel
# Course  : R for Data Science
# Purpose : Generate synthetic climate/ecosystem indicator time-series that
#           mimic real-world datasets (Arctic Sea Ice, Amazon Rainfall, Coral
#           Bleaching Index, Soil Carbon) with known regime shifts embedded.
# =============================================================================

set.seed(42)

# ── Libraries ────────────────────────────────────────────────────────────────
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(zoo)
})

# =============================================================================
# 1. HELPER: Ornstein–Uhlenbeck process (mean-reverting noise)
#    Used to simulate realistic autocorrelated ecological signals
# =============================================================================
simulate_ou <- function(n, mu = 0, theta = 0.3, sigma = 1, x0 = 0) {
  x <- numeric(n)
  x[1] <- x0
  dt <- 1
  for (i in 2:n) {
    x[i] <- x[i-1] + theta * (mu - x[i-1]) * dt +
             sigma * sqrt(dt) * rnorm(1)
  }
  return(x)
}

# =============================================================================
# 2. SIMULATE FOUR ECOLOGICAL INDICATORS (monthly, 30 years = 360 points)
# =============================================================================
n        <- 360          # months
dates    <- seq(as.Date("1994-01-01"), by = "month", length.out = n)
tp_month <- 240          # tipping point at month 240 (year 20)

# ── 2a. Arctic Sea Ice Extent (million km²) ─────────────────────────────────
#   Before tip: stable ~7.5 Mkm²  |  After tip: crashes to ~4.2 Mkm²
sea_ice_base  <- c(
  7.5 - seq(0, 0.8, length.out = tp_month),          # slow decline
  4.2 - seq(0, 0.3, length.out = n - tp_month)        # post-collapse
)
seasonal_ice  <- 1.5 * sin(2 * pi * (1:n) / 12 + pi) # seasonal cycle
noise_ice     <- simulate_ou(n, mu=0, theta=0.4, sigma=0.15)
sea_ice       <- sea_ice_base + seasonal_ice + noise_ice
sea_ice       <- pmax(sea_ice, 0.5)                    # physical floor

# ── 2b. Amazon Rainfall Index (mm/month) ────────────────────────────────────
#   Simulates dieback-driven rainfall decline after deforestation threshold
rain_base     <- c(
  180 + simulate_ou(tp_month, mu=0, theta=0.2, sigma=5),
  120 + simulate_ou(n - tp_month, mu=-10, theta=0.15, sigma=8)
)
seasonal_rain <- 40 * sin(2 * pi * (1:n) / 12)
noise_rain    <- rnorm(n, 0, 3)
amazon_rain   <- rain_base + seasonal_rain + noise_rain
amazon_rain   <- pmax(amazon_rain, 20)

# ── 2c. Coral Bleaching Stress Index (°C-weeks above threshold) ─────────────
#   Accelerates sharply after tipping point
bleach_base   <- c(
  2 + 0.003 * (1:tp_month) + rnorm(tp_month, 0, 0.4),
  5 + 0.02  * (1:(n - tp_month)) + rnorm(n - tp_month, 0, 0.8)
)
seasonal_b    <- 1.2 * sin(2 * pi * (1:n) / 12 + pi/4)
coral_stress  <- bleach_base + seasonal_b
coral_stress  <- pmax(coral_stress, 0)

# ── 2d. Soil Carbon Stock (Pg C, relative index 0–100) ──────────────────────
#   Stable then sharp release (respiration > photosynthesis after warming tip)
soil_base     <- c(
  75 - 0.01 * (1:tp_month) + simulate_ou(tp_month, 0, 0.3, 0.5),
  73 - 0.12 * (1:(n - tp_month)) + simulate_ou(n - tp_month, 0, 0.3, 1.2)
)
soil_carbon   <- pmax(soil_base, 20)

# =============================================================================
# 3. ASSEMBLE MASTER DATAFRAME
# =============================================================================
eco_data <- data.frame(
  date          = dates,
  month_index   = 1:n,
  year          = year(dates),
  month         = month(dates),
  sea_ice       = round(sea_ice, 3),
  amazon_rain   = round(amazon_rain, 2),
  coral_stress  = round(coral_stress, 3),
  soil_carbon   = round(soil_carbon, 3),
  tipping_point = ifelse(1:n > tp_month, 1L, 0L),
  stringsAsFactors = FALSE
)

# =============================================================================
# 4. COMPUTE ROLLING EARLY WARNING SIGNALS (EWS)
#    Key metrics before a tipping point (from bifurcation theory):
#    • Variance increases  → "critical slowing down" variance
#    • Autocorrelation (AC-1) increases → slower recovery from perturbations
#    • Skewness changes   → asymmetric fluctuations near bifurcation
#    • Coefficient of Variation (CV)
# =============================================================================
rolling_ews <- function(x, win = 30) {
  n  <- length(x)
  rv <- ac <- sk <- cv_out <- rep(NA_real_, n)
  for (i in win:n) {
    w       <- x[(i - win + 1):i]
    rv[i]   <- var(w, na.rm = TRUE)
    ac[i]   <- ifelse(sd(w) > 0, cor(w[-win], w[-1]), NA)
    sk[i]   <- (mean((w - mean(w))^3)) / (sd(w)^3 + 1e-9)
    cv_out[i] <- sd(w) / (abs(mean(w)) + 1e-9)
  }
  list(variance = rv, autocorr = ac, skewness = sk, cv = cv_out)
}

# Compute EWS for each indicator (30-month rolling window)
ews_ice    <- rolling_ews(eco_data$sea_ice,      win = 30)
ews_rain   <- rolling_ews(eco_data$amazon_rain,  win = 30)
ews_coral  <- rolling_ews(eco_data$coral_stress, win = 30)
ews_soil   <- rolling_ews(eco_data$soil_carbon,  win = 30)

eco_data <- eco_data %>%
  mutate(
    # Sea ice EWS
    ice_var     = ews_ice$variance,
    ice_ac1     = ews_ice$autocorr,
    ice_skew    = ews_ice$skewness,
    ice_cv      = ews_ice$cv,
    # Amazon rain EWS
    rain_var    = ews_rain$variance,
    rain_ac1    = ews_rain$autocorr,
    rain_skew   = ews_rain$skewness,
    rain_cv     = ews_rain$cv,
    # Coral EWS
    coral_var   = ews_coral$variance,
    coral_ac1   = ews_coral$autocorr,
    coral_skew  = ews_coral$skewness,
    coral_cv    = ews_coral$cv,
    # Soil EWS
    soil_var    = ews_soil$variance,
    soil_ac1    = ews_soil$autocorr,
    soil_skew   = ews_soil$skewness,
    soil_cv     = ews_soil$cv
  )

# =============================================================================
# 5. NORMALIZE EWS FEATURES (min-max to 0–1 for model input)
# =============================================================================
minmax <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (diff(rng) == 0) return(rep(0, length(x)))
  (x - rng[1]) / diff(rng)
}

feature_cols <- c("ice_var","ice_ac1","ice_skew","ice_cv",
                  "rain_var","rain_ac1","rain_skew","rain_cv",
                  "coral_var","coral_ac1","coral_skew","coral_cv",
                  "soil_var","soil_ac1","soil_skew","soil_cv")

eco_data_norm <- eco_data
eco_data_norm[feature_cols] <- lapply(eco_data[feature_cols], minmax)

# =============================================================================
# 6. TRAIN / TEST SPLIT (first 80% = train, last 20% = test/future)
# =============================================================================
train_end  <- floor(0.8 * n)   # 288
train_data <- eco_data_norm[1:train_end, ]
test_data  <- eco_data_norm[(train_end + 1):n, ]

cat("=== EcoSentinel Data Simulation Complete ===\n")
cat(sprintf("Total observations : %d months (%d years)\n", n, n/12))
cat(sprintf("Tipping point at   : month %d (year %.1f)\n", tp_month, tp_month/12))
cat(sprintf("Training set       : %d observations\n", nrow(train_data)))
cat(sprintf("Test set           : %d observations\n", nrow(test_data)))
cat(sprintf("Features computed  : %d EWS features per indicator\n", 4))
cat(sprintf("Total EWS features : %d\n", length(feature_cols)))
cat("\n── Sample of processed data (last 5 rows) ──\n")
print(tail(eco_data_norm[, c("date","sea_ice","amazon_rain",
                              "ice_var","ice_ac1","tipping_point")], 5))

# Save datasets for other modules
saveRDS(eco_data,      "eco_data_raw.rds")
saveRDS(eco_data_norm, "eco_data_norm.rds")
saveRDS(train_data,    "train_data.rds")
saveRDS(test_data,     "test_data.rds")
saveRDS(feature_cols,  "feature_cols.rds")

cat("\n✓ Datasets saved: eco_data_raw.rds, eco_data_norm.rds, train_data.rds, test_data.rds\n")
