# ============================================================
# Forecasting U.S. ILI Trends Using ARIMA and ETS Models
# Stationarity Testing
# ============================================================

library(readr)
library(dplyr)
library(MMWRweek)
library(tseries)
library(forecast)

# ── Load and clean data ──────────────────────────────────────
ili_raw <- read_csv("ILINet.csv", skip = 1, show_col_types = FALSE)

ili_clean <- ili_raw %>%
  filter(`REGION TYPE` == "National") %>%
  select(YEAR, WEEK, ILI = `% WEIGHTED ILI`) %>%
  filter(!is.na(ILI)) %>%
  arrange(YEAR, WEEK) %>%
  mutate(date = MMWRweek2Date(YEAR, WEEK))

# ── Build the time series object ─────────────────────────────
# frequency = 52 because we have weekly data (52 weeks per year)
ili_ts <- ts(ili_clean$ILI, 
             start = c(1997, 40), 
             frequency = 52)

cat("Time series object created\n")
cat("Start:", start(ili_ts), "\n")
cat("End:", end(ili_ts), "\n")
cat("Frequency:", frequency(ili_ts), "\n")
cat("Length:", length(ili_ts), "\n")

# ── Test 1: Augmented Dickey-Fuller (ADF) Test ───────────────
# H0: The series has a unit root (non-stationary)
# H1: The series is stationary
# If p-value < 0.05 we reject H0 and conclude stationarity
cat("\n=== Augmented Dickey-Fuller Test (Original Series) ===\n")
adf_original <- adf.test(ili_ts)
print(adf_original)

# ── Test 2: KPSS Test ────────────────────────────────────────
# H0: The series is stationary (opposite of ADF)
# H1: The series is non-stationary
# If p-value < 0.05 we reject H0 and conclude non-stationarity
cat("\n=== KPSS Test (Original Series) ===\n")
kpss_original <- kpss.test(ili_ts)
print(kpss_original)

# ── Check how many differences are needed ────────────────────
cat("\n=== Number of Differences Required ===\n")
cat("Regular differences needed:", ndiffs(ili_ts), "\n")
cat("Seasonal differences needed:", nsdiffs(ili_ts), "\n")