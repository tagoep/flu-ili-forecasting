# ============================================================
# Forecasting U.S. ILI Trends Using ARIMA and ETS Models
# ACF and PACF Plots
# ============================================================

library(readr)
library(dplyr)
library(MMWRweek)
library(forecast)
library(ggplot2)

# ── Load and clean data ──────────────────────────────────────
ili_raw <- read_csv("ILINet.csv", skip = 1, show_col_types = FALSE)

ili_clean <- ili_raw %>%
  filter(`REGION TYPE` == "National") %>%
  select(YEAR, WEEK, ILI = `% WEIGHTED ILI`) %>%
  filter(!is.na(ILI)) %>%
  arrange(YEAR, WEEK) %>%
  mutate(date = MMWRweek2Date(YEAR, WEEK))

# ── Build time series object ─────────────────────────────────
ili_ts <- ts(ili_clean$ILI,
             start = c(1997, 40),
             frequency = 52)

# ── Apply differencing ───────────────────────────────────────
# First apply seasonal difference (lag 52) then regular difference
ili_ts_sdiff  <- diff(ili_ts, lag = 52)        # seasonal difference
ili_ts_diff   <- diff(ili_ts_sdiff, lag = 1)   # regular difference

cat("Original series length:", length(ili_ts), "\n")
cat("After seasonal diff length:", length(ili_ts_sdiff), "\n")
cat("After both diffs length:", length(ili_ts_diff), "\n")

# ── Plot 1: Original vs Differenced Series ───────────────────
png("outputs/02_differenced_series.png", width = 1400, height = 800, res = 150)
par(mfrow = c(3, 1), mar = c(3, 4, 3, 2))

plot(ili_ts, main = "Original ILI Series",
     ylab = "% ILI", col = "#2166ac", lwd = 0.8)

plot(ili_ts_sdiff, main = "After Seasonal Differencing (lag = 52)",
     ylab = "Seasonal Diff", col = "#1b7837", lwd = 0.8)

plot(ili_ts_diff, main = "After Seasonal + Regular Differencing",
     ylab = "Both Diffs", col = "#762a83", lwd = 0.8)

dev.off()
cat("Plot saved: outputs/02_differenced_series.png\n")

# ── Plot 2: ACF of differenced series ────────────────────────
png("outputs/03_acf_plot.png", width = 1200, height = 500, res = 150)
acf(ili_ts_diff, lag.max = 104, 
    main = "ACF — Seasonally and Regularly Differenced ILI Series",
    col = "#2166ac")
dev.off()
cat("Plot saved: outputs/03_acf_plot.png\n")

# ── Plot 3: PACF of differenced series ───────────────────────
png("outputs/04_pacf_plot.png", width = 1200, height = 500, res = 150)
pacf(ili_ts_diff, lag.max = 104,
     main = "PACF — Seasonally and Regularly Differenced ILI Series",
     col = "#d6604d")
dev.off()
cat("Plot saved: outputs/04_pacf_plot.png\n")

# ── Print numerical ACF/PACF values at key lags ──────────────
cat("\n=== ACF Values at Key Lags ===\n")
acf_vals <- acf(ili_ts_diff, lag.max = 104, plot = FALSE)
cat("Lag 1:", round(acf_vals$acf[2], 4), "\n")
cat("Lag 2:", round(acf_vals$acf[3], 4), "\n")
cat("Lag 3:", round(acf_vals$acf[4], 4), "\n")
cat("Lag 52:", round(acf_vals$acf[53], 4), "\n")
cat("Lag 53:", round(acf_vals$acf[54], 4), "\n")
cat("Lag 104:", round(acf_vals$acf[105], 4), "\n")

cat("\n=== PACF Values at Key Lags ===\n")
pacf_vals <- pacf(ili_ts_diff, lag.max = 104, plot = FALSE)
cat("Lag 1:", round(pacf_vals$acf[1], 4), "\n")
cat("Lag 2:", round(pacf_vals$acf[2], 4), "\n")
cat("Lag 3:", round(pacf_vals$acf[3], 4), "\n")
cat("Lag 52:", round(pacf_vals$acf[52], 4), "\n")
cat("Lag 53:", round(pacf_vals$acf[53], 4), "\n")