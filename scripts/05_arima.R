############################################################
# ============================================================
# Forecasting U.S. ILI Trends Using ARIMA and ETS Models
# ARIMA Modeling — Fitting, Diagnostics & Evaluation
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

# ── Train/test split ─────────────────────────────────────────
# Train: 1997-2023 | Test: 2024 (52 weeks held out)
train_ts <- window(ili_ts, end = c(2023, 52))
test_ts  <- window(ili_ts, start = c(2024, 1))

cat("Training set:", length(train_ts), "weeks\n")
cat("Test set:", length(test_ts), "weeks\n")

# ── Custom accuracy metrics function ─────────────────────────
calc_metrics <- function(actual, forecast) {
  errors  <- actual - forecast
  mse     <- mean(errors^2)
  rmse    <- sqrt(mse)
  mae     <- mean(abs(errors))
  mape    <- mean(abs(errors / actual)) * 100
  mspe    <- mean((errors / actual)^2) * 100
  
  cat("  RMSE:", round(rmse, 4), "\n")
  cat("  MAE: ", round(mae,  4), "\n")
  cat("  MSE: ", round(mse,  4), "\n")
  cat("  MAPE:", round(mape, 4), "%\n")
  cat("  MSPE:", round(mspe, 4), "%\n")
  
  return(data.frame(RMSE = rmse, MAE = mae, 
                    MSE = mse, MAPE = mape, MSPE = mspe))
}

# ── Model 1: Manual SARIMA(1,1,2)(1,1,1)[52] ─────────────────
cat("\n=== Fitting Manual SARIMA(1,1,2)(1,1,1)[52] ===\n")
cat("Please wait — this may take a few minutes...\n")

sarima_manual <- Arima(train_ts,
                       order = c(1, 1, 2),
                       seasonal = list(order = c(1, 1, 1),
                                       period = 52))


cat("\n--- Manual SARIMA Information Criteria ---\n")
cat("AIC:", round(AIC(sarima_manual), 2), "\n")
cat("BIC:", round(BIC(sarima_manual), 2), "\n")
cat("Log Likelihood:", round(sarima_manual$loglik, 2), "\n")

# ── Model 2: Auto ARIMA ───────────────────────────────────────
cat("\n=== Fitting Auto ARIMA ===\n")
cat("Please wait — searching over model orders...\n")

sarima_auto <- auto.arima(train_ts,
                          seasonal = TRUE,
                          stepwise = TRUE,
                          approximation = TRUE,
                          trace = FALSE)

cat("Done!\n")
cat("\nAuto ARIMA selected:", as.character(sarima_auto), "\n")
cat("\n--- Auto ARIMA Information Criteria ---\n")
cat("AIC:", round(AIC(sarima_auto), 2), "\n")
cat("BIC:", round(BIC(sarima_auto), 2), "\n")
cat("Log Likelihood:", round(sarima_auto$loglik, 2), "\n")

# ── Forecast on test set ──────────────────────────────────────
fc_manual <- forecast(sarima_manual, h = length(test_ts))
fc_auto   <- forecast(sarima_auto,   h = length(test_ts))

# ── Accuracy metrics on test set ─────────────────────────────
cat("\n=== Forecast Accuracy on Test Set (2024) ===\n")

cat("\n--- Manual SARIMA(1,1,2)(1,1,1)[52] ---\n")
metrics_manual <- calc_metrics(as.numeric(test_ts),
                               as.numeric(fc_manual$mean))

cat("\n--- Auto ARIMA ---\n")
metrics_auto <- calc_metrics(as.numeric(test_ts),
                             as.numeric(fc_auto$mean))

# ── Summary comparison table ──────────────────────────────────
cat("\n=== Model Comparison Summary ===\n")
comparison <- data.frame(
  Model   = c("Manual SARIMA(1,1,2)(1,1,1)[52]",
              paste("Auto ARIMA", as.character(sarima_auto))),
  AIC     = round(c(AIC(sarima_manual), AIC(sarima_auto)), 2),
  BIC     = round(c(BIC(sarima_manual), BIC(sarima_auto)), 2),
  RMSE    = round(c(metrics_manual$RMSE, metrics_auto$RMSE), 4),
  MAE     = round(c(metrics_manual$MAE,  metrics_auto$MAE),  4),
  MSE     = round(c(metrics_manual$MSE,  metrics_auto$MSE),  4),
  MAPE    = round(c(metrics_manual$MAPE, metrics_auto$MAPE), 4),
  MSPE    = round(c(metrics_manual$MSPE, metrics_auto$MSPE), 4)
)
print(comparison)

# ── Plot: Forecast vs Actual on test set ─────────────────────
test_dates <- ili_clean %>%
  filter(YEAR >= 2024) %>%
  pull(date)

test_df <- data.frame(
  date     = test_dates[1:length(test_ts)],
  actual   = as.numeric(test_ts),
  manual   = as.numeric(fc_manual$mean),
  auto     = as.numeric(fc_auto$mean),
  lo80     = as.numeric(fc_manual$lower[, 1]),
  hi80     = as.numeric(fc_manual$upper[, 1]),
  lo95     = as.numeric(fc_manual$lower[, 2]),
  hi95     = as.numeric(fc_manual$upper[, 2])
)

ggplot(test_df, aes(x = date)) +
  geom_ribbon(aes(ymin = lo95, ymax = hi95),
              fill = "#2166ac", alpha = 0.1) +
  geom_ribbon(aes(ymin = lo80, ymax = hi80),
              fill = "#2166ac", alpha = 0.2) +
  geom_line(aes(y = actual, color = "Actual"),
            linewidth = 0.8) +
  geom_line(aes(y = manual, color = "Manual SARIMA"),
            linewidth = 0.7, linetype = "dashed") +
  geom_line(aes(y = auto, color = "Auto ARIMA"),
            linewidth = 0.7, linetype = "dotted") +
  scale_color_manual(values = c("Actual" = "black",
                                "Manual SARIMA" = "#2166ac",
                                "Auto ARIMA" = "#d6604d")) +
  labs(title = "ARIMA Forecast vs Actual ILI — Test Set 2024",
       subtitle = "Shaded regions show 80% and 95% prediction intervals",
       x = "Date", y = "% ILI Visits",
       color = "Series",
       caption = "Source: CDC FluView ILINet") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom")

ggsave("outputs/05_arima_forecast_vs_actual.png",
       width = 12, height = 5, dpi = 150)
