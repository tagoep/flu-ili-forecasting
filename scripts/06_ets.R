# ============================================================
# Forecasting U.S. ILI Trends Using ARIMA and ETS Models
# ETS Modeling using STL Decomposition
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
train_ts <- window(ili_ts, end = c(2023, 52))
test_ts  <- window(ili_ts, start = c(2024, 1))

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

# ── Why stlf() instead of ets() ──────────────────────────────
# ets() cannot handle frequency > 24
# stlf() works in two stages:
#   Stage 1: STL decomposes the series into trend, season, remainder
#   Stage 2: ETS is applied to the seasonally adjusted remainder
#   Stage 3: Seasonality is added back to produce final forecast
# This is the recommended approach for weekly data

# ── Model 1: STL + Auto ETS ──────────────────────────────────
cat("=== Fitting STL + Auto ETS ===\n")

stl_ets_auto <- stlf(train_ts,
                     h = length(test_ts),
                     method = "ets",
                     s.window = "periodic")

cat("\n--- STL + Auto ETS Information Criteria ---\n")
cat("AIC:", round(stl_ets_auto$model$aic, 2), "\n")

# ── Model 2: STL + ARIMA ─────────────────────────────────────
cat("\n=== Fitting STL + ARIMA ===\n")

stl_arima <- stlf(train_ts,
                  h = length(test_ts),
                  method = "arima",
                  s.window = "periodic")


# ── STL Decomposition Plot ────────────────────────────────────
cat("\n=== Saving STL Decomposition Plot ===\n")

png("outputs/06_stl_decomposition.png",
    width = 1200, height = 900, res = 150)
plot(stl(train_ts, s.window = "periodic"),
     main = "STL Decomposition of ILI Series")
dev.off()
cat("Plot saved: outputs/06_stl_decomposition.png\n")

# ── Accuracy metrics on test set ─────────────────────────────
cat("\n=== Forecast Accuracy on Test Set ===\n")

cat("\n--- STL + Auto ETS ---\n")
metrics_stl_ets <- calc_metrics(as.numeric(test_ts),
                                as.numeric(stl_ets_auto$mean))

cat("\n--- STL + ARIMA ---\n")
metrics_stl_arima <- calc_metrics(as.numeric(test_ts),
                                  as.numeric(stl_arima$mean))

# ── Refit Auto ARIMA for full comparison ─────────────────────
cat("\n=== Refitting Auto ARIMA for comparison ===\n")
sarima_auto <- auto.arima(train_ts,
                          seasonal = TRUE,
                          stepwise = TRUE,
                          approximation = TRUE,
                          trace = FALSE)

fc_arima <- forecast(sarima_auto, h = length(test_ts))

metrics_arima <- calc_metrics(as.numeric(test_ts),
                              as.numeric(fc_arima$mean))

# ── Full comparison table ─────────────────────────────────────
cat("\n=== Full Model Comparison Table ===\n")
full_comparison <- data.frame(
  Model = c(
    paste("Auto ARIMA:", as.character(sarima_auto)),
    "STL + Auto ETS",
    "STL + ARIMA"
  ),
  AIC  = round(c(AIC(sarima_auto),
                 stl_ets_auto$model$aic,
                 NA), 2),
  RMSE = round(c(metrics_arima$RMSE,
                 metrics_stl_ets$RMSE,
                 metrics_stl_arima$RMSE), 4),
  MAE  = round(c(metrics_arima$MAE,
                 metrics_stl_ets$MAE,
                 metrics_stl_arima$MAE), 4),
  MSE  = round(c(metrics_arima$MSE,
                 metrics_stl_ets$MSE,
                 metrics_stl_arima$MSE), 4),
  MAPE = round(c(metrics_arima$MAPE,
                 metrics_stl_ets$MAPE,
                 metrics_stl_arima$MAPE), 4),
  MSPE = round(c(metrics_arima$MSPE,
                 metrics_stl_ets$MSPE,
                 metrics_stl_arima$MSPE), 4)
)
print(full_comparison)

# ── Plot: All models vs Actual ────────────────────────────────
test_dates <- ili_clean %>%
  filter(date >= as.Date("2024-01-01")) %>%
  pull(date)

test_df <- data.frame(
  date       = test_dates[1:length(test_ts)],
  actual     = as.numeric(test_ts),
  arima      = as.numeric(fc_arima$mean),
  stl_ets    = as.numeric(stl_ets_auto$mean),
  stl_arima  = as.numeric(stl_arima$mean),
  lo95       = pmax(as.numeric(stl_ets_auto$lower[, 2]), 0),
  hi95       = as.numeric(stl_ets_auto$upper[, 2]),
  lo80       = pmax(as.numeric(stl_ets_auto$lower[, 1]), 0),
  hi80       = as.numeric(stl_ets_auto$upper[, 1])
)

ggplot(test_df, aes(x = date)) +
  geom_ribbon(aes(ymin = lo95, ymax = hi95),
              fill = "#1b7837", alpha = 0.1) +
  geom_ribbon(aes(ymin = lo80, ymax = hi80),
              fill = "#1b7837", alpha = 0.2) +
  geom_line(aes(y = actual,    color = "Actual"),
            linewidth = 0.9) +
  geom_line(aes(y = arima,     color = "Auto ARIMA"),
            linewidth = 0.7, linetype = "longdash") +
  geom_line(aes(y = stl_ets,   color = "STL + ETS"),
            linewidth = 0.7, linetype = "dashed") +
  geom_line(aes(y = stl_arima, color = "STL + ARIMA"),
            linewidth = 0.7, linetype = "dotted") +
  scale_color_manual(values = c(
    "Actual"     = "black",
    "Auto ARIMA" = "#d6604d",
    "STL + ETS"  = "#1b7837",
    "STL + ARIMA"= "#762a83"
  )) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  labs(title = "All Models Forecast vs Actual ILI — Test Set 2024 Onwards",
       subtitle = "Shaded regions show 80% and 95% prediction intervals (STL + ETS)",
       x = "Date", y = "% ILI Visits",
       color = "Model",
       caption = "Source: CDC FluView ILINet") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

ggsave("outputs/07_all_models_forecast.png",
       width = 12, height = 5, dpi = 150)

cat("\nPlot saved: outputs/07_all_models_forecast.png\n")