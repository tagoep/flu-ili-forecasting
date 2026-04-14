# ============================================================
# Forecasting U.S. ILI Trends Using ARIMA and ETS Models
# Final Forecast — 52 Weeks Ahead
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

# ── Build time series object on FULL dataset ─────────────────
# We now retrain on ALL available data (1997-2026)
# This gives the model maximum information before forecasting
ili_ts <- ts(ili_clean$ILI,
             start = c(1997, 40),
             frequency = 52)

cat("Full series length:", length(ili_ts), "weeks\n")
cat("Forecasting 52 weeks ahead from:",
    format(max(ili_clean$date), "%B %d, %Y"), "\n")

# ── Fit winning model on full data ───────────────────────────
# STL + ARIMA won on RMSE, MAE, MSE, MAPE and MSPE
cat("\n=== Fitting STL + ARIMA on Full Dataset ===\n")

final_forecast <- stlf(ili_ts,
                       h = 52,
                       method = "arima",
                       s.window = "periodic")



# ── Also fit STL + ETS for comparison on final forecast ──────
final_ets <- stlf(ili_ts,
                  h = 52,
                  method = "ets",
                  s.window = "periodic")

# ── Build forecast date sequence ─────────────────────────────
last_date    <- max(ili_clean$date)
future_dates <- seq(last_date + 7, by = "week", length.out = 52)

# ── Build forecast dataframe ──────────────────────────────────
forecast_df <- data.frame(
  date      = future_dates,
  arima_fc  = as.numeric(final_forecast$mean),
  ets_fc    = as.numeric(final_ets$mean),
  lo80      = pmax(as.numeric(final_forecast$lower[, 1]), 0),
  hi80      = as.numeric(final_forecast$upper[, 1]),
  lo95      = pmax(as.numeric(final_forecast$lower[, 2]), 0),
  hi95      = as.numeric(final_forecast$upper[, 2])
)


# ── Recent history for context (last 2 years) ────────────────
recent_df <- ili_clean %>%
  filter(date >= as.Date("2024-01-01"))

# ── Plot 1: Final forecast with recent history ────────────────
ggplot() +
  geom_ribbon(data = forecast_df,
              aes(x = date, ymin = lo95, ymax = hi95),
              fill = "#762a83", alpha = 0.12) +
  geom_ribbon(data = forecast_df,
              aes(x = date, ymin = lo80, ymax = hi80),
              fill = "#762a83", alpha = 0.22) +
  geom_line(data = recent_df,
            aes(x = date, y = ILI, color = "Actual (Recent)"),
            linewidth = 0.9) +
  geom_line(data = forecast_df,
            aes(x = date, y = arima_fc, color = "STL + ARIMA Forecast"),
            linewidth = 0.8, linetype = "dashed") +
  geom_line(data = forecast_df,
            aes(x = date, y = ets_fc, color = "STL + ETS Forecast"),
            linewidth = 0.7, linetype = "dotted") +
  geom_vline(xintercept = as.numeric(last_date),
             linetype = "solid", color = "gray40",
             linewidth = 0.5) +
  annotate("text", x = last_date - 30, y = 7.5,
           label = "← Observed", size = 3,
           color = "gray40", hjust = 1) +
  annotate("text", x = last_date + 30, y = 7.5,
           label = "Forecast →", size = 3,
           color = "gray40", hjust = 0) +
  scale_color_manual(values = c(
    "Actual (Recent)"      = "black",
    "STL + ARIMA Forecast" = "#762a83",
    "STL + ETS Forecast"   = "#1b7837"
  )) +
  scale_x_date(
    breaks = seq(as.Date("2024-01-01"), 
                 as.Date("2027-06-01"), by = "3 months"),
    date_labels = "%b %Y"
  ) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(title = "U.S. ILI 52-Week Forecast — STL + ARIMA vs STL + ETS",
       subtitle = "Shaded regions show 80% and 95% prediction intervals (STL + ARIMA)",
       x = "Date", y = "% ILI Visits",
       color = "Series",
       caption = "Source: CDC FluView ILINet | Models: STL + ARIMA, STL + ETS") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

ggsave("outputs/08_final_forecast.png",
       width = 12, height = 5, dpi = 150)

# ── Plot 2: Full history + forecast ───────────────────────────
full_df <- ili_clean %>% select(date, ILI)

ggplot() +
  geom_ribbon(data = forecast_df,
              aes(x = date, ymin = lo95, ymax = hi95),
              fill = "#762a83", alpha = 0.12) +
  geom_ribbon(data = forecast_df,
              aes(x = date, ymin = lo80, ymax = hi80),
              fill = "#762a83", alpha = 0.22) +
  geom_line(data = full_df,
            aes(x = date, y = ILI),
            color = "#2166ac", linewidth = 0.5) +
  geom_line(data = forecast_df,
            aes(x = date, y = arima_fc),
            color = "#762a83", linewidth = 0.9,
            linetype = "dashed") +
  geom_vline(xintercept = as.numeric(last_date),
             linetype = "solid", color = "gray40",
             linewidth = 0.5) +
  annotate("text", x = as.Date("2025-06-01"), y = 8,
           label = "Forecast →", size = 3,
           color = "gray40", hjust = 0) +
  scale_x_date(
    breaks = seq(as.Date("1997-01-01"),
                 as.Date("2027-01-01"), by = "2 years"),
    date_labels = "%Y"
  ) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(title = "U.S. ILI Weekly Visits 1997–2027 with 52-Week Forecast",
       subtitle = "Blue = observed history | Purple dashed = STL + ARIMA forecast | Shaded = prediction intervals",
       x = "Year", y = "% ILI Visits",
       caption = "Source: CDC FluView ILINet") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("outputs/09_full_history_forecast.png",
       width = 14, height = 5, dpi = 150)

# ── Save forecast table as CSV ────────────────────────────────
forecast_df %>%
  mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
  write.csv("outputs/forecast_52weeks.csv", row.names = FALSE)


