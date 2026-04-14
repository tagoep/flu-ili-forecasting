
####################################################
# ============================================================
# Forecasting U.S. ILI Trends Using ARIMA and ETS Models
# Step 2: EDA 
# ============================================================

library(readr)
library(dplyr)
library(ggplot2)
library(MMWRweek)

# ── Load and clean data ──────────────────────────────────────
ili_raw <- read_csv("ILINet.csv", skip = 1, show_col_types = FALSE)

ili_clean <- ili_raw %>%
  filter(`REGION TYPE` == "National") %>%
  select(YEAR, WEEK, ILI = `% WEIGHTED ILI`) %>%
  filter(!is.na(ILI)) %>%
  arrange(YEAR, WEEK) %>%
  mutate(date = MMWRweek2Date(YEAR, WEEK))

# ── Plot 1: Full ILI time series with all years labeled ──────
ggplot(ili_clean, aes(x = date, y = ILI)) +
  geom_line(color = "#2166ac", linewidth = 0.5) +
  geom_hline(yintercept = mean(ili_clean$ILI),
             linetype = "dashed", color = "red", linewidth = 0.4) +
  annotate("text", x = as.Date("1999-01-01"),
           y = mean(ili_clean$ILI) + 0.3,
           label = paste("Mean ILI:", round(mean(ili_clean$ILI), 2), "%"),
           color = "red", size = 3) +
  annotate("rect", xmin = as.Date("2009-04-01"),
           xmax = as.Date("2010-04-01"),
           ymin = -Inf, ymax = Inf,
           alpha = 0.15, fill = "orange") +
  annotate("text", x = as.Date("2009-10-01"), y = 8.8,
           label = "H1N1", size = 2.8, color = "darkorange") +
  annotate("rect", xmin = as.Date("2020-03-01"),
           xmax = as.Date("2021-09-01"),
           ymin = -Inf, ymax = Inf,
           alpha = 0.15, fill = "purple") +
  annotate("text", x = as.Date("2020-10-01"), y = 8.8,
           label = "COVID-19", size = 2.8, color = "purple") +
  scale_x_date(
    breaks = seq(as.Date("1997-01-01"), as.Date("2026-01-01"), by = "1 year"),
    date_labels = "%Y",
    expand = c(0.01, 0)
  ) +
  labs(title = "U.S. Influenza-Like Illness (ILI) Weekly Visits, 1997–2026",
       subtitle = "Percentage of outpatient visits due to ILI — CDC ILINet Surveillance",
       x = "Year", y = "% ILI Visits",
       caption = "Source: CDC FluView ILINet") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8)
  )

ggsave("outputs/01_ili_full_timeseries.png", width = 14, height = 5, dpi = 150)