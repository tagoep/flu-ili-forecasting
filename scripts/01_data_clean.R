####################################################
# ============================================================
# Forecasting U.S. ILI Trends Using ARIMA and ETS Models
# Data Cleaning and Preparation
# ============================================================
library(readr)
library(dplyr)
library(lubridate)

# Load data
ili_raw <- read_csv("ILINet.csv", skip = 1, show_col_types = FALSE)

# Clean and prepare
ili_clean <- ili_raw %>%
  filter(`REGION TYPE` == "National") %>%
  select(YEAR, WEEK, ILI = `% WEIGHTED ILI`) %>%
  filter(!is.na(ILI)) %>%
  arrange(YEAR, WEEK)

# Check for any missing values
cat("Missing values:", sum(is.na(ili_clean$ILI)), "\n")
cat("Date range:", ili_clean$YEAR[1], "Week", ili_clean$WEEK[1], 
    "to", tail(ili_clean$YEAR, 1), "Week", tail(ili_clean$WEEK, 1), "\n")
cat("Total weeks:", nrow(ili_clean), "\n")

# Preview
head(ili_clean, 10)

install.packages("MMWRweek")