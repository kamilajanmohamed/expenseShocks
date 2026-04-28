#############################################################
# Author: Kamila Janmohamed
# Date: 2026-04-28
# Description: Plot exhibit set E
        # E1. Comparison of future belief vs. prob of expense risk
        # E2. Comparison of coping mechanisms with ex. ante q about liquidity

#############################################################

# Set WD ------------------------------------------------------------
source("00_setup/config.R")

# Load packages ------------------------------------------------------------
library(tidyverse)
library(kableExtra)

# Load data ------------------------------------------------------------
df <- read_csv("data/merged/merged_survey.csv")

# Table E1: Comparison of future belief vs. prob of expense risk ------------------------------------------------------------
table(df$lshock_pred, df$shock_forecast_bin)

cross_tab <- df %>%
  count(lshock_pred, shock_forecast_bin) %>%
  group_by(lshock_pred) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup() %>%
  select(lshock_pred, forecast_bin, pct) %>%
  pivot_wider(names_from = forecast_bin, values_from = pct, values_fill = 0) %>%
  left_join(row_ns, by = "lshock_pred") %>%
  select(lshock_pred, N, all_of(forecast_labels))

cross_tab %>%
  gt(rowname_col = "lshock_pred") %>%
  tab_stubhead(label = "Retrospective belief") %>%
  tab_spanner(
    label = "Ex-ante forecast (shock_forecast)",
    columns = all_of(forecast_labels)
  ) %>%
  fmt_number(columns = all_of(forecast_labels), decimals = 1, suffix = "%") %>%
  tab_header(
    title    = "Ex-ante forecast vs. retrospective belief about expense shock",
    subtitle = "Row percentages within each retrospective belief category"
  ) %>%
  tab_footnote("Forecast bins: 0% = [0, 12.5%), 25% = [12.5%, 37.5%), 50% = [37.5%, 62.5%), 75% = [62.5%, 87.5%), 100% = [87.5%, 100%]") %>%
  gtsave("03_output/figures/E1_forecast_vs_pred.html")

table(df$lshock_pred)
