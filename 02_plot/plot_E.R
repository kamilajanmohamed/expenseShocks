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
df <- read_rds("data/merged/merged_survey.rds")

# Table E1: Comparison of future belief vs. prob of expense risk ------------------------------------------------------------
cross_tab <- df %>%
  count(shock_forecast_bin, lshock_pred) %>%
  na.omit() %>%
  group_by(lshock_pred) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup() %>%
  select(shock_forecast_bin, lshock_pred, pct) %>%
  pivot_wider(names_from = lshock_pred, values_from = pct, values_fill = 0)

cross_tab %>%
  mutate(across(-shock_forecast_bin, ~paste0(formatC(., format = "f", digits = 1), "\\%"))) %>%
  kable(
    format   = "latex",
    booktabs = TRUE,
    caption  = "Future shock risk vs.\ Retrospective shock risk (\\% within retrospective shock risk bin)",
    label    = "tab:cross_tab_E1",
    col.names = c("Forecast bin", colnames(cross_tab)[-1])
  ) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  save_kable(file = "output/tables/tab_e1.tex")
