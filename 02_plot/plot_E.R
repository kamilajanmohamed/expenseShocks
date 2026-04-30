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

# Table E1.1: Retrospective shock risk by income ------------------------------------------------------------
tab_e1.1 <- df %>%
  filter(!is.na(lshock_hh_inc), !is.na(lshock_pred)) %>%
  mutate(income_q = ntile(lshock_hh_inc, 4),
         income_q = factor(income_q,
                           levels = 1:4,
                           labels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)"))) %>%
  group_by(income_q) %>%
  count(lshock_pred) %>%
  mutate(pct = n / sum(n)) %>%
  #mutate(lshock_prob = factor(lshock_type, levels = shock_levels)) %>%
  select(income_q, lshock_pred, pct) %>%
  mutate(pct = scales::percent(pct, accuracy = 1)) %>%
  pivot_wider(names_from = lshock_pred, values_from = pct, values_fill = "0%") %>%
    arrange(income_q)

  tab_e1.1 %>%
   kable(
    format   = "latex",
    booktabs = TRUE,
    caption  = "Retrospective Expense Shock Risk by Household Income Quartile",
    label    = "tab:retrospective_shock_risk_by_income",
    col.names = c("Group", colnames(tab_e1.1)[-1])
  ) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  save_kable(file = "output/tables/tab_e1.1.tex")

# Table E1.2: Retrospective shock risk by education level ------------------------------------------------------------
tab_e1.2 <- df %>%
  filter(!is.na(education), !is.na(lshock_pred)) %>%
  group_by(education) %>%
  count(lshock_pred) %>%
  mutate(pct = n / sum(n)) %>%
  #mutate(lshock_prob = factor(lshock_type, levels = shock_levels)) %>%
  select(education, lshock_pred, pct) %>%
  mutate(pct = scales::percent(pct, accuracy = 1)) %>%
  pivot_wider(names_from = lshock_pred, values_from = pct, values_fill = "0%") %>%
  arrange(education)

  tab_e1.2 %>%
   kable(
    format   = "latex",
    booktabs = TRUE,
    caption  = "Retrospective Expense Shock Risk by Household Income Quartile",
    label    = "tab:retrospective_shock_risk_by_education",
    col.names = c("Group", colnames(tab_e1.2)[-1])
  ) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  save_kable(file = "output/tables/tab_e1.2.tex")

# Table E2: Comparison of future belief vs. prob of expense risk ------------------------------------------------------------
tab_e2 <- df %>%
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
