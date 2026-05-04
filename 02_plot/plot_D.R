#############################################################
# Author: Kamila Janmohamed
# Date: 2026-05-03
# Description: Plot Financial Distress Exhibits (Set D)
#############################################################

# Set WD ------------------------------------------------------------
source("00_setup/config.R")

# Load packages ------------------------------------------------------------
library(tidyverse)
library(kableExtra)

# Load data ------------------------------------------------------------
df <- read_rds("data/merged/merged_survey.rds") %>%
mutate(shock_incidence = lshock_size / lshock_hh_inc)

# Outcome labels ------------------------------------------------------------
outcome_labels <- c("Auto repossession notice", "Debt sent to collections", "Eviction/foreclosure notice","Utility shutoff notice",  "90+ days past due: loan/credit card", "90+ days past due: rent/mortgage", "No adverse financial outcome")

# outcome levels ------------------------------------------------------------
outcome_levels = c("repo_notice_6m", "collections_6m", "eviction_notice_6m", "utility_cutoff_6m", "pastdue_lcc_6m", "pastdue_rm_6m", "no_adverse_outcome_6m")

# Fig D.1: Overall share of respondents with various outcomes ------------------------------------------------------------
d1_data <- df %>% 
select(contains("_6m")) %>% 
select(-c("fdi_6m")) %>%
# pivot_longer
pivot_longer(everything(), names_to = "outcome", values_to = "value") %>%
filter(!is.na(value)) %>%
group_by(outcome) %>%
summarise(share = mean(value)) %>%
mutate(label = scales::percent(share, accuracy = 0.1),
        outcome = factor(outcome, levels = outcome_levels, labels = outcome_labels, ordered = TRUE)) %>%
        arrange(outcome)

# Table D.2: Probability of shock by incidence quartile ------------------------------------------------------------
tab_d2 <- df %>% 
select(contains("_6m"), shock_incidence) %>%
select(-c("fdi_6m")) %>%
# generate shock incidence quartile
filter(!is.na(shock_incidence)) %>%
mutate(shock_incidence_q = ntile(shock_incidence, 4),
         shock_incidence_q = factor(shock_incidence_q,
                           levels = 1:4,
                           labels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)"))) %>%
# pivot_longer
pivot_longer(cols = contains("_6m"), names_to = "outcome", values_to = "value") %>%
filter(!is.na(value)) %>%
group_by(shock_incidence_q, outcome) %>%
summarise(share = mean(value)) %>%
mutate(label = scales::percent(share, accuracy = 0.1),
        outcome = factor(outcome, levels = outcome_levels, labels = outcome_labels, ordered = TRUE)) %>%
#pivot_wider
pivot_wider(names_from = shock_incidence_q, values_from = label, id_cols = outcome) %>%
arrange(outcome)


kable(tab_d2,
      format    = "latex",
      booktabs  = TRUE,
      col.names = c("Adverse Financial Outcome", names(tab_d2)[-1]),
      caption   = "Share of Shock Victims Reporting Adverse Financial Outcomes Within 6 Months of Shock by Shock Incidence Quartile",
      label     = "outcome_by_shock_incidence") %>%
  kable_styling(latex_options = c("hold_position")) %>%
  cat(file = "output/tables/tab_d2_adverse_outcome_by_shock_incidence.tex")


# Table D3.1: Probability of outcome by HH income ------------------------------------------------------------
tab_d3.1 <- df %>% 
select(contains("_6m"), lshock_hh_inc) %>%
select(-c("fdi_6m")) %>%
# generate shock incidence quartile
filter(!is.na(lshock_hh_inc)) %>%
mutate(lshock_hh_inc_q = ntile(lshock_hh_inc, 4),
         lshock_hh_inc_q = factor(lshock_hh_inc_q,
                           levels = 1:4,
                           labels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)"))) %>%
# pivot_longer
pivot_longer(cols = contains("_6m"), names_to = "outcome", values_to = "value") %>%
filter(!is.na(value)) %>%
group_by(lshock_hh_inc_q, outcome) %>%
summarise(share = mean(value)) %>%
mutate(label = scales::percent(share, accuracy = 0.1),
        outcome = factor(outcome, levels = outcome_levels, labels = outcome_labels, ordered = TRUE)) %>%
#pivot_wider
pivot_wider(names_from = lshock_hh_inc_q, values_from = label, id_cols = outcome) %>%
arrange(outcome)


kable(tab_d3.1,
      format    = "latex",
      booktabs  = TRUE,
      col.names = c("Adverse Financial Outcome", names(tab_d3.1)[-1]),
      caption   = "Share of Shock Victims Reporting Adverse Financial Outcomes Within 6 Months of Shock by HH Income Quartile",
      label     = "outcome_by_income") %>%
  kable_styling(latex_options = c("hold_position")) %>%
  cat(file = "output/tables/tab_d3.1_adverse_outcome_by_income.tex")

# Table D3.2: Probability of outcome by cash buffer ------------------------------------------------------------
tab_d3.2 <- df %>% 
select(contains("_6m"), hh_liquidity) %>%
select(-c("fdi_6m")) %>%
# generate shock incidence quartile
filter(!is.na(hh_liquidity)) %>%
mutate(hh_liquidity_q = ntile(hh_liquidity, 4),
         hh_liquidity_q = factor(hh_liquidity_q,
                           levels = 1:4,
                           labels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)"))) %>%
# pivot_longer
pivot_longer(cols = contains("_6m"), names_to = "outcome", values_to = "value") %>%
filter(!is.na(value)) %>%
group_by(hh_liquidity_q, outcome) %>%
summarise(share = mean(value)) %>%
mutate(label = scales::percent(share, accuracy = 0.1),
        outcome = factor(outcome, levels = outcome_levels, labels = outcome_labels, ordered = TRUE)) %>%
#pivot_wider
pivot_wider(names_from = hh_liquidity_q, values_from = label, id_cols = outcome) %>%
arrange(outcome)


kable(tab_d3.2,
      format    = "latex",
      booktabs  = TRUE,
      col.names = c("Adverse Financial Outcome", names(tab_d3.2)[-1]),
      caption   = "Share of Shock Victims Reporting Adverse Financial Outcomes Within 6 Months of Shock by Cash Buffer",
      label     = "outcome_by_cash_buffer") %>%
  kable_styling(latex_options = c("hold_position")) %>%
  cat(file = "output/tables/tab_d3.2_adverse_outcome_by_cash_buffer.tex")

# Table D3.3: Probability of outcome by credit access------------------------------------------------------------
tab_d3.3 <- df %>% 
select(contains("_6m"), hh_credit) %>%
select(-c("fdi_6m")) %>%
filter(!is.na(hh_credit)) %>%
# pivot_longer
pivot_longer(cols = contains("_6m"), names_to = "outcome", values_to = "value") %>%
filter(!is.na(value)) %>%
group_by(hh_credit, outcome) %>%
summarise(share = mean(value)) %>%
mutate(label = scales::percent(share, accuracy = 0.1),
        outcome = factor(outcome, levels = outcome_levels, labels = outcome_labels, ordered = TRUE)) %>%
#pivot_wider
pivot_wider(names_from = hh_credit, values_from = label, id_cols = outcome) %>%
arrange(outcome)


kable(tab_d3.3,
      format    = "latex",
      booktabs  = TRUE,
      col.names = c("Adverse Financial Outcome", names(tab_d3.3)[-1]),
      caption   = "Share of Shock Victims Reporting Adverse Financial Outcomes Within 6 Months of Shock by Credit Access",
      label     = "outcome_by_credit_access") %>%
  kable_styling(latex_options = c("hold_position")) %>%
  cat(file = "output/tables/tab_d3.3_adverse_outcome_by_credit_access.tex")
