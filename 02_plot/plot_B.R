#############################################################
# Author: Kamila Janmohamed
# Date: 2026-04-23
# Description: Plot exhibit set B
#############################################################

# Set WD ------------------------------------------------------------
source("00_setup/config.R")

# Load packages ------------------------------------------------------------
library(tidyverse)
library(kableExtra)

# Load data ------------------------------------------------------------
df <- read_csv("data/merged/merged_survey.csv")

# Prep data ------------------------------------------------------------
# identify all cats that account for less than 3% of shocks and recategories into other
to_recat <- df %>%
  count(lshock_type) %>%
  na.omit() %>%
  mutate(
    pct = n / sum(n)) %>%
filter(pct < 0.03)  %>%
pull(lshock_type)

# recat obs
df <- df %>%
mutate(lshock_type = ifelse(lshock_type %in% to_recat, "Other", lshock_type))

# global shock category order (ascending count, Other always first)
shock_levels <- df %>%
  count(lshock_type) %>%
  na.omit() %>%
  mutate(lshock_type = fct_reorder(lshock_type, n)) %>%
  mutate(lshock_type = fct_relevel(lshock_type, "Other", after = 0)) %>%
  arrange(lshock_type) %>%
  pull(lshock_type) %>%
  as.character()

# B2. Shock Type Distribution------------------------------------------------------------
ex_b2 <- df %>%
  count(lshock_type) %>%
  na.omit() %>%
  mutate(pct = n / sum(n)) %>%
  mutate(lshock_type = fct_reorder(lshock_type, n)) %>%
  mutate(lshock_type = fct_relevel(lshock_type, "Other", after = 0)) %>%
  mutate(label = scales::percent(pct, accuracy = 1)) %>%
  ggplot(aes(x = pct, y = lshock_type, fill = lshock_type)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = label), hjust = -0.1, color = "black") +
  scale_x_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.1))) +
  ggtitle("Distribution of Shock Types") +
  xlab("Share of Reported Shocks") + 
  ylab("Type of Shock") + 
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text  = element_text(size = 13),
    axis.title = element_text(size = 14)
  )

# export figure
ggsave("output/figures/ex_b2.pdf", ex_b2, width = 7, height = 5, units = "in")


# B2.1 Table: Shock type distribution by income at time of shock ------------------------------------------------------------
tab_b2.1 <- df %>%
  filter(!is.na(lshock_hh_inc), !is.na(lshock_type)) %>%
  mutate(income_q = ntile(lshock_hh_inc, 4),
         income_q = factor(income_q,
                           levels = 1:4,
                           labels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)"))) %>%
  group_by(income_q) %>%
  count(lshock_type) %>%
  mutate(pct = n / sum(n)) %>%
  mutate(lshock_type = factor(lshock_type, levels = shock_levels)) %>%
  select(income_q, lshock_type, pct) %>%
  mutate(pct = scales::percent(pct, accuracy = 1)) %>%
  pivot_wider(names_from = income_q, values_from = pct, values_fill = "0%") %>%
  arrange(desc(lshock_type))

kable(tab_b2.1,
      format   = "latex",
      booktabs = TRUE,
      col.names = c("Shock Type", "Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)"),
      caption  = "Shock Type Distribution by Household Income Quartile",
      label    = "tab:shock_by_income") %>%
  kable_styling(latex_options = c("hold_position")) %>%
  cat(file = "output/tables/tab_b2.1_shock_by_income.tex")

# B2.2 Table: Shock type distribution by liquidity buffer at time of shock ------------------------------------------------------------
tab_b2.2 <- df %>%
  filter(!is.na(hh_liquidity), !is.na(lshock_type)) %>%
  mutate(
    hh_liquidity = factor(hh_liquidity, levels = c(
      "$0 - $100", "$100 - $500", "$500 - $1,000", "$1,000 - $2,500",
      "$2,500 - $5,000", "$5,000 - $7,500", "$7,500 - $10,000",
      "$10,000 - $20,000", "$20,000 - $50,000", "More than $50,000"
    )),
    liquidity_q = ntile(as.integer(hh_liquidity), 4),
    liquidity_q = factor(liquidity_q,
                         levels = 1:4,
                         labels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)"))
  ) %>%
  group_by(liquidity_q) %>%
  count(lshock_type) %>%
  mutate(pct = n / sum(n)) %>%
  mutate(lshock_type = factor(lshock_type, levels = shock_levels)) %>%
  select(liquidity_q, lshock_type, pct) %>%
  mutate(pct = scales::percent(pct, accuracy = 1)) %>%
  pivot_wider(names_from = liquidity_q, values_from = pct, values_fill = "0%") %>%
  arrange(desc(lshock_type))

kable(tab_b2.2,
      format    = "latex",
      booktabs  = TRUE,
      col.names = c("Shock Type", "Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)"),
      caption   = "Shock Type Distribution by Household Liquidity Buffer Quartile",
      label     = "tab:shock_by_liquidity") %>%
  kable_styling(latex_options = c("hold_position")) %>%
  cat(file = "output/tables/tab_b2.2_shock_by_liquidity.tex")

# B2.3 Shock type distribution by Home value at time of shock ------------------------------------------------------------
  # check correlations between current and lagged home values
  cor(na.omit(df$home_value_lshock_month), na.omit(df$home_value_lag_lshock_month))

# Approx 1. let's just use the contemporaneous home value

tab_b2.3 <- df %>%
  filter(!is.na(home_value_lshock_month), !is.na(lshock_type)) %>%
  mutate(home_value_q = ntile(as.integer(home_value_lshock_month), 4),
    home_value_q = factor(home_value_q,
                         levels = 1:4,
                         labels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)"))) %>%
  group_by(home_value_q) %>%
  count(lshock_type) %>%
  mutate(pct = n / sum(n)) %>%
  mutate(lshock_type = factor(lshock_type, levels = shock_levels)) %>%
  select(home_value_q, lshock_type, pct) %>%
  mutate(pct = scales::percent(pct, accuracy = 1)) %>%
  pivot_wider(names_from = home_value_q, values_from = pct, values_fill = "0%") %>%
  arrange(desc(lshock_type))

  kable(tab_b2.3,
      format    = "latex",
      booktabs  = TRUE,
      col.names = c("Shock Type", "Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)"),
      caption   = "Shock Type Distribution by Avg Zipcode Home Value Quartile",
      label     = "tab:shock_by_home_value") %>%
  kable_styling(latex_options = c("hold_position")) %>%
  cat(file = "output/tables/tab_b2.3_shock_by_home_value.tex")
