#############################################################
# Author: Kamila Janmohamed
# Date: 2026-04-23
# Description: Plot exhibit set E
#############################################################

# Set WD ------------------------------------------------------------
source("00_setup/config.R")

# Load packages ------------------------------------------------------------
library(tidyverse)
library(pals)

# Load data ------------------------------------------------------------
df <- read_csv("data/clean/surveyData.csv")

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
df %>%
  count(lshock_type) %>%
  na.omit() %>%
  mutate(
    pct = n / sum(n)) %>%
mutate(lshock_type = fct_reorder(lshock_type, n)) %>%
mutate(lshock_type = fct_relevel(lshock_type, "Other", after = 0)) %>%
arrange(desc(lshock_type)) %>%
mutate(
    label = scales::percent(pct, accuracy = 1),
    ypos = cumsum(n) - n / 2
  ) %>%
  ggplot(aes(x = 0.5, y = n, fill = lshock_type)) +
  geom_col(width = 1, color = "white", linewidth = 0.5) +
  coord_polar(theta = "y") +
  geom_text(aes(x = 0.85, y = ypos, label = label), size = 3.5, color = "white", fontface = "bold") +
  xlim(0, 1.5) +
  scale_fill_manual(values=rev(unname(tol()[1:9]))) + 
  #scale_fill_brewer(palette = "Blues", direction = -1) +
  guides(fill = guide_legend(reverse = TRUE, title = "Shock Type")) +
  theme_void()

# B2.1 Shock Type Distribution by Income ------------------------------------------------------------
df %>%
  filter(!is.na(income), !is.na(lshock_type)) %>%
  mutate(income = factor(income, levels = c(
    "- $20k", "$20k-40k", "$40k-60k", "$60k-80k", "$80k-100k", "$100k-125k", "$125k +"
  ))) %>%
  group_by(income) %>%
  count(lshock_type) %>%
  mutate(pct = n / sum(n)) %>%
  mutate(lshock_type = fct_reorder(lshock_type, n)) %>%
  mutate(lshock_type = fct_relevel(lshock_type, "Other", after = 0)) %>%
  arrange(income, desc(lshock_type)) %>%
  mutate(
    label = scales::percent(pct, accuracy = 1),
    ypos = cumsum(pct) - pct / 2
  ) %>%
  ggplot(aes(x = 0.5, y = pct, fill = lshock_type)) +
  geom_col(width = 1, color = "white", linewidth = 0.5) +
  coord_polar(theta = "y") +
  geom_text(aes(x = 0.85, y = ypos, label = label), size = 3.5, color = "white", fontface = "bold") +
  xlim(0, 1.5) +
  scale_fill_manual(values = rev(unname(tol()[1:9]))) +
  guides(fill = guide_legend(reverse = TRUE, title = "Shock Type")) +
  facet_wrap(~ income, nrow = 4) +
  theme_void() +
  theme(strip.text = element_text(size = 10, margin = margin(b = 0)),
        panel.spacing = unit(0, "pt"))

# B2.2 Shock type distribution by cash buffer ------------------------------------------------------------
df %>%
  filter(!is.na(hh_liquidity), !is.na(lshock_type)) %>%
  mutate(hh_liquidity = factor(hh_liquidity, levels = c("$0 - $100", 
"$100 - $500", "$500 - $1,000", "$1,000 - $2,500" ,  "$2,500 - $5,000" , "$5,000 - $7,500", "$7,500 - $10,000", "$10,000 - $20,000", "$20,000 - $50,000",  "More than $50,000"

  ))) %>%
  group_by(hh_liquidity) %>%
  count(lshock_type) %>%
  mutate(pct = n / sum(n)) %>%
  mutate(lshock_type = factor(lshock_type, levels = shock_levels)) %>%
  arrange(hh_liquidity, desc(lshock_type)) %>%
  mutate(
    label = scales::percent(pct, accuracy = 1),
    ypos = cumsum(pct) - pct / 2
  ) %>%
  ggplot(aes(x = 0.5, y = pct, fill = lshock_type)) +
  geom_col(width = 1, color = "white", linewidth = 0.5) +
  coord_polar(theta = "y") +
  geom_text(aes(x = 0.85, y = ypos, label = label), size = 3.5, color = "white", fontface = "bold") +
  xlim(0, 1.5) +
  scale_fill_manual(values = rev(unname(tol()[1:9]))) +
  guides(fill = guide_legend(reverse = TRUE, title = "Shock Type")) +
  facet_wrap(~ hh_liquidity, nrow = 4) +
  theme_void() +
  theme(strip.text = element_text(size = 10, margin = margin(b = 0)),
        panel.spacing = unit(0, "pt"))
