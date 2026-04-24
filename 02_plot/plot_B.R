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
library(patchwork)

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
ex_b2 <- df %>%
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
  geom_text(aes(x = 0.85, y = ypos, label = label), size = 5, color = "white", fontface = "bold") +
  xlim(0, 1) +
  scale_fill_manual(values=rev(unname(tol()[1:9]))) + 
  #scale_fill_brewer(palette = "Blues", direction = -1) +
  ggtitle("Distribution of Shock Types") + 
  guides(fill = guide_legend(reverse = TRUE, title = "Shock Type")) +
  theme_void() + 
  theme(aspect.ratio = 1)

# export figure
ggsave("output/figures/ex_b2.pdf", ex_b2, width = 9, height = 6, units = "in")

# B2.1 Shock Type Distribution by Income ------------------------------------------------------------
# Build plot WITHOUT legend
ex_b2.1 <- df %>%
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
  xlim(0, 1) +
  scale_fill_manual(values = rev(unname(tol()[1:9]))) +
  guides(fill = guide_legend(reverse = TRUE, title = "Shock Type")) +
  ggtitle("Distribution of Shock Types by Income") + 
  facet_wrap(~ income, nrow = 3) +
  theme_void() +
  theme(
    strip.text      = element_text(size = 10, margin = margin(b = 0, t = 2)),
    panel.spacing   = unit(-8, "pt"),   # pull panels together
    plot.margin     =  margin(10,10, 5,10),
    aspect.ratio    = 1,
    legend.position  = c(0.83, 0.18),
    legend.title    = element_text(size = 9, face = "bold"),
    legend.text     = element_text(size = 8),
    legend.key.size = unit(0.35, "cm"),
    legend.margin   = margin(t = -5)    # pull legend up toward pies
  )

# export figure
ggsave("output/figures/ex_b2.1.pdf", ex_b2.1, width = 10, height = 11, units = "in")

# B2.2 Shock type distribution by cash buffer ------------------------------------------------------------
ex_b2.2 <- df %>%
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
  xlim(0, 1) +
  scale_fill_manual(values = rev(unname(tol()[1:9]))) +
  guides(fill = guide_legend(reverse = TRUE, title = "Shock Type")) +
  ggtitle("Shock Distribution by Liquidity Buffer") + 
  facet_wrap(~ hh_liquidity, nrow = 4) +
  theme_void() +
  theme(
    strip.text      = element_text(size = 10, margin = margin(b = 0, t = 2)),
    panel.spacing   = unit(-8, "pt"),   # pull panels together
    plot.margin     = margin(10,10, 5,10),
    aspect.ratio    = 1,
    legend.position  = c(0.83, 0.18),
    legend.title    = element_text(size = 9, face = "bold"),
    legend.text     = element_text(size = 8),
    legend.key.size = unit(0.35, "cm"),
    legend.margin   = margin(t = -5)    # pull legend up toward pies
  )

ggsave("output/figures/ex_b2.2.pdf", ex_b2.2, width = 9, height = 12, units = "in")

