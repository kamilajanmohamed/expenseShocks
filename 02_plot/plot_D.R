#############################################################
# Author: Kamila Janmohamed
# Date: 2026-05-03
# Description: Plot Financial Distress Exhibits (Set D)
#############################################################

# Set WD ------------------------------------------------------------
source("00_setup/config.R")

# Load packages ------------------------------------------------------------
library(tidyverse)
library(psych)

# Load data ------------------------------------------------------------
df <- read_rds("data/merged/merged_survey.rds")


subset <- df %>%
select(any_shock, lshock_size, ends_with("6m"), ends_with("oct25"))
s
cor(subset)

mean(subset$pastdue_lcc_6m, na.rm = T)
mean(subset$pastdue_rm_6m, na.rm = T)
mean(subset$collections_6m, na.rm = T)
mean(subset$utility_cutoff_6m, na.rm = T)
mean(subset$repo_notice_6m, na.rm = T)
mean(subset$eviction_notice_6m, na.rm = T)

table(df$any_shock, df$pastdue_lcc_oct25)


pc <- prcomp(subset %>% select(contains("6m")) %>% na.omit(),
             center = TRUE,
            scale. = TRUE)
print(pc)
