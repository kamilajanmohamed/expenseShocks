#############################################################
# Author: Kamila Janmohamed
# Date: 2026-04-24
# Description: Explore income data. Numerator reports annual income bins. The survey collects monthly income bins at the time of the shock + statements of actual income amounts. We want to 
    # 1. determine how many reported monthly income amounts are outside the reported monthly income bin and why they might look like this e.g. respondent accidentally reported annual income
    # 2. calculate reported monthly income by 12 to determine data quality i.e. what share of imputed annual income maps to numerator-reported permanent annual income?
#############################################################

# Set WD ------------------------------------------------------------
source("00_setup/config.R")

# Call functions ------------------------------------------------------------
source("code/01_clean/_fun.R")

# Load packages ------------------------------------------------------------
library(tidyverse)
library(readxl)

# Load data ------------------------------------------------------------
df <- read_excel("data/raw/Expense Shock Final Results.xlsx")

# Select income variables ------------------------------------------------------------ 
df <- df %>%
    rename(lshock_hh_inc_bin = `At the time you had this large and unexpected expense, what was your household’s monthly take-home income (your household's monthly income after taxes). Please include all sources of income such as income from jobs, any social security or government payments, and any retirement income.`,
              under_25k = `You previously stated [question('value'), id='11']. Specifically, about how much was your household's monthly take-home income (your household's monthly income after taxes) at the time you had this large and unexpected expense?`,
              over_25k = `You previously stated $25,000 or more. Specifically, about how much was your household's monthly take-home income (your household's monthly income after taxes) at the time you had this large and unexpected expense?`) %>%
       # coalesce the reported income values into one column
       mutate(lshock_hh_inc = coalesce(under_25k, over_25k)) %>%
       # select variables of interest
       select(income, lshock_hh_inc_bin, lshock_hh_inc) %>%
       #turn income into factor
       mutate(income = factor(income, levels = c("- $20k",     "$20k-40k",   "$40k-60k",   "$60k-80k",   "$80k-100k", "$100k-125k", "$125k +"), ordered = TRUE))

# Examine self-reported monthly income bins vs values ------------------------------------------------------------
df2 <- impute_bin(df, "lshock_hh_inc_bin", "lshock_hh_inc", 25000, "lshock_hh_inc_cleaned", "lshock_hh_inc_imputed") %>%
       # add bin back (impute_bin drops it) for downstream diagnostics
       mutate(lshock_hh_inc_bin = df$lshock_hh_inc_bin)


#### CHECK: Classify imputed values ####
cleaned <- df2 %>%
       mutate(
              # re-derive bin bounds (mirrors impute_bin logic) for diagnosis
              lower_bound = case_when(
                     str_detect(lshock_hh_inc_bin, "-") ~
                            as.numeric(gsub("[^0-9]", "", sub("-.*", "", lshock_hh_inc_bin))),
                     str_detect(lshock_hh_inc_bin, "More than| \\+") ~ 25000,
                     TRUE ~ NA_real_),
              upper_bound = case_when(
                     str_detect(lshock_hh_inc_bin, "-") ~
                            as.numeric(gsub("[^0-9]", "", sub(".*-", "", lshock_hh_inc_bin))),
                     str_detect(lshock_hh_inc_bin, "More than") ~ 25000,
                     TRUE ~ NA_real_),
              # test: does value/12 fall inside the bin? → likely annual-reporting mistake
              annual_mistake = case_when(
                     lshock_hh_inc_imputed == 1 &
                            (lshock_hh_inc / 12) >= lower_bound &
                            (upper_bound >= 25000 | (lshock_hh_inc / 12) <= upper_bound) ~ 1,
                     TRUE ~ 0),
              # classify each observation
              inc_error_type = case_when(
                     lshock_hh_inc_imputed == 0                              ~ "in_bin",
                     lshock_hh_inc_imputed == 1 & annual_mistake == 1        ~ "annual_reported_as_monthly",
                     lshock_hh_inc_imputed == 1 & is.na(lshock_hh_inc)       ~ "missing_value",
                     lshock_hh_inc_imputed == 1                              ~ "other_error")
       ) %>%
       select(-c(lower_bound, upper_bound, annual_mistake))

#### CHECK: summarise error types ####
cleaned %>%
       count(inc_error_type) %>%
       mutate(pct = round(n / sum(n) * 100, 1))

# View out-of-bin rows with their classification
View(cleaned %>%
       filter(lshock_hh_inc_imputed == 1) %>%
       select(income, lshock_hh_inc_bin, lshock_hh_inc, lshock_hh_inc_cleaned, inc_error_type) %>%
       filter(inc_error_type == "annual_reported_as_monthly"))

# i can't think of any "annual reported as monthly" case where the reported income seems equal to the numerator permanent annual income so abandon the plan to infer respondent errors and impute accordingly. 

# Exploring annual income and self-reported income ------------------------------------------------------------

# create variable lshock_hh_inc_bin_annual categorising the self-reported income into bins corresponding to the income bins in the numerator-provided income variable. Since the survey collects monthly income, multiply by 12 before binning. Then create a dummy lshock_hh_inc_conflict equal to 1 if lshock_hh_inc_bin_annual \neq income.

test <- df2 %>%
       mutate(temp_lshock_hh_inc = lshock_hh_inc*12) %>%
       mutate(lshock_hh_inc_bin_annual = factor(case_when(temp_lshock_hh_inc < 20000 ~ "- $20k",
                                          temp_lshock_hh_inc >= 20000 & temp_lshock_hh_inc <= 40000 ~ "$20k-40k",
                                          temp_lshock_hh_inc >40000 & temp_lshock_hh_inc <= 60000 ~ "$40k-60k",
                                          temp_lshock_hh_inc >60000 & temp_lshock_hh_inc <= 80000 ~ "$60k-80k",
                                          temp_lshock_hh_inc > 80000 & temp_lshock_hh_inc <= 100000 ~ "$80k-100k",
                                          temp_lshock_hh_inc >100000 & temp_lshock_hh_inc <= 125000 ~ "$100k-125k",
                                          temp_lshock_hh_inc > 125000 ~ "$125k +",
                                          TRUE ~ NA), 
                                          levels = c("- $20k",     "$20k-40k",   "$40k-60k",   "$60k-80k",   "$80k-100k", "$100k-125k", "$125k +"), ordered = TRUE),
              lshock_hh_inc_conflict = case_when(income != lshock_hh_inc_bin_annual & is.na(lshock_hh_inc_bin_annual) == F ~ 1, 
                                                 income == lshock_hh_inc_bin_annual ~ 0,
                                                 TRUE ~ NA)) %>%
       select(-c(temp_lshock_hh_inc))

#### CHECK: Where are errors concentrated in adjacent cells? ####
table(test$lshock_hh_inc_bin_annual, test$income)
# mass concentated in upper right. Suggests that our self-reported annual income is generally lower than actual annual income. sus. 

#### CHECK: Overall conflict rate by bin ####
test %>%
  group_by(income) %>%
  summarise(conflict_rate = mean(lshock_hh_inc_conflict, na.rm = TRUE), n = n())
  # conflict rates increasing with income - the income change seems level-dependent.

#### CHECK: Under and overreporting rate by income bin ####
test %>%
  mutate(direction = case_when(
    lshock_hh_inc_bin_annual < income ~ "underreporting",
    lshock_hh_inc_bin_annual > income ~ "overreporting",
    lshock_hh_inc_bin_annual == income ~ "no_conflict",
    TRUE ~ NA_character_
  )) %>%
  group_by(income, direction) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(income) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  arrange(income, direction) %>%
  filter(!is.na(direction))

#### CHECK: cases where both vars are non NA but conflicts are NA - should not happen ####
test %>% filter(!is.na(income) & !is.na(lshock_hh_inc_bin_annual) & is.na(lshock_hh_inc_conflict))
#none

#### CHECK: Is the imputation causing this issue? ####
unimputed <- test %>% filter(lshock_hh_inc_imputed == 0)
table(unimputed$lshock_hh_inc_bin_annual, unimputed$income)
# same patterns exist even when dropping imputed values - suggests that self-reported income tends to be lower than atual income. 

unimputed %>% 
  group_by(income) %>% 
  summarise(conflict_rate = mean(lshock_hh_inc_conflict, na.rm = TRUE), n = n())
# again the modal conflict bin is 60k to 80k. 

# compare this to the imputed values
imputed <- test %>% filter(lshock_hh_inc_imputed == 1)
table(imputed$lshock_hh_inc_bin_annual, imputed$income)
# same patterns
imputed %>% 
  group_by(income) %>% 
  summarise(conflict_rate = mean(lshock_hh_inc_conflict, na.rm = TRUE), n = n())
  #conflict rates marginally higher for imputed values

### What I conclude is that imputation is not driving the conflicts; there is a positive relationship between underreported monthly income and annual income bin. Suggests something is off about numerator income data - either they collect gross while we collect net, or their data is just outdated.
