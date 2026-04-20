/#############################################################
# Author: Kamila Janmohamed
# Date: 2026-04-20
# Description: Clean survey data on expense shocks
#############################################################

# Set WD ------------------------------------------------------------
#run once per session - this will be the root directory for all file paths in this project
source("00_setup/config.R")

# Load packages ------------------------------------------------------------
library(tidyverse)
library(readxl)

# Load data ------------------------------------------------------------
df <- read_excel("data/raw/Expense Shock Final Results.xlsx")

# Clean data ------------------------------------------------------------
cleaned <- df %>%
# drop time_started
  select(-`Time Started`) %>%
  rename(
    date    = `Date Submitted`,
    age     = age_exact,
    age_gen = age_gen_long,
    children = has_children
  ) %>%
  mutate(
    date              = as.Date(date),
    complete_response = case_when(Status == "Complete" ~ 1, 
                                  is.na(Status) = TRUE ~ NA,
                                  TRUE ~ 0),
    ### age vars ###
    age_bucket        = factor(age_bucket,
                               levels  = c("21-24", "25-34", "35-44", "45-54", "55-64", "65+"),
                               ordered = TRUE),
    age               = suppressWarnings(as.numeric(age)),
    # if age missing, use age bucket to impute age as midpoint of bucket
    age               = case_when(is.na(age) & age_bucket == "21-24" ~ mean(c(21,24)),
                                 is.na(age) & age_bucket == "25-34" ~ mean(c(25,34)),
                                 is.na(age) & age_bucket == "35-44" ~ mean(c(35,44)),
                                 is.na(age) & age_bucket == "45-54" ~ mean(c(45,54)),
                                 is.na(age) & age_bucket == "55-64" ~ mean(c(55,64)),
                                 is.na(age) & age_bucket == "65+"   ~ 70,
                                 TRUE ~ age),
    age_gen           = factor(age_gen,
                               levels = c("Boomers+ [< 1965]", "Gen X [1965-1981]",
                                          "Millennials [1982-1995]", "Gen Z [> 1995]")),
    education         = factor(education,
                               levels = c("Less than high school",  
                                          "High School/GED",
                                          "Trade/Technical Degree",
                                           "2 year College Degree", "Some College or university",
                                           "4 year College Degree", "Some Graduate School",
                                           "Graduate Degree"),
                               ordered = TRUE),
    ethnicity         = factor(ethnicity,
                               levels = c("White/Caucasian", "Black or African American",
                                          "Asian", "Hispanic/Latino", "Other")),
    gender            = factor(gender, levels = c("Male", "Female", "Other")),
    children          = case_when(children == "Yes" ~ 1,
                                  children == "No" ~  0,
                                  TRUE ~ NA),
    hh_size           = suppressWarnings(as.numeric(hh_size))
  ) %>%
  select(-c(Status))
