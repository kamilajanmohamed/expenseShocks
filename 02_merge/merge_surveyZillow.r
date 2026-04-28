#############################################################
# Author: Kamila Janmohamed
# Date: 2026-04-28
# Description: Merge Home Values onto survey responses
#############################################################

# Set WD ------------------------------------------------------------
source("00_setup/config.R")

# Load packages ------------------------------------------------------------
library(tidyverse)

# Load data ------------------------------------------------------------
zillow <- read_csv("data/raw/Zip_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv")  %>%
    select(-c(SizeRank, RegionID, RegionType, StateName, State, City, Metro, CountyName)) %>%
    rename(postal_code = RegionName) %>%
    mutate(postal_code = as.numeric(postal_code))

survey <- read_csv("data/clean/surveyData.csv")

# Pivot zillow data to long ------------------------------------------------------------
zillow_wide <- pivot_longer(zillow, 
                        cols = names(zillow)[-1],
                        names_to = "trunc_date") %>%
    #keep dates in 2025 and 2026 to shorten df
    filter(str_detect(trunc_date, "2024|2025|2026")) %>%
    mutate(trunc_date = floor_date(as.Date(trunc_date), "month"))

# Merge home values onto survey data ------------------------------------------------------------
merged_survey <- survey %>%
                mutate(postal_code = as.numeric(as.character(postal_code))) %>%
                # merge home values on by survey date
                mutate(trunc_date = floor_date(as.Date(date), "month")) %>%
                left_join(zillow_wide, by = c("trunc_date", "postal_code")) %>%
                select(-c(trunc_date)) %>%
                rename(home_value_date = value) %>%
                # merge home values on by shock date
                mutate(trunc_date = floor_date(as.Date(lshock_month), "month")) %>%
                left_join(zillow_wide, by = c("trunc_date", "postal_code")) %>%
                select(-c(trunc_date)) %>%
                rename(home_value_lshock_month = value) %>%
                # merge home values by month before shock date
                mutate(trunc_date = floor_date(as.Date(lshock_month) %m-% months(1), "month")) %>%
                left_join(zillow_wide, by = c("trunc_date", "postal_code")) %>%
                select(-c(trunc_date)) %>%
                rename(home_value_lag_lshock_month = value) 

#### CHECK: Missing home_value_date ####
cat("Missing date:", sum(is.na(merged_survey$date)), "\n")
cat("Missing home_value_date:", sum(is.na(merged_survey$home_value_date)), "\n")
# 569 obs missing home_value_date. seems to be driven by dates from march 2026 onwards. can revisit this in a few months with updated zillow data. 
unique(merged_survey$date[is.na(merged_survey$home_value_lshock_month)])

#### CHECK: Missing home_value_lshock_month ####
cat("Missing lshock_month:", sum(is.na(merged_survey$home_value_lshock_month)), "\n")
cat("Missing home_value_lshock_month:", sum(is.na(merged_survey$home_value_lshock_month)), "\n")
# we have zipcode home values for everyone with a shock!

#### CHECK: Missing home_value_lag_lshock_month ####
cat("Missing lshock_month:", sum(is.na(merged_survey$home_value_lshock_month)), "\n")
cat("Missing home_value_lshock_month:", sum(is.na(merged_survey$home_value_lag_lshock_month)), "\n")
# we have lagged month home values for everyone with a shock!

# Export merged data ------------------------------------------------------------
write_csv(merged_survey, "data/merged/merged_survey.csv")

