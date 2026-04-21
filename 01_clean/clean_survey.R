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
  ####### demographics #######
  # drop time survey started since we are only interested in date submitted for now
  select(-`Time Started`) %>%
  # rename variables to be more concise and easier to work with, and to follow snake case convention
  rename(
    date = `Date Submitted`,
    age = age_exact,
    age_gen = age_gen_long,
    children = has_children,
    census_division = user_census_division_name,
    census_region = user_census_region_name,
    state = user_state_name,
    urbanicity = user_urbanicity
    ) %>%
  mutate(
    user_id = as.factor(user_id),
    date = as.Date(date),
    complete_response = case_when(Status == "Complete" ~ 1, 
                                  is.na(Status) == TRUE ~ NA,
                                  TRUE ~ 0),
    ### age vars ###
    age_bucket = factor(age_bucket,
                               levels  = c("21-24", "25-34", "35-44", "45-54", "55-64", "65+"),
                               ordered = TRUE),
    age = suppressWarnings(as.numeric(age)),
    # if age missing, use age bucket to impute age as midpoint of bucket
    age = case_when(is.na(age) & age_bucket == "21-24" ~ mean(c(21,24)),
                                 is.na(age) & age_bucket == "25-34" ~ mean(c(25,34)),
                                 is.na(age) & age_bucket == "35-44" ~ mean(c(35,44)),
                                 is.na(age) & age_bucket == "45-54" ~ mean(c(45,54)),
                                 is.na(age) & age_bucket == "55-64" ~ mean(c(55,64)),
                                 is.na(age) & age_bucket == "65+"   ~ 70,
                                 TRUE ~ age),
    age_gen = factor(age_gen,
                     levels = c("Boomers+ [< 1965]", "Gen X [1965-1981]", 
                                        "Millennials [1982-1995]", "Gen Z [> 1995]")),
    education= factor(education,
                      levels = c("Less than high school",
                                 "High School/GED",
                                  "Trade/Technical Degree",
                                  "2 year College Degree", "Some College or university",
                                  "4 year College Degree", "Some Graduate School",
                                  "Graduate Degree"),
                      ordered = TRUE),
    ethnicity = factor(ethnicity,
                       levels = c("White/Caucasian", "Black or African American",
                                  "Asian", "Hispanic/Latino", "Other")),
    gender = factor(gender, 
                    levels = c("Male", "Female", "Other")),
    children = case_when(children == "Yes" ~ 1,
                         children == "No" ~  0,
                         TRUE ~ NA),
    hh_size = suppressWarnings(as.numeric(hh_size)),
    income = factor(income, 
                    levels = c("- $20k", "$20k-40k", "$40k-60k", 
                               "$60k-80k", "$80k-100k", "$100k-125k", "$125k +"),
                    ordered = TRUE),
    job_ref = suppressWarnings(as.numeric(job_ref)),
    marital_status = factor(marital_status, 
                            levels = c("Never married", "Living with partner", 
                                       "Married", "Widower", "Divorced", "Separated"), 
                            ordered = TRUE),
    postal_code = as.factor(postal_code),
    census_division = factor(census_division, 
                             levels = c("New England", "Mid-Atlantic", 
                                        "East North Central", "West North Central", 
                                        "South Atlantic", "East South Central", 
                                        "West South Central", "Mountain", "Pacific")),
    census_region = factor(census_region, 
                           levels = c("Northeast", "Midwest", "South", "West")),
    state = factor(state, 
                   levels = c("Alabama", "Alaska", "Arizona", "Arkansas", 
                              "California", "Colorado", "Connecticut", 
                              "Delaware", "Florida", "Georgia", "Hawaii", 
                              "Idaho", "Illinois", "Indiana", "Iowa", 
                              "Kansas", "Kentucky", "Louisiana", "Maine", 
                              "Maryland", "Massachusetts", "Michigan", 
                              "Minnesota", "Mississippi", "Missouri", 
                              "Montana", "Nebraska", "Nevada", 
                              "New Hampshire", "New Jersey", "New Mexico", 
                              "New York", "North Carolina", "North Dakota", 
                              "Ohio", "Oklahoma", "Oregon", "Pennsylvania",
                              "Rhode Island", "South Carolina", "South Dakota", 
                              "Tennessee", "Texas", "Utah", "Vermont", "Virginia", 
                              "Washington", "West Virginia", "Wisconsin", "Wyoming")),
    urbanicity = factor(urbanicity, 
                        levels = c("Urban", "Suburban", "Rural")),
    demo_weight = as.numeric(demo_weight)) %>%
  select(-c(Status)) %>%
  # group demographics together at the front of the dataset
  relocate(c("user_id", "date", "complete_response", "age_bucket", 
             "age", "age_gen", "education", "ethnicity", "gender", 
             "children", "hh_size", "income", "job_ref", "marital_status", 
             "postal_code", "census_division", "census_region", "state", 
             "urbanicity", "demo_weight")) %>% 
  ######## expense shock vars ####### 
  # rename relevant variables to be more concise and easier to work with
  rename(
    expense_shock_25 = `Since the beginning of 2025, did your household have a large and unexpected expense?`,
    n_expense_shocks_25 = `How many times since the beginning of 2025 did your household have a large and unexpected expense?`,
    last_expense_shock_type_25 = `For the most recent time, what type of large and unexpected expense was this?`,
    last_expense_shock_type_other_25 = `Some other large and unexpected expense, please specify::For the most recent time, what type of large and unexpected expense was this?`) %>%
  mutate(
    expense_shock_25 = case_when(expense_shock_25 == "Yes" ~ 1,
                                 expense_shock_25 == "No" ~ 0,
                                  TRUE ~ NA),
    n_expense_shocks_25 = factor(n_expense_shocks_25, 
                                 levels = c("1 time", "2 times", "3 times" , 
                                            "4 times", "5 to 10 times" , 
                                            "More than 10 times"), 
                                 ordered = TRUE),
  # first, create a variable for the type of most recent expense shock using raw response categories - includes "other"
  last_expense_shock_type_25 = factor(last_expense_shock_type_25,levels = 
                                        c("Funeral or burial expenses", 
                                          "A major out-of-pocket medical or dental expense", 
                                          "A major home or appliance repair", 
                                          "A major vehicle repair or replacement", 
                                          "Legal expenses, taxes, or fines", 
                                          "Moving or other relocation costs", 
                                          "A large and unexpected increase in rent payment", 
                                          "A major veterinary or pet care expense", 
                                          "A computer or mobile phone repair or replacement", 
                                          "An increase in childcare or dependent care expenses", 
                                          "A large and unexpected increase in electric, heating/cooling, or other utility bill", 
                                          "An unplanned gift or loan to a family member or friend", 
                                          "Some other large and unexpected expense, please specify:"),
                                  labels = c("Funeral/Burial expenses", 
                                             "Medical/Dental expenses", 
                                             "Home/Appliance repair", 
                                             "Vehicle repair/replacement", 
                                             "Legal expenses/taxes/fines", 
                                             "Moving/Relocation costs", 
                                             "Rent increase", 
                                             "Veterinary/Pet care expenses", 
                                             "Computer/Phone repair/replacement", 
                                             "Childcare/Dependent care expenses", 
                                             "Utility bill increase", 
                                             "Gift/Loan to family/friend", 
                                             "Other")),
  # standardise "other" responses by converting to lowercase 
  last_expense_shock_type_other_25 = tolower(last_expense_shock_type_other_25),
  # then, replace other vars in the last_expense_shock_type_25_variable with recategorised other responses where applicable
  last_expense_shock_type_25 = case_when(last_expense_shock_type_25 != "Other" ~ last_expense_shock_type_25,
                                last_expense_shock_type_25 == "Other" & 
                                  str_detect(last_expense_shock_type_other_25, "funeral|burial|died") ~ "Funeral/Burial expenses",
                                         last_expense_shock_type_25 == "Other" & 
                                  str_detect(last_expense_shock_type_other_25, "medical|dental|health|cancer|surgery|surgeries|sugeries|hospital|therapy") & 
                                  str_detect(last_expense_shock_type_other_25, "son|family|child") == FALSE ~ "Medical/Dental expenses",
                                         last_expense_shock_type_25 == "Other" & 
                                  str_detect(last_expense_shock_type_other_25, "home|house|window|roof|basement|garage|driveway|crawl|deck|roof|window|lawn|pool|tree|pest|leak|plumber|plumbing|flood|paint|furnace|a/c|conditioner|aircon|furnace|electrical|appliance|hvac|freezer|fridge|refrigerator|stove| washer|dryer|heater|drainage|gas line|new guest bed|new recliner|heating system repair|hot tub|solar system|well water") & 
                                  str_detect(last_expense_shock_type_other_25, "house payment|purchase a home") == FALSE ~ "Home/Appliance repair",
                                         last_expense_shock_type_25 == "Other" & 
                                  str_detect(last_expense_shock_type_other_25, "vehicle|auto|car|truck|auto|tire|boat|oil tank") & 
                                  str_detect(last_expense_shock_type_other_25, "we had to buy a trailer to use as another room to place my son's in so that we could take care of a family member who needed 24 hour care") == FALSE ~ "Vehicle repair/replacement",
                                         last_expense_shock_type_25 == "Other" & 
                                  str_detect(last_expense_shock_type_other_25, "legal|tax|fine") ~ "Legal expenses/taxes/fines",
                                         last_expense_shock_type_25 == "Other" & 
                                  str_detect(last_expense_shock_type_other_25, "moving|relocation|deposit") ~ "Moving/Relocation costs",
                                         last_expense_shock_type_25 == "Other" & 
                                  str_detect(last_expense_shock_type_other_25, "\\brent\\b|mortgage|house payment")  ~ "Rent increase",
                                         last_expense_shock_type_25 == "Other" & 
                                  str_detect(last_expense_shock_type_other_25, "veterinary|pet|\\bcat\\b|dog") ~ "Veterinary/Pet care expenses",
                                         last_expense_shock_type_25 == "Other" & 
                                  str_detect(last_expense_shock_type_other_25, "computer|phone") ~ "Computer/Phone repair/replacement",
                                         last_expense_shock_type_25 == "Other" & 
                                  str_detect(last_expense_shock_type_other_25, "childcare|dependent care") ~ "Childcare/Dependent care expenses",
                                         last_expense_shock_type_25 == "Other" & 
                                  str_detect(last_expense_shock_type_other_25, "utility|electric|light bill") ~ "Utility bill increase",
                                         last_expense_shock_type_25 == "Other" & 
                                  str_detect(last_expense_shock_type_other_25, "gift|loan|son|child|grandson|eldest|family|daughter") & 
                                  str_detect(last_expense_shock_type_other_25, "family members moved in with us|visit to family member whose time on earth was limited") == FALSE ~ "Gift/Loan to family/friend",
                                         TRUE ~ last_expense_shock_type_25)) %>%
  # drop the other categorisation column
  select(-c(last_expense_shock_type_other_25))
