#############################################################
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
#### Demographic variables -------------------------------------------------------------
cleaned <- df %>%
  # drop time survey started since we are only interested in date submitted for now
  select(-`Time Started`) %>%
  # rename variables for conciseness
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
             "urbanicity", "demo_weight"))


#### Expense shock types -------------------------------------------------------------
cleaned <- cleaned %>%
  # rename relevant variables for conciseness
  rename(
    shock_25 = `Since the beginning of 2025, did your household have a large and unexpected expense?`,
    n_shock_25 = `How many times since the beginning of 2025 did your household have a large and unexpected expense?`,
    shock_type_25 = `For the most recent time, what type of large and unexpected expense was this?`,
    shock_type_other_25 = `Some other large and unexpected expense, please specify::For the most recent time, what type of large and unexpected expense was this?`) %>%
  mutate(
    shock_25 = case_when(shock_25 == "Yes" ~ 1,
                                 shock_25 == "No" ~ 0,
                                  TRUE ~ NA),
    n_shock_25 = factor(n_shock_25, 
                                 levels = c("1 time", "2 times", "3 times" , 
                                            "4 times", "5 to 10 times" , 
                                            "More than 10 times"), 
                                 ordered = TRUE),
  # first, create a variable for the type of most recent expense shock using raw response categories - includes "other"
  shock_type_25 = factor(shock_type_25,levels = 
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
  shock_type_other_25 = tolower(shock_type_other_25),
  # then, replace other vars in the shock_type_25_variable with recategorised other responses where applicable
  shock_type_25 = case_when(shock_type_25 != "Other" ~ shock_type_25,
                                shock_type_25 == "Other" & 
                                  str_detect(shock_type_other_25, "funeral|burial|died") ~ "Funeral/Burial expenses",
                                         shock_type_25 == "Other" & 
                                  str_detect(shock_type_other_25, "medical|dental|health|cancer|surgery|surgeries|sugeries|hospital|therapy") & 
                                  str_detect(shock_type_other_25, "son|family|child") == FALSE ~ "Medical/Dental expenses",
                                         shock_type_25 == "Other" & 
                                  str_detect(shock_type_other_25, "home|house|window|roof|basement|garage|driveway|crawl|deck|roof|window|lawn|pool|tree|pest|leak|plumber|plumbing|flood|paint|furnace|a/c|conditioner|aircon|furnace|electrical|appliance|hvac|freezer|fridge|refrigerator|stove| washer|dryer|heater|drainage|gas line|new guest bed|new recliner|heating system repair|hot tub|solar system|well water") & 
                                  str_detect(shock_type_other_25, "house payment|purchase a home") == FALSE ~ "Home/Appliance repair",
                                         shock_type_25 == "Other" & 
                                  str_detect(shock_type_other_25, "vehicle|auto|car|truck|auto|tire|boat|oil tank") & 
                                  str_detect(shock_type_other_25, "we had to buy a trailer to use as another room to place my son's in so that we could take care of a family member who needed 24 hour care") == FALSE ~ "Vehicle repair/replacement",
                                         shock_type_25 == "Other" & 
                                  str_detect(shock_type_other_25, "legal|tax|fine") ~ "Legal expenses/taxes/fines",
                                         shock_type_25 == "Other" & 
                                  str_detect(shock_type_other_25, "moving|relocation|deposit") ~ "Moving/Relocation costs",
                                         shock_type_25 == "Other" & 
                                  str_detect(shock_type_other_25, "\\brent\\b|mortgage|house payment")  ~ "Rent increase",
                                         shock_type_25 == "Other" & 
                                  str_detect(shock_type_other_25, "veterinary|pet|\\bcat\\b|dog") ~ "Veterinary/Pet care expenses",
                                         shock_type_25 == "Other" & 
                                  str_detect(shock_type_other_25, "computer|phone") ~ "Computer/Phone repair/replacement",
                                         shock_type_25 == "Other" & 
                                  str_detect(shock_type_other_25, "childcare|dependent care") ~ "Childcare/Dependent care expenses",
                                         shock_type_25 == "Other" & 
                                  str_detect(shock_type_other_25, "utility|electric|light bill") ~ "Utility bill increase",
                                         shock_type_25 == "Other" & 
                                  str_detect(shock_type_other_25, "gift|loan|son|child|grandson|eldest|family|daughter") & 
                                  str_detect(shock_type_other_25, "family members moved in with us|visit to family member whose time on earth was limited") == FALSE ~ "Gift/Loan to family/friend",
                                         TRUE ~ shock_type_25)) %>%
  # drop the other categorisation column
  select(-c(shock_type_other_25)) %>%
       # reorder to group expense shock variables together
       relocate(c("shock_25", "n_shock_25", "shock_type_25"), 
               .after = "demo_weight")

#### Expense shock amounts -------------------------------------------------------------
cleaned <- cleaned %>%
  #rename variables for conciseness
  rename(expense_bin = `About how much money was the total cost for this large and unexpected expense?  Please estimate the full amount of this expense even if you did not pay all of it at once or if you are paying over time.`,
         expense_value_under_20k = `You previously stated [question('value'), id='6']. Specifically, what was the approximate total cost for this large and unexpected expense?`,
         expense_value_over_20k = `You previously stated $20,000 or more. Specifically, what was the approximate total cost for this large and unexpected expense?`) %>%
       # coalesce the reported expense values into one column 
       mutate(shock_size_25 = coalesce(expense_value_under_20k, expense_value_over_20k)) %>%
         # create bin variables
         mutate(
              # lower bound of expense bin variable
              lower_bound =  case_when(str_detect(expense_bin, "-") ~ (as.numeric(gsub("[^0-9]", "", sub("-.*", "", expense_bin)))),
                                                                      str_detect(expense_bin, "More than") ~ 20000,
                                                                      TRUE ~ NA_real_),
              # upper bound of expense bin variable
              upper_bound = case_when(str_detect(expense_bin, "-") ~ (as.numeric(gsub("[^0-9]", "", sub(".*-", "", expense_bin)))),
                                                                      str_detect(expense_bin, "More than") ~ 20000,
                                                                      TRUE ~ NA_real_),
              bin_midpoint = (lower_bound+upper_bound)/2) %>% 
       # create helper dummies 
       mutate(
              # impute if the expense value is less than the lower bound of the bin or if the expense value is greater than the upper bound of the bin (for bins under 20k)
              reporting_error = case_when(shock_size_25 < lower_bound ~ 1,
                                          upper_bound < 20000 & shock_size_25 > upper_bound ~ 1,
                                                 TRUE ~ 0),
              # missingness in reported expense value but not in expense bin
              missing_expense_value = case_when(is.na(shock_size_25) == TRUE & is.na(expense_bin) == FALSE ~ 1,
                                              TRUE ~ 0)) %>%
       # impute expense shock sizes for responses with errors 
       mutate(shock_size_25 = case_when(reporting_error == 1 | missing_expense_value == 1 ~ bin_midpoint, 
                                                     TRUE ~ shock_size_25),
              shock_size_25_imputed = case_when(reporting_error == 1 | missing_expense_value == 1 ~ 1,
                                                            TRUE ~ 0)) %>%
       select(-c(expense_bin, lower_bound, upper_bound, bin_midpoint, reporting_error, missing_expense_value, expense_value_over_20k, expense_value_under_20k)) %>%
       # rearrange expense shock variables
       relocate(c("shock_size_25", "shock_size_25_imputed"), .after = "shock_type_25")

#### Expense shock repayment -------------------------------------------------------------
cleaned <- cleaned %>%
       # rename variables
       rename(shock_paid_3m_25 = `Did you pay the full amount of this large and unexpected expense within three months after you had this expense?`,
              shock_amt_paid_3m_25 = `Within three months after you had this large and unexpected expense, about how much of it had you paid off?`,
              shock_paid_cs_acct_25 = `I used money in checking and savings accounts:How did you pay for this large and unexpected expense? Please select all that apply.`,
              shock_paid_ret_acct_25 = `I used money in retirement accounts:How did you pay for this large and unexpected expense? Please select all that apply.`,
              shock_paid_ccard_same_month_25 =  `I paid it using my credit card and paid the full amount at the end of the month:How did you pay for this large and unexpected expense? Please select all that apply.`,
              shock_paid_ccard_many_months_25 = `I paid it using my credit card and paid the amount over multiple months:How did you pay for this large and unexpected expense? Please select all that apply.`,
              shock_paid_ff_loan_25 = `I borrowed from family and/or friends:How did you pay for this large and unexpected expense? Please select all that apply.`,
              shock_paid_payday_loan_25 = `I used a payday or auto-title loan:How did you pay for this large and unexpected expense? Please select all that apply.`,
              shock_paid_bank_loan_25 = `I took a loan from a bank or a credit union:How did you pay for this large and unexpected expense? Please select all that apply.`,
              shock_paid_home_equity_25 = `I took out or used a home equity line of credit:How did you pay for this large and unexpected expense? Please select all that apply.`,
              shock_paid_reduced_spending_25 = `I reduced my spending on other expenses:How did you pay for this large and unexpected expense? Please select all that apply.`,
              shock_paid_missed_payments_25 = `I missed some bills or other payments:How did you pay for this large and unexpected expense? Please select all that apply.`,
              shock_paid_increased_income_25 = `I increased my income by working more hours or at another job:How did you pay for this large and unexpected expense? Please select all that apply.`,
              shock_paid_sold_belongings_25 = `I sold some belongings:How did you pay for this large and unexpected expense? Please select all that apply.`,
              shock_paid_dk_25 = `I’m not sure / I don’t recall:How did you pay for this large and unexpected expense? Please select all that apply.`,
              shock_paid_other_25 = `Other, please specify::How did you pay for this large and unexpected expense? Please select all that apply....43`,
              shock_paid_other_text_25 = `Other, please specify::How did you pay for this large and unexpected expense? Please select all that apply....45`) %>%
       # clean shock repayment variables
       mutate(
              shock_paid_3m_25 = factor(shock_paid_3m_25, levels = c("Yes, I <u>paid all</u> of the expense within three months" , "No, I <u>paid only some</u> of the expense within three months", "No, I <u>did not pay any</u> of the expense within three months"), 
                                                         labels = c("Paid in full within 3 months", "Partially paid within 3 months", "Not paid at all within 3 months"), 
                                                         ordered = TRUE),
              shock_amt_paid_3m_25 = factor(shock_amt_paid_3m_25, levels = c("I only paid very little (close to 0%)", "I paid about half (50%) of the expense", "I paid about three-quarters (75%) of the expense", "I paid almost all (almost 100%) of the expense"), 
                                                                      labels = c("About 0%", "About 50%", "About 75%", "About 100%"), 
                                                                      ordered = TRUE)) %>%
       # if you paid 0% or 100% within 3 months in 2025 from shock_paid_3m_25, add levels for 100% paid and 0% paid to shock_amt_paid_3m_25
       mutate(shock_amt_paid_3m_25 = case_when(shock_paid_3m_25 == "Not paid at all within 3 months" & is.na(shock_amt_paid_3m_25) == TRUE ~ "0%",
                                                 shock_paid_3m_25 == "Paid in full within 3 months" & is.na(shock_amt_paid_3m_25) == TRUE ~ "100%",
                                                 TRUE ~ shock_amt_paid_3m_25)) %>%
       # add and reorder factor levels
       mutate(shock_amt_paid_3m_25 = factor(shock_amt_paid_3m_25, levels = c("0%", "About 0%", "About 50%", "About 75%", "About 100%", "100%"), ordered = TRUE)) %>%
       # recode shock repayment method variables as dummies. if shock amt paid is NA, all of these should also be NAs
       mutate(across(c(shock_paid_cs_acct_25, shock_paid_ret_acct_25, shock_paid_ccard_same_month_25,
                            shock_paid_ccard_many_months_25, shock_paid_ff_loan_25, shock_paid_payday_loan_25,
                            shock_paid_bank_loan_25, shock_paid_home_equity_25, shock_paid_reduced_spending_25,
                            shock_paid_missed_payments_25, shock_paid_increased_income_25, shock_paid_sold_belongings_25,
                            shock_paid_dk_25, shock_paid_other_25),
                            ~ as.integer(!is.na(.x)))) %>%
       # lower case free test responses for easier searching
       mutate(shock_paid_other_text_25 = tolower(shock_paid_other_text_25)) %>%
       # reassign free text reponses where possible using string searching. intentionally not using an LLM for reproducibility and transparency
       mutate(
              shock_paid_cs_acct_25 = if_else(
    shock_paid_other_25 == 1 &
      str_detect(shock_paid_other_text_25,
        "\\bcash\\b|insurance|\\bhsa\\b|tax|inheritance|christmas money|dedicated account|salary||savings|checking|check|work bonus|deductible|out of pocket|debit|deposit"),
    1L, shock_paid_cs_acct_25, shock_paid_cs_acct_25),

  shock_paid_ret_acct_25 = if_else(
    shock_paid_other_25 == 1 &
      str_detect(shock_paid_other_text_25, "401k|\\bira\\b|retirement"),
    1L, shock_paid_ret_acct_25, shock_paid_ret_acct_25),

  shock_paid_ccard_many_months_25 = if_else(
              shock_paid_other_25 == 1 &
              str_detect(shock_paid_other_text_25,
                     "credit card|credit line"),
              1, shock_paid_ccard_many_months_25, shock_paid_ccard_many_months_25),

              shock_paid_ff_loan_25 = if_else(
              shock_paid_other_25 == 1 &
              str_detect(shock_paid_other_text_25, "son|church|go.?fund.?me"),
              1L, shock_paid_ff_loan_25, shock_paid_ff_loan_25),

              shock_paid_bank_loan_25 = if_else(
              shock_paid_other_25 == 1 &
              str_detect(shock_paid_other_text_25,
                     "loan|interest|financed|financing") &
              !str_detect(shock_paid_other_text_25,
                     "401k|retirement|\\bira\\b|care credit|payment plan|affirm|medical credit"),
              1L, shock_paid_bank_loan_25, shock_paid_bank_loan_25),

              shock_paid_home_equity_25 = if_else(
              shock_paid_other_25 == 1 &
              str_detect(shock_paid_other_text_25, "refinanc"),
              1L, shock_paid_home_equity_25, shock_paid_home_equity_25),

              shock_paid_increased_income_25 = if_else(
              shock_paid_other_25 == 1 &
              str_detect(shock_paid_other_text_25, "bonus|recycl|whored"),
              1L, shock_paid_increased_income_25, shock_paid_increased_income_25),

              shock_paid_sold_belongings_25 = if_else(
              shock_paid_other_25 == 1 &
              str_detect(shock_paid_other_text_25,
                     "stock|sell|sold|salvage|cashed"),
              1L, shock_paid_sold_belongings_25, shock_paid_sold_belongings_25)) %>%
       # drop cleaned free text variable
       select(-c(shock_paid_other_text_25)) %>%
       # remove other flag for recategorised values in shock_paid_other_25
       mutate(reclassified = case_when(shock_paid_other_25 == 1 & (shock_paid_cs_acct_25 == 1 | shock_paid_ret_acct_25 == 1 | shock_paid_ccard_many_months_25 == 1 | shock_paid_ff_loan_25 == 1 | shock_paid_bank_loan_25 == 1 | shock_paid_home_equity_25 == 1 | shock_paid_increased_income_25 == 1 | shock_paid_sold_belongings_25 == 1) ~ 1,
                                    TRUE ~ 0),
              shock_paid_other_25 = case_when(reclassified == 1 ~ 0,
                                              TRUE ~ shock_paid_other_25)) %>%
       # drop reclassified flag variable
       select(-c(reclassified)) %>%
       # reorder variables to group shock repayment variables together
       relocate(c("shock_paid_3m_25", "shock_amt_paid_3m_25", "shock_paid_cs_acct_25", "shock_paid_ret_acct_25", "shock_paid_ccard_same_month_25",
                  "shock_paid_ccard_many_months_25", "shock_paid_ff_loan_25", "shock_paid_payday_loan_25",
                  "shock_paid_bank_loan_25", "shock_paid_home_equity_25", "shock_paid_increased_income_25", "shock_paid_sold_belongings_25",
                  "shock_paid_dk_25", "shock_paid_other_25"), .after = "shock_size_25_imputed")

#### HH income at time of shock -------------------------------------------------------------
cleaned <- cleaned %>%
       # rename variables for conciseness
       rename(income_bin = `At the time you had this large and unexpected expense, what was your household’s monthly take-home income (your household's monthly income after taxes). Please include all sources of income such as income from jobs, any social security or government payments, and any retirement income.`,
              income_value_under_25k = `You previously stated [question('value'), id='11']. Specifically, about how much was your household's monthly take-home income (your household's monthly income after taxes) at the time you had this large and unexpected expense?`,
              income_over_25k = `You previously stated $25,000 or more. Specifically, about how much was your household's monthly take-home income (your household's monthly income after taxes) at the time you had this large and unexpected expense?`) %>%
       # coalesce the reported income values into one column
       mutate(income_25 = coalesce(income_value_under_25k, income_over_25k)) %>%
       # create bin variables
       mutate(
              # lower bound of income bin variable
              income_lower_bound =  case_when(str_detect(income_bin, "-") ~ (as.numeric(gsub("[^0-9]", "", sub("-.*", "", income_bin)))),
                                                                      str_detect(income_bin, "More than") ~ 25000,
                                                                      TRUE ~ NA), 
              # upper bound of income bin variable
              income_upper_bound = case_when(str_detect(income_bin, "-") ~ (as.numeric(gsub("[^0-9]", "", sub(".*-", "", income_bin)))),
                                                                      str_detect(income_bin, "More than") ~ 25000,
                                                                      TRUE ~ NA),
              income_bin_midpoint = (income_lower_bound+income_upper_bound)/2) %>%
       # create helper dummies
       mutate(
              # impute if income value less than lower bound of bin or if income value greater than upper bound of bin (for bins under 25k)
              income_reporting_error = case_when(income_25 < income_lower_bound ~ 1,
                                               income_upper_bound < 25000 & income_25 > income_upper_bound ~ 1,
                                               TRUE ~ 0),
              # missingness in reported income value but not in income bin
              income_missing_value = case_when(is.na(income_25) == TRUE & is.na(income_bin) == FALSE ~ 1,
                                              TRUE ~ 0)) %>%
       # impute income values for responses with errors
       mutate(shock_income_25 = case_when(income_reporting_error == 1 | income_missing_value == 1 ~ income_bin_midpoint, 
                                        TRUE ~ income_25),
              shock_income_25_imputed = case_when(income_reporting_error == 1 | income_missing_value == 1 ~ 1,
                                                 TRUE ~ 0)) %>%
       select(-c(income_bin, income_lower_bound, income_upper_bound, income_bin_midpoint, income_value_under_25k, income_over_25k, income_reporting_error, income_missing_value)             
       ) %>%
       # relocate new income variables to be next to other shock variables
       relocate(c("shock_income_25", "shock_income_25_imputed"), .after = "shock_paid_other_25")

#### Timing of shock -------------------------------------------------------------