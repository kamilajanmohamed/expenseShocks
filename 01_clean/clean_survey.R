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
   any_shock= `Since the beginning of 2025, did your household have a large and unexpected expense?`,
    n_shock = `How many times since the beginning of 2025 did your household have a large and unexpected expense?`,
    lshock_type = `For the most recent time, what type of large and unexpected expense was this?`,
    lshock_type_other = `Some other large and unexpected expense, please specify::For the most recent time, what type of large and unexpected expense was this?`) %>%
  mutate(
   any_shock= case_when(any_shock == "Yes" ~ 1,
                                any_shock== "No" ~ 0,
                                  TRUE ~ NA),
    n_shock = factor(n_shock, 
                                 levels = c("1 time", "2 times", "3 times" , 
                                            "4 times", "5 to 10 times" , 
                                            "More than 10 times"), 
                                 ordered = TRUE),
  # first, create a variable for the type of most recent expense shock using raw response categories - includes "other"
  lshock_type = factor(lshock_type,levels = 
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
  lshock_type_other = tolower(lshock_type_other),
  # then, replace other vars in the lshock_type_variable with recategorised other responses where applicable
  lshock_type = case_when(lshock_type != "Other" ~ lshock_type,
                                lshock_type == "Other" & 
                                  str_detect(lshock_type_other, "funeral|burial|died") ~ "Funeral/Burial expenses",
                                         lshock_type == "Other" & 
                                  str_detect(lshock_type_other, "medical|dental|health|cancer|surgery|surgeries|sugeries|hospital|therapy") & 
                                  str_detect(lshock_type_other, "son|family|child") == FALSE ~ "Medical/Dental expenses",
                                         lshock_type == "Other" & 
                                  str_detect(lshock_type_other, "home|house|window|roof|basement|garage|driveway|crawl|deck|roof|window|lawn|pool|tree|pest|leak|plumber|plumbing|flood|paint|furnace|a/c|conditioner|aircon|furnace|electrical|appliance|hvac|freezer|fridge|refrigerator|stove| washer|dryer|heater|drainage|gas line|new guest bed|new recliner|heating system repair|hot tub|solar system|well water") & 
                                  str_detect(lshock_type_other, "house payment|purchase a home") == FALSE ~ "Home/Appliance repair",
                                         lshock_type == "Other" & 
                                  str_detect(lshock_type_other, "vehicle|auto|car|truck|auto|tire|boat|oil tank") & 
                                  str_detect(lshock_type_other, "we had to buy a trailer to use as another room to place my son's in so that we could take care of a family member who needed 24 hour care") == FALSE ~ "Vehicle repair/replacement",
                                         lshock_type == "Other" & 
                                  str_detect(lshock_type_other, "legal|tax|fine") ~ "Legal expenses/taxes/fines",
                                         lshock_type == "Other" & 
                                  str_detect(lshock_type_other, "moving|relocation|deposit") ~ "Moving/Relocation costs",
                                         lshock_type == "Other" & 
                                  str_detect(lshock_type_other, "\\brent\\b|mortgage|house payment")  ~ "Rent increase",
                                         lshock_type == "Other" & 
                                  str_detect(lshock_type_other, "veterinary|pet|\\bcat\\b|dog") ~ "Veterinary/Pet care expenses",
                                         lshock_type == "Other" & 
                                  str_detect(lshock_type_other, "computer|phone") ~ "Computer/Phone repair/replacement",
                                         lshock_type == "Other" & 
                                  str_detect(lshock_type_other, "childcare|dependent care") ~ "Childcare/Dependent care expenses",
                                         lshock_type == "Other" & 
                                  str_detect(lshock_type_other, "utility|electric|light bill") ~ "Utility bill increase",
                                         lshock_type == "Other" & 
                                  str_detect(lshock_type_other, "gift|loan|son|child|grandson|eldest|family|daughter") & 
                                  str_detect(lshock_type_other, "family members moved in with us|visit to family member whose time on earth was limited") == FALSE ~ "Gift/Loan to family/friend",
                                         TRUE ~ lshock_type)) %>%
  # drop the other categorisation column
  select(-c(lshock_type_other)) %>%
       # reorder to group expense shock variables together
       relocate(c("any_shock", "n_shock", "lshock_type"), 
               .after = "demo_weight")

#### Expense shock amounts -------------------------------------------------------------
cleaned <- cleaned %>%
  #rename variables for conciseness
  rename(expense_bin = `About how much money was the total cost for this large and unexpected expense?  Please estimate the full amount of this expense even if you did not pay all of it at once or if you are paying over time.`,
         expense_value_under_20k = `You previously stated [question('value'), id='6']. Specifically, what was the approximate total cost for this large and unexpected expense?`,
         expense_value_over_20k = `You previously stated $20,000 or more. Specifically, what was the approximate total cost for this large and unexpected expense?`) %>%
       # coalesce the reported expense values into one column 
       mutate(lshock_size = coalesce(expense_value_under_20k, expense_value_over_20k)) %>%
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
              reporting_error = case_when(lshock_size < lower_bound ~ 1,
                                          upper_bound < 20000 &lshock_size > upper_bound ~ 1,
                                                 TRUE ~ 0),
              # missingness in reported expense value but not in expense bin
              missing_expense_value = case_when(is.na(lshock_size) == TRUE & is.na(expense_bin) == FALSE ~ 1,
                                              TRUE ~ 0)) %>%
       # impute expense shock sizes for responses with errors 
       mutate(lshock_size = case_when(reporting_error == 1 | missing_expense_value == 1 ~ bin_midpoint, 
                                                     TRUE ~lshock_size),
             lshock_size_imputed = case_when(reporting_error == 1 | missing_expense_value == 1 ~ 1,
                                                            TRUE ~ 0)) %>%
       select(-c(expense_bin, lower_bound, upper_bound, bin_midpoint, reporting_error, missing_expense_value, expense_value_over_20k, expense_value_under_20k)) %>%
       # rearrange expense shock variables
       relocate(c("lshock_size", "lshock_size_imputed"), .after = "lshock_type")

#### Expense shock repayment -------------------------------------------------------------
cleaned <- cleaned %>%
       # rename variables
       rename(lshock_paid_3m = `Did you pay the full amount of this large and unexpected expense within three months after you had this expense?`,
              lshock_amt_paid_3m = `Within three months after you had this large and unexpected expense, about how much of it had you paid off?`,
              lshock_paid_csacct = `I used money in checking and savings accounts:How did you pay for this large and unexpected expense? Please select all that apply.`,
              lshock_paid_retacct = `I used money in retirement accounts:How did you pay for this large and unexpected expense? Please select all that apply.`,
              lshock_paid_ccard_sm =  `I paid it using my credit card and paid the full amount at the end of the month:How did you pay for this large and unexpected expense? Please select all that apply.`,
              lshock_paid_ccard_mm = `I paid it using my credit card and paid the amount over multiple months:How did you pay for this large and unexpected expense? Please select all that apply.`,
              lshock_paid_ffloan = `I borrowed from family and/or friends:How did you pay for this large and unexpected expense? Please select all that apply.`,
              lshock_paid_pdloan = `I used a payday or auto-title loan:How did you pay for this large and unexpected expense? Please select all that apply.`,
              lshock_paid_bloan = `I took a loan from a bank or a credit union:How did you pay for this large and unexpected expense? Please select all that apply.`,
              lshock_paid_heloc = `I took out or used a home equity line of credit:How did you pay for this large and unexpected expense? Please select all that apply.`,
              lshock_paid_redspend = `I reduced my spending on other expenses:How did you pay for this large and unexpected expense? Please select all that apply.`,
              lshock_paid_mispay = `I missed some bills or other payments:How did you pay for this large and unexpected expense? Please select all that apply.`,
              lshock_paid_incinc = `I increased my income by working more hours or at another job:How did you pay for this large and unexpected expense? Please select all that apply.`,
              lshock_paid_solbel = `I sold some belongings:How did you pay for this large and unexpected expense? Please select all that apply.`,
              lshock_paid_dk = `I’m not sure / I don’t recall:How did you pay for this large and unexpected expense? Please select all that apply.`,
              lshock_paid_other = `Other, please specify::How did you pay for this large and unexpected expense? Please select all that apply....43`,
              lshock_paid_other_text = `Other, please specify::How did you pay for this large and unexpected expense? Please select all that apply....45`) %>%
       # clean shock repayment variables
       mutate(
              lshock_paid_3m = factor(lshock_paid_3m, levels = c("Yes, I <u>paid all</u> of the expense within three months" , "No, I <u>paid only some</u> of the expense within three months", "No, I <u>did not pay any</u> of the expense within three months"), 
                                                         labels = c("Paid in full within 3 months", "Partially paid within 3 months", "Not paid at all within 3 months"), 
                                                         ordered = TRUE),
              lshock_amt_paid_3m = factor(lshock_amt_paid_3m, levels = c("I only paid very little (close to 0%)", "I paid about half (50%) of the expense", "I paid about three-quarters (75%) of the expense", "I paid almost all (almost 100%) of the expense"), 
                                                                      labels = c("About 0%", "About 50%", "About 75%", "About 100%"), 
                                                                      ordered = TRUE)) %>%
       # if you paid 0% or 100% within 3 months in 2025 from lshock_paid_3m, add levels for 100% paid and 0% paid to lshock_amt_paid_3m
       mutate(lshock_amt_paid_3m = case_when(lshock_paid_3m == "Not paid at all within 3 months" & is.na(lshock_amt_paid_3m) == TRUE ~ "0%",
                                                 lshock_paid_3m == "Paid in full within 3 months" & is.na(lshock_amt_paid_3m) == TRUE ~ "100%",
                                                 TRUE ~ lshock_amt_paid_3m)) %>%
       # add and reorder factor levels
       mutate(lshock_amt_paid_3m = factor(lshock_amt_paid_3m, levels = c("0%", "About 0%", "About 50%", "About 75%", "About 100%", "100%"), ordered = TRUE)) %>%
       # recode shock repayment method variables as dummies. if shock amt paid is NA, all of these should also be NAs
       mutate(across(c(lshock_paid_csacct, lshock_paid_retacct, lshock_paid_ccard_sm,
                            lshock_paid_ccard_mm, lshock_paid_ffloan, lshock_paid_pdloan,
                            lshock_paid_bloan, lshock_paid_heloc, lshock_paid_redspend,
                            lshock_paid_mispay, lshock_paid_incinc, lshock_paid_solbel,
                            lshock_paid_dk, lshock_paid_other),
                            ~ as.integer(!is.na(.x)))) %>%
       # lower case free test responses for easier searching
       mutate(lshock_paid_other_text = tolower(lshock_paid_other_text)) %>%
       # reassign free text reponses where possible using string searching. intentionally not using an LLM for reproducibility and transparency
       mutate(
              lshock_paid_csacct = if_else(
    lshock_paid_other == 1 &
      str_detect(lshock_paid_other_text,
        "\\bcash\\b|insurance|\\bhsa\\b|tax|inheritance|christmas money|dedicated account|salary||savings|checking|check|work bonus|deductible|out of pocket|debit|deposit"),
    1L, lshock_paid_csacct, lshock_paid_csacct),

  lshock_paid_retacct = if_else(
    lshock_paid_other == 1 &
      str_detect(lshock_paid_other_text, "401k|\\bira\\b|retirement"),
    1L, lshock_paid_retacct, lshock_paid_retacct),

  lshock_paid_ccard_mm = if_else(
              lshock_paid_other == 1 &
              str_detect(lshock_paid_other_text,
                     "credit card|credit line"),
              1, lshock_paid_ccard_mm, lshock_paid_ccard_mm),

              lshock_paid_ffloan = if_else(
              lshock_paid_other == 1 &
              str_detect(lshock_paid_other_text, "son|church|go.?fund.?me"),
              1L, lshock_paid_ffloan, lshock_paid_ffloan),

              lshock_paid_bloan = if_else(
              lshock_paid_other == 1 &
              str_detect(lshock_paid_other_text,
                     "loan|interest|financed|financing") &
              !str_detect(lshock_paid_other_text,
                     "401k|retirement|\\bira\\b|care credit|payment plan|affirm|medical credit"),
              1L, lshock_paid_bloan, lshock_paid_bloan),

              lshock_paid_heloc = if_else(
              lshock_paid_other == 1 &
              str_detect(lshock_paid_other_text, "refinanc"),
              1L, lshock_paid_heloc, lshock_paid_heloc),

              lshock_paid_incinc = if_else(
              lshock_paid_other == 1 &
              str_detect(lshock_paid_other_text, "bonus|recycl|whored"),
              1L, lshock_paid_incinc, lshock_paid_incinc),

              lshock_paid_solbel = if_else(
              lshock_paid_other == 1 &
              str_detect(lshock_paid_other_text,
                     "stock|sell|sold|salvage|cashed"),
              1L, lshock_paid_solbel, lshock_paid_solbel)) %>%
       # drop cleaned free text variable
       select(-c(lshock_paid_other_text)) %>%
       # remove other flag for recategorised values in lshock_paid_other
       mutate(reclassified = case_when(lshock_paid_other == 1 & (lshock_paid_csacct == 1 | lshock_paid_retacct == 1 | lshock_paid_ccard_mm == 1 | lshock_paid_ffloan == 1 | lshock_paid_bloan == 1 | lshock_paid_heloc == 1 | lshock_paid_incinc == 1 | lshock_paid_solbel == 1) ~ 1,
                                    TRUE ~ 0),
              lshock_paid_other = case_when(reclassified == 1 ~ 0,
                                              TRUE ~ lshock_paid_other)) %>%
       # drop reclassified flag variable
       select(-c(reclassified)) %>%
       # reorder variables to group shock repayment variables together
       relocate(c("lshock_paid_3m", "lshock_amt_paid_3m", "lshock_paid_csacct", "lshock_paid_retacct", "lshock_paid_ccard_sm",
                  "lshock_paid_ccard_mm", "lshock_paid_ffloan", "lshock_paid_pdloan",
                  "lshock_paid_bloan", "lshock_paid_heloc", "lshock_paid_redspend",  
                  "lshock_paid_mispay", "lshock_paid_incinc", "lshock_paid_solbel",
                  "lshock_paid_dk", "lshock_paid_other"), .after = "lshock_size_imputed")

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
       mutate(lshock_hh_inc = case_when(income_reporting_error == 1 | income_missing_value == 1 ~ income_bin_midpoint, 
                                        TRUE ~ income_25),
              lshock_hh_inc_imputed = case_when(income_reporting_error == 1 | income_missing_value == 1 ~ 1,
                                                 TRUE ~ 0)) %>%
       select(-c(income_bin, income_lower_bound, income_upper_bound, income_bin_midpoint, income_value_under_25k, income_over_25k, income_reporting_error, income_missing_value)             
       ) %>%
       # relocate new income variables to be next to other shock variables
       relocate(c("lshock_hh_inc", "lshock_hh_inc_imputed"), .after = "lshock_paid_other")

#### Timing of shock -------------------------------------------------------------
cleaned <- cleaned %>%
        #rename variables for conciseness
              rename(shock_window = `Approximately when did this large and unexpected expense occur?`,
                     shock_month_1 = `In approximately what month did you have this large and unexpected expense?...50`,
                     shock_month_2 = `In approximately what month did you have this large and unexpected expense?...51`,
                     shock_month_3 = `In approximately what month did you have this large and unexpected expense?...52`,
                     shock_month_4 = `In approximately what month did you have this large and unexpected expense?...53`,
                     shock_month_5 = `In approximately what month did you have this large and unexpected expense?...54` ) %>%
       # create helper variables  
       mutate(shock_window_lower = case_when(shock_window == "In January, February, or March of 2025" ~ as.Date("2025-01-01"),
                                              shock_window == "In April, May, or June of 2025" ~ as.Date("2025-04-01"),
                                              shock_window == "In July, August, or September of 2025" ~ as.Date("2025-07-01"),
                                              shock_window == "In October, November, or December of 2025" ~ as.Date("2025-10-01"),
                                              shock_window == "In January, February, or March of 2026" ~ as.Date("2026-01-01"),
                                              TRUE ~ NA),
              shock_window_upper = case_when(shock_window == "In January, February, or March of 2025" ~ as.Date("2025-03-31"),
                                                 shock_window == "In April, May, or June of 2025" ~ as.Date("2025-06-30"),
                                              shock_window == "In July, August, or September of 2025" ~ as.Date("2025-09-30"),
                                              shock_window == "In October, November, or December of 2025" ~ as.Date("2025-12-31"),
                                              shock_window == "In January, February, or March of 2026" ~ as.Date("2026-03-31"),
                                              TRUE ~ NA),
              shock_window_midpoint = case_when(shock_window == "In January, February, or March of 2025" ~ as.Date("2025-02-01"),
                                                   shock_window == "In April, May, or June of 2025" ~ as.Date("2025-05-01"),
                                                   shock_window == "In July, August, or September of 2025" ~ as.Date("2025-08-01"),
                                                   shock_window == "In October, November, or December of 2025" ~ as.Date("2025-11-01"),
                                                   shock_window == "In January, February, or March of 2026" ~ as.Date("2026-02-01"),
                                                   TRUE ~ NA )) %>%
       # convert numbers to dates using excel's known epoch date system. if the response is not a number, it will be converted to NA
       mutate(across(c(shock_month_1, shock_month_2, shock_month_3, shock_month_4, shock_month_5), 
                     ~ as.Date(as.numeric(.x), origin = "1899-12-30"))) %>%
       # none of the respondents have multiple reported shock months - safe to coalesce into one variable if needed for analysis of shock timing
       mutate(lshock_month = coalesce(shock_month_1, shock_month_2, shock_month_3, shock_month_4, shock_month_5)) %>%
       # impute values if shock window is given but shock month is missing OR shock month falls outside of shock window 
       mutate(lshock_month_imputed = case_when(is.na(shock_window) == FALSE & is.na(lshock_month) == TRUE ~ 1,
                                         is.na(shock_window) == FALSE & (lshock_month < shock_window_lower | lshock_month > shock_window_upper) ~ 1,
                                         TRUE ~ 0),
       lshock_month = case_when(lshock_month_imputed == 1 ~ shock_window_midpoint,
                                        TRUE ~ lshock_month)) %>%
       # drop helper variables
       select(-c(shock_window_lower, shock_window_upper, shock_window_midpoint, shock_month_1, shock_month_2, shock_month_3, shock_month_4, shock_month_5, shock_window)) %>%
       # relocate shock month variable to be next to shock window variable
       relocate(lshock_month, lshock_month_imputed, .after = "lshock_hh_inc_imputed")
       
#### Predictability and preventability -------------------------------------------------------------
cleaned <- cleaned %>%
       # rename_variables for conciseness
       rename(lshock_pred = `Right before this large and unexpected expense occurred, what did you think were the chances this event would occur?`,
              lshock_prev = `Looking back at this large and unexpected expense, could your household have taken steps to prevent it from happening or reduce its cost?`) %>%
       # clean forecasting as factor variable
       mutate(lshock_pred = factor(lshock_pred, levels = c("I did not think it would happen at all (about 0% chance)",
                                                                      "I thought it was unlikely to happen (about 25% chance)" ,
                                                                      "I thought it was equally likely and unlikely to happen (about 50% chance)",
                                                                      "I thought it was likely to happen (about 75% chance)",
                                                                      "I thought it would definitely happen (about 100% chance)"),
                                                         labels = c("Impossible (0% chance)", "Unlikely (25% chance)", "Equally likely and unlikely(50% chance)", "Likely (75% chance)", "Definitely (100% chance)"))) %>%
       # clean prevention as dummy
       mutate(lshock_prev = case_when(lshock_prev == "Yes, we could have prevented it or reduced the cost of the expense (for example, with preventive care, maintenance, or insurance)" ~ 1,
                                          lshock_prev == "No, there was nothing we could have done to prevent it or reduce the cost" ~ 0,
                                          TRUE ~ NA))

#### Liquidity and credit constraints -------------------------------------------------------------
cleaned <- cleaned %>%
       #rename variables for conciseness
       rename(hh_liquidity = `At the time you had this large and unexpected expense, about how much money did your household have in all your checking and savings accounts (excluding retirement accounts)?`,
              hh_emergency_fund = `At the time you had this large and unexpected expense, about how many months of your household’s typical monthly expenses could you cover using money you had saved in all your checking and savings accounts (excluding retirement accounts)?`,
              hh_credit = `At the time you had this large and unexpected expense, did you have access to a credit card that could cover the full expense cost?`,
              hh_credit_score = `At the time you had this large and unexpected expense, what was your credit score?`) %>%
       # clean variables
       mutate(hh_liquidity = factor(hh_liquidity, levels = c("$0 - $100", "$100 - $500", "$500 - $1,000", "$1,000 - $2,500" ,  
                                                               "$2,500 - $5,000" , "$5,000 - $7,500", "$7,500 - $10,000", 
                                                               "$10,000 - $20,000", "$20,000 - $50,000",  "More than $50,000"),
                                                               ordered = TRUE),
              hh_emergency_fund = factor(hh_emergency_fund, levels = c("Less than 1 month", "1 month", "2 months", "3 months", 
                                                                      "4 months", "5 months", "6 months", "7 months", "8 months", "9 months", "10 months", "11 months", 
                                                                      "12 months", "More than 12 months"),
                                                               ordered = TRUE),
              hh_credit = factor(hh_credit, levels = c("No, I did not have access to any credit cards", 
                                                        "Yes, but I did not have enough available credit to cover the full expense", 
                                                        "Yes, and I had enough available credit to cover the full expense"), 
                                                 labels = c("No credit", "Insufficient credit", "Sufficient credit"),
                                                 ordered = TRUE),
              hh_credit_score = factor(hh_credit_score, levels = c("300-540",  "540-600", "600-660", "660-720", "720-780", "780-850"),
                                                        ordered = TRUE))


#### Other shocks -------------------------------------------------------------
cleaned <- cleaned %>%
       # rename variables for conciseness
       rename(other_shocks = `Looking back at when you experienced this large and unexpected expense, did you experience any additional, smaller unexpected expenses during the same month?`,
              other_shock_totsize = `To the best of your memory, what was the approximate total cost for all unexpected expenses you experienced that month?`) %>%
       # clean other shocks variables
       mutate(other_shocks = case_when(other_shocks == "Yes" ~ 1,
                                      other_shocks == "No" ~ 0,
                                      TRUE ~ NA),
              other_shock_totsize = as.numeric(other_shock_totsize))