#############################################################
# Author: Kamila Janmohamed
# Date: 2026-04-22
# Description: Helper functions for cleaning survey data
#############################################################

# Bin imputation ------------------------------------------------------------
impute_bin  <- function(dataframe, bin_var, value_var, threshold, out_col, imputed_col) {

       dataframe %>%
        mutate(
            # extract lower bin bound
            lower_bound = case_when(str_detect(dataframe[[bin_var]], "-") ~
                           as.numeric(gsub("[^0-9]", "", sub("-.*", "", dataframe[[bin_var]]))),
                         str_detect(dataframe[[bin_var]], "More than| \\+") ~ threshold,
                         TRUE ~ NA_real_),
            # extract upper bin bound
            upper_bound = case_when(str_detect(dataframe[[bin_var]], "-") ~
                           as.numeric(gsub("[^0-9]", "", sub(".*-", "", dataframe[[bin_var]]))),
                         str_detect(dataframe[[bin_var]], "More than") ~ threshold,
                         TRUE ~ NA_real_),
            # calculate bin midpoint
            midpoint = (lower_bound + upper_bound) / 2,
            # identify errors where value is outside of bin bounds
            reporting_error  = case_when(dataframe[[value_var]] < lower_bound ~ 1,
                        upper_bound < threshold & dataframe[[value_var]] > upper_bound ~ 1,
                        TRUE ~ 0),
            # identify missing values where value is NA but bin is not NA
            missing_value = if_else(is.na(dataframe[[value_var]]) & !is.na(dataframe[[bin_var]]), 1, 0),
            # impute value with bin midpoint if error or missing, otherwise keep original value
            "{out_col}"     := case_when(reporting_error == 1 | missing_value == 1 ~ midpoint, TRUE ~ .data[[value_var]]),
            # create imputed indicator variable: NA = no bin reported, 1 = imputed, 0 = original value
            "{imputed_col}" := case_when(is.na(dataframe[[bin_var]]) ~ NA, reporting_error == 1 | missing_value == 1 ~ 1, TRUE ~ 0)
        ) %>%
        select(-c(all_of(bin_var), lower_bound, upper_bound, midpoint, reporting_error, missing_value))
}

# Process advrerse outcomes ------------------------------------------------------------
process_adverse_outcomes <- function(data, suffix) {

  # add suffix to variable names 
  vars <- paste0(c("pastdue_lcc", "pastdue_rm", "collections",
                   "utility_cutoff", "repo_notice", "eviction_notice",
                   "no_adverse_outcome"), suffix)
  #variables to drop
  drop <- paste0(c("dont_recall_outcome", "decline_to_answer_outcome"), suffix)
  # 
  data %>%
  # create helper variable: dummy for whether they responded to at least one category
    mutate(resp_temp = if_any(all_of(vars), ~ !is.na(.x))) %>%
     # turn underlying variables into dummies
    mutate(across(all_of(vars), ~ case_when(!is.na(.x) ~ 1,
                                             is.na(.x) & resp_temp > 0 ~ 0,
                                             TRUE ~ NA))) %>%
    select(-c(resp_temp, all_of(drop)))
}