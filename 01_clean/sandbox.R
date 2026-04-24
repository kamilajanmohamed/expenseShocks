# something weird is going on with the self-reported income data
test <- cleaned %>%
       mutate(temp_lshock_hh_inc = lshock_hh_inc*12) %>%
       mutate(lshock_hh_inc_bin = factor(case_when(temp_lshock_hh_inc < 20000 ~ "- $20k",
                                          temp_lshock_hh_inc %in% c(20000:40000) ~ "$20k-40k",
                                          temp_lshock_hh_inc >40000 & temp_lshock_hh_inc <= 60000 ~ "$40k-60k",
                                          temp_lshock_hh_inc >60000 & temp_lshock_hh_inc <= 80000 ~ "$60k-80k",
                                          temp_lshock_hh_inc > 80000 & temp_lshock_hh_inc <= 100000 ~ "$80k-100k",
                                          temp_lshock_hh_inc >100000 & temp_lshock_hh_inc <= 125000 ~ "$100k-125k",
                                          temp_lshock_hh_inc > 125000 ~ "$125k +",
                                          TRUE ~ NA), 
                                          levels = c("- $20k",     "$20k-40k",   "$40k-60k",   "$60k-80k",   "$80k-100k", "$100k-125k", "$125k +"), ordered = TRUE),
              lshock_hh_inc_conflict = ifelse(income != lshock_hh_inc_bin, 1, 0)) %>%
       select(-c(temp_lshock_hh_inc)) %>%
       relocate(c("lshock_hh_inc_bin", "lshock_hh_inc_conflict"), .after = "lshock_hh_inc_imputed")

table(test$income, test$lshock_hh_inc_bin)

ggplot(test, aes(x = lshock_hh_inc_bin_annual, y = lshock_hh_inc)) +
geom_point()

