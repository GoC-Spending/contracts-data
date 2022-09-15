# More specific / one-off research analyses for the "Findings" section of our research paper
# In addition to lib/research_findings.R which in most cases is run against the full dataset

# These are designed to be run after the main set of contracts is parsed, in load.R
# Note: if run directly here, this will take 3-4 hours to process first (!)
source("load.R")

# Dataframes:

# contract_spending_by_date is the per-day breakdown of spending,
# useful for calculating (estimated) actual spending over time.

# contract_spending_overall includes a row for each "group" representing
# a contract and its amendments.

# contract_spending_overall_initiated includes entries from contract_spending_overall
# that were new since the start of the summary_start_fiscal_year_short fiscal year

# contract_spending_overall_ongoing includes entries from contract_spending_overall
# that have been active since the start of the summary_start_fiscal_year_short fiscal year


# Analysis thresholds ===========================

# $10M max
threshold_standish_rule_value <- 10000000

# $2M max
threshold_jaquith_rule_value <- 2000000

# 2 years max, in days
threshold_standish_rule_duration_days <- 2 * 365

# 6 months max, in days
threshold_jaquith_rule_duration_days <- ceiling(365/2)


# Helper functions ==============================

# Designed to be run against any of the contract_spending_overall_* dataframes

add_total_number_of_days <- function(df) {
  
  # Reuse the calculate duration function in research_findings.R
  # Note that this removes entries that are longer than summary_maximum_duration_cutoff_years
  # It adds duration_days and duration_years columns
  df %>%
    s42_calculate_duration()
  
}

filter_to_information_technology <- function(df) {
  
  df %>%
    filter(d_most_recent_category == "3_information_technology")
  
}

filter_to_it_consulting_services <- function(df) {
  
  df %>%
    filter(d_most_recent_it_subcategory == "it_consulting_services")
  
}

filter_to_it_consulting_services_and_software_licensing <- function(df) {
  
  included_subcategories <- c("it_consulting_services", "it_software_licensing")
  
  df %>%
    filter(d_most_recent_it_subcategory %in% included_subcategories)
  
}

filter_to_active_during_fiscal_year <- function(df, target_fiscal_year) {
  
  # If it's active during the target fiscal year, that means that
  #  1) it ends after the start of the target fiscal year
  #  2) it doesn't start after the end of the target fiscal year
  df %>%
    filter(
      d_overall_end_date >= ymd(str_c(target_fiscal_year,"04","01"))
    ) %>%
    filter(
      !d_overall_start_date > ymd(str_c(target_fiscal_year,"03","31"))
    )
  
}

filter_to_initiated_during_fiscal_year <- function(df, target_fiscal_year) {
  
  # If it's initiated during the target fiscal year, that means that
  #  1) it started on or after the start of the target fiscal year
  #  2) it didn't start after the end of the target fiscal year
  df %>%
    filter(
      d_overall_start_date >= ymd(str_c(target_fiscal_year,"04","01"))
    ) %>%
    filter(
      !d_overall_start_date > ymd(str_c(target_fiscal_year + 1,"03","31"))
    )
  
}


# Value threshold indicators ====================

a61_namesake_rule_value <- function(df, threshold_value, label = "namesake") {
  
  col_name_meets_rule = str_c("meets_", label, "_rule")
  
  # Note: reduces the total number of contracts by filtering out contracts without a value specified.
  df <- df %>%
    filter(!is.na(d_overall_contract_value)) %>%
    mutate(
      meets_rule = case_when(
        d_overall_contract_value < !!threshold_value ~ 1,
        TRUE ~ 0
      )
    )
  
  df %>%
    group_by(meets_rule) %>%
    summarise(
      count = n(),
      total = sum(d_overall_contract_value, na.rm = TRUE)
      ) %>%
    summarize_add_count_percentage() %>%
    summarize_add_total_percentage() %>%
    exports_round_percentages() %>%
    rename(
      !!col_name_meets_rule := "meets_rule"
    )
  
}

a611_standish_rule_value <- function(df) {
 
  df %>%
    a61_namesake_rule_value(threshold_standish_rule_value, "standish")
   
}

a612_jaquith_rule_value <- function(df) {
  
  df %>%
    a61_namesake_rule_value(threshold_jaquith_rule_value, "jaquith")
  
}

# Run it against two comparison fiscal years

# Against all time (example)
# contract_spending_overall %>% 
#   filter_to_information_technology() %>% 
#   a611_standish_rule_value()

# Standish threshold

# 2017-2018 fiscal year
contract_spending_overall %>% 
  filter_to_active_during_fiscal_year(2017) %>%
  filter_to_information_technology() %>% 
  a611_standish_rule_value()

# 2021-2022 fiscal year
contract_spending_overall %>% 
  filter_to_active_during_fiscal_year(2021) %>%
  filter_to_information_technology() %>% 
  a611_standish_rule_value()

# Specifically for IT consulting services and software licensing
# Key finding:
contract_spending_overall %>% 
  filter_to_active_during_fiscal_year(2021) %>%
  filter_to_it_consulting_services_and_software_licensing() %>% 
  a611_standish_rule_value()

# Same but with the Jaquith threshold

# 2017-2018 fiscal year
contract_spending_overall %>% 
  filter_to_active_during_fiscal_year(2017) %>%
  filter_to_information_technology() %>% 
  a612_jaquith_rule_value()

# 2021-2022 fiscal year
contract_spending_overall %>% 
  filter_to_active_during_fiscal_year(2021) %>%
  filter_to_information_technology() %>% 
  a612_jaquith_rule_value()

# Same but for IT consulting services

# 2017-2018 fiscal year
contract_spending_overall %>% 
  filter_to_active_during_fiscal_year(2017) %>%
  filter_to_it_consulting_services() %>% 
  a612_jaquith_rule_value()

# 2021-2022 fiscal year
contract_spending_overall %>% 
  filter_to_active_during_fiscal_year(2021) %>%
  filter_to_it_consulting_services() %>% 
  a612_jaquith_rule_value()


# Same for IT consulting services & software licensing


# 2017-2018 fiscal year
contract_spending_overall %>% 
  filter_to_active_during_fiscal_year(2017) %>%
  filter_to_it_consulting_services_and_software_licensing() %>% 
  a612_jaquith_rule_value()

# 2021-2022 fiscal year
# Key finding:
contract_spending_overall %>% 
  filter_to_active_during_fiscal_year(2021) %>%
  filter_to_it_consulting_services_and_software_licensing() %>% 
  a612_jaquith_rule_value()


# Using the Jaquith threshold with contracts initiated in that year

# 2017-2018 fiscal year
contract_spending_overall %>% 
  filter_to_initiated_during_fiscal_year(2017) %>%
  filter_to_it_consulting_services_and_software_licensing() %>% 
  a612_jaquith_rule_value()

# 2021-2022 fiscal year
contract_spending_overall %>% 
  filter_to_initiated_during_fiscal_year(2021) %>%
  filter_to_it_consulting_services_and_software_licensing() %>% 
  a612_jaquith_rule_value()


# Duration threshold indicators ====================

a62_namesake_rule_duration <- function(df, threshold_value, label = "namesake") {
  
  col_name_meets_rule = str_c("meets_", label, "_rule")
  
  df <- df %>%
    add_total_number_of_days()
  
  # Note: reduces the total number of contracts by filtering out contracts without a value specified.
  df <- df %>%
    filter(!is.na(duration_days)) %>%
    mutate(
      meets_rule = case_when(
        duration_days < !!threshold_value ~ 1,
        TRUE ~ 0
      )
    )
  
  df %>%
    group_by(meets_rule) %>%
    summarise(
      count = n(),
      total = sum(d_overall_contract_value, na.rm = TRUE)
    ) %>% 
    summarize_add_count_percentage() %>%
    summarize_add_total_percentage() %>%
    exports_round_percentages() %>%
    rename(
      !!col_name_meets_rule := "meets_rule"
    )
  
}

a621_standish_rule_duration <- function(df) {
  
  df %>%
    a62_namesake_rule_duration(threshold_standish_rule_duration_days, "standish")
  
}

a622_jaquith_rule_duration <- function(df) {
  
  df %>%
    a62_namesake_rule_duration(threshold_jaquith_rule_duration_days, "jaquith")
  
}

# Standish threshold

# 2017-2018 fiscal year
contract_spending_overall %>% 
  filter_to_active_during_fiscal_year(2017) %>%
  filter_to_information_technology() %>% 
  a621_standish_rule_duration()

# 2021-2022 fiscal year
contract_spending_overall %>% 
  filter_to_active_during_fiscal_year(2021) %>%
  filter_to_information_technology() %>% 
  a621_standish_rule_duration()

# Jaquith threshold

# 2017-2018 fiscal year
contract_spending_overall %>% 
  filter_to_active_during_fiscal_year(2017) %>%
  filter_to_information_technology() %>% 
  a622_jaquith_rule_duration()

# 2021-2022 fiscal year
contract_spending_overall %>% 
  filter_to_active_during_fiscal_year(2021) %>%
  filter_to_information_technology() %>% 
  a622_jaquith_rule_duration()

# Using the Jaquith threshold with contracts initiated in that year
# Note: this tends to undercount long-term (e.g. 10+ year) contracts
# Would recommend using filter_to_active_during_fiscal_year() instead

# 2017-2018 fiscal year
contract_spending_overall %>% 
  filter_to_initiated_during_fiscal_year(2017) %>%
  filter_to_information_technology() %>% 
  a622_jaquith_rule_duration()

# 2021-2022 fiscal year
contract_spending_overall %>% 
  filter_to_initiated_during_fiscal_year(2021) %>%
  filter_to_information_technology() %>% 
  a622_jaquith_rule_duration()


# Jaquith threshold with IT consulting services contracts

# 2017-2018 fiscal year
contract_spending_overall %>% 
  filter_to_active_during_fiscal_year(2017) %>%
  filter_to_it_consulting_services_and_software_licensing() %>% 
  a622_jaquith_rule_duration()

# 2021-2022 fiscal year
# Key finding:
contract_spending_overall %>% 
  filter_to_active_during_fiscal_year(2021) %>%
  filter_to_it_consulting_services_and_software_licensing() %>% 
  a622_jaquith_rule_duration()

# Standish threshold with IT consulting services contracts

# 2017-2018 fiscal year
contract_spending_overall %>% 
  filter_to_active_during_fiscal_year(2017) %>%
  filter_to_it_consulting_services_and_software_licensing() %>% 
  a621_standish_rule_duration()

# 2021-2022 fiscal year
# Key finding
contract_spending_overall %>% 
  filter_to_active_during_fiscal_year(2021) %>%
  filter_to_it_consulting_services_and_software_licensing() %>% 
  a621_standish_rule_duration()


# Ongoing during the time period
contract_spending_overall_ongoing %>% 
  filter_to_information_technology() %>% 
  a621_standish_rule_duration()

contract_spending_overall_ongoing %>% 
  filter_to_it_consulting_services() %>% 
  a621_standish_rule_duration()

contract_spending_overall_ongoing %>% 
  filter_to_information_technology() %>% 
  a622_jaquith_rule_duration()

contract_spending_overall_ongoing %>% 
  filter_to_it_consulting_services() %>% 
  a622_jaquith_rule_duration()



# Contracts that mention open source ============

a711_references_open_source <- function(df) {
  
  open_source_options <- "open source|open-source|opensource| oss "
  
  df <- df %>%
    mutate(
      is_open_source = case_when(
        str_detect(d_overall_description_comments_extended, open_source_options) ~ 1,
        TRUE ~ 0
      )
    )
  
  df %>%
    summarise(
      contracts = n(),
      contracts_that_reference_open_source = sum(is_open_source, na.rm = TRUE),
      percentage = contracts_that_reference_open_source / contracts
    ) %>%
    exports_round_percentages()
}

# Key finding:
contract_spending_overall_ongoing %>%
  filter_to_it_consulting_services_and_software_licensing() %>% 
  a711_references_open_source()


# IP terms ======================================
# For reference:
# https://www.tbs-sct.canada.ca/pol/doc-eng.aspx?id=14676

# A2 : Crown owned – exception 2, contractor ownership is precluded by prior obligations
# A3 : Crown owned – exception 3, contractor has no interest in owning Foreground IP
# A41 : Crown owned – exception 4.1, generate knowledge for public dissemination
# A42 : Crown owned – exception 4.2, augment existing body of Crown background
# A43 : Crown owned – exception 4.3, deliver a not-yet fully developed component or sub-system
# A5 : Crown owned – exception 5, Foreground IP consists of material subject to copyright
# A8 : Crown owned – exemption 8 (note: must have received approval of the Treasury Board via a Treasury Board submission)
# B : Contractor Owned
# C : No IP Terms in Contract

a712_crown_versus_contractor_owned_intellectual_property <- function(df) {
  
  # Exclude NA entries as well as "No IP terms in contract" (option C) entries:
  df <- df %>%
    filter(!is.na(d_intellectual_property)) %>%
    filter(d_intellectual_property != "C") %>%
    mutate(
      is_crown_owned_ip = case_when(
        str_detect(d_intellectual_property, "A") ~ 1,
        str_detect(d_intellectual_property, "B") ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    filter(!is.na(is_crown_owned_ip))
  
  # Note: test this with measuring by value
  df %>%
    summarise(
      contracts = n(),
      contracts_crown_owned_ip = sum(is_crown_owned_ip, na.rm = TRUE),
      percentage = contracts_crown_owned_ip / contracts
    ) %>%
    exports_round_percentages()
  
}

# Key finding:
contract_spending_overall_ongoing %>%
  filter_to_it_consulting_services_and_software_licensing() %>% 
  a712_crown_versus_contractor_owned_intellectual_property()
