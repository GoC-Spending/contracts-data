# More specific / one-off research analyses for the "Findings" section of our research paper
# In addition to lib/research_findings.R which in most cases is run against the full dataset

# These are designed to be run after the main set of contracts is parsed, in load.R
# Note: if run directly here, this will take 3-4 hours to process first (!)
# source("load.R")

# Dataframes:

# contract_spending_by_date is the per-day breakdown of spending,
# useful for calculating (estimated) actual spending over time.

# contract_spending_overall includes a row for each "group" representing
# a contract and its amendments.

# contract_spending_overall_initiated includes entries from contract_spending_overall
# that were new since the start of the summary_start_fiscal_year_short fiscal year

# contract_spending_overall_ongoing includes entries from contract_spending_overall
# that have been active since the start of the summary_start_fiscal_year_short fiscal year

# contract_spending_overall_active includes entries from contract_spending_overall
# that have been active since the start of the summary_start_fiscal_year_short fiscal year, and excludes contracts that were initiated after the end of summary_end_fiscal_year_short
# From 2022-09-30 on we'll use contract_spending_overall_active in most overall analysis calculations.


# Analysis thresholds ===========================

# $10M max
threshold_standish_rule_value <- 10000000

# $2M max
threshold_jaquith_rule_value <- 2000000

# $2M max *per year*
# https://github.com/18F/technology-budgeting/blob/master/handbook.pdf
threshold_handbook_rule_value <- 2000000

# 2 years max, in days
threshold_standish_rule_duration_days <- 2 * 365

# 6 months max, in days
threshold_jaquith_rule_duration_days <- ceiling(365/2)

# 3 years max, in days
threshold_handbook_rule_duration_days <- 3 * 365


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
      !d_overall_start_date > ymd(str_c(target_fiscal_year + 1,"03","31"))
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

format_revert_output_totals <- function(df) {
  
  df <- df %>%
    mutate(
      # Converts to double to avoid string type issues in future numeric operations
      total = as.double(total),
      total_constant_2019_dollars = as.double(total_constant_2019_dollars)
    )
  
  df
  
}


# Value threshold indicators ====================

a61_namesake_rule_value <- function(df, threshold_value, label = "namesake", value_threshold_is_annual = FALSE, provide_rule_breaking_summary = FALSE) {
  
  col_name_meets_rule = str_c("meets_", label, "_rule")
  
  if(value_threshold_is_annual == TRUE) {
    # Use the ceiling year to handle contracts taking place "over the course of" a given year or set of years
    # e.g. a duration of 0.6 years becomes 1 year
    # a duration of 2.9 years becomes 3 years
    # As a result this is slightly gentler on contracts that might be slightly above $2M per year if calculated to decimal portions of years.
    df <- df %>%
      filter(!is.na(d_overall_contract_value)) %>%
      s42_calculate_duration() %>%
      mutate(
        duration_years_ceiling = ceiling(duration_years),
        d_contract_value_per_year = d_overall_contract_value / duration_years_ceiling
      ) %>%
      mutate(
        # Note, the handbook uses "no more than", so switching to <= here.
        meets_rule = case_when(
          d_contract_value_per_year <= !!threshold_value ~ 1,
          TRUE ~ 0
        )
      )
    
  }
  else {
    # Note: reduces the total number of contracts by filtering out contracts without a value specified.
    df <- df %>%
      filter(!is.na(d_overall_contract_value)) %>%
      mutate(
        meets_rule = case_when(
          d_overall_contract_value < !!threshold_value ~ 1,
          TRUE ~ 0
        )
      )
  }

  
  if(provide_rule_breaking_summary == TRUE) {
    
    df %>%
      filter(meets_rule == 0) %>%
      summarize(
        average_value = mean(d_overall_contract_value, na.rm = TRUE),
        min_value = min(d_overall_contract_value, na.rm = TRUE),
        max_value = max(d_overall_contract_value, na.rm = TRUE)
      )
    
  }
  else {
    
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

  
}

a611_standish_rule_value <- function(df) {
 
  df %>%
    a61_namesake_rule_value(threshold_standish_rule_value, "standish")
   
}

a612_jaquith_rule_value <- function(df) {
  
  df %>%
    a61_namesake_rule_value(threshold_jaquith_rule_value, "jaquith")
  
}

a613_handbook_rule_value <- function(df, provide_rule_breaking_summary = FALSE) {
  
  df %>%
    a61_namesake_rule_value(threshold_handbook_rule_value, "handbook", TRUE, provide_rule_breaking_summary)
  
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
contract_spending_overall %>% 
  filter_to_active_during_fiscal_year(2021) %>%
  filter_to_it_consulting_services_and_software_licensing() %>% 
  a612_jaquith_rule_value()

# 2021-2022 fiscal year
# using the handbook threshold for value ($2M per year)
contract_spending_overall %>% 
  filter_to_active_during_fiscal_year(2021) %>%
  filter_to_it_consulting_services_and_software_licensing() %>% 
  a613_handbook_rule_value()

# Updated to use the full 5-year time range
# Key finding:
# "nearly all of them were no more than $2 million per year ([34,147] of [34,558] contracts)"
contract_spending_overall_active %>%
  filter_to_it_consulting_services_and_software_licensing() %>% 
  a613_handbook_rule_value()

# Key finding:
# "Amongst these ‘rule breaking’ contracts, the average contract value was [$25M], with a range of just over [$2M] to [$1.1B]."
contract_spending_overall_active %>%
  filter_to_it_consulting_services_and_software_licensing() %>% 
  a613_handbook_rule_value(TRUE)

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

a62_namesake_rule_duration <- function(df, threshold_value, label = "namesake", provide_rule_breaking_summary = FALSE) {
  
  col_name_meets_rule = str_c("meets_", label, "_rule")
  
  df <- df %>%
    add_total_number_of_days()
  
  # Note: reduces the total number of contracts by filtering out contracts without a duration specified.
  df <- df %>%
    filter(!is.na(duration_days)) %>%
    # Also skip contracts without a value for consistency in number size with the value calculations.
    filter(!is.na(d_overall_contract_value)) %>%
    # Note, switching this to <= ("no more than")
    mutate(
      meets_rule = case_when(
        duration_days <= !!threshold_value ~ 1,
        TRUE ~ 0
      )
    )
  
  if(provide_rule_breaking_summary == TRUE) {
    
    df %>%
      filter(meets_rule == 0) %>%
      summarize(
        average_years = mean(duration_years, na.rm = TRUE),
        min_years = min(duration_years, na.rm = TRUE),
        max_years = max(duration_years, na.rm = TRUE)
      )
    
  }
  else {
    
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
    
}

a621_standish_rule_duration <- function(df) {
  
  df %>%
    a62_namesake_rule_duration(threshold_standish_rule_duration_days, "standish")
  
}

a622_jaquith_rule_duration <- function(df) {
  
  df %>%
    a62_namesake_rule_duration(threshold_jaquith_rule_duration_days, "jaquith")
  
}

a623_handbook_rule_duration <- function(df, provide_rule_breaking_summary = FALSE) {
  
  df %>%
    a62_namesake_rule_duration(threshold_handbook_rule_duration_days, "handbook", provide_rule_breaking_summary)
  
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
contract_spending_overall %>% 
  filter_to_active_during_fiscal_year(2021) %>%
  filter_to_it_consulting_services_and_software_licensing() %>% 
  a622_jaquith_rule_duration()

# Same but with the Handbook threshold (3 years)
# Key finding:
# "91% of IT consulting services and software licensing contracts in the dataset are no more than 3 years in duration ([31,364] of [34,558] contracts)."
contract_spending_overall_active %>% 
  filter_to_it_consulting_services_and_software_licensing() %>% 
  a623_handbook_rule_duration()

# Key finding:
# "Amongst these ‘rule breaking’ contracts, the average contract length was 4.3 years, with a range of just over 3 years to 17.6 years in duration"
contract_spending_overall_active %>% 
  filter_to_it_consulting_services_and_software_licensing() %>% 
  a623_handbook_rule_duration(TRUE)

# Standish threshold with IT consulting services contracts

# 2017-2018 fiscal year
contract_spending_overall %>% 
  filter_to_active_during_fiscal_year(2017) %>%
  filter_to_it_consulting_services_and_software_licensing() %>% 
  a621_standish_rule_duration()

# 2021-2022 fiscal year
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

a711_references_open_source <- function(df, summarize_results = TRUE) {
  
  open_source_options <- "open source|open-source|opensource| oss "
  
  df <- df %>%
    mutate(
      is_open_source = case_when(
        str_detect(d_overall_description_comments_extended, open_source_options) ~ 1,
        TRUE ~ 0
      )
    )
  
  if(summarize_results == TRUE) {
    
    df %>%
      summarise(
        contracts = n(),
        contracts_that_reference_open_source = sum(is_open_source, na.rm = TRUE),
        percentage = contracts_that_reference_open_source / contracts
      ) %>%
      exports_round_percentages()
    
  }
  else {
    
    df %>%
      filter(is_open_source == 1)
    
  }
  
  
}

a712_references_standing_offer <- function(df, summarize_results = TRUE) {
  
  open_source_options <- "standing offer|standing-offer|supply-arrangement|tbips"
  
  df <- df %>%
    mutate(
      is_open_source = case_when(
        str_detect(d_overall_description_comments_extended, open_source_options) ~ 1,
        TRUE ~ 0
      )
    )
  
  if(summarize_results == TRUE) {
    
    df %>%
      summarise(
        contracts = n(),
        contracts_that_reference_standing_offer = sum(is_open_source, na.rm = TRUE),
        percentage = contracts_that_reference_standing_offer / contracts
      ) %>%
      exports_round_percentages()
    
  }
  else {
    
    df %>%
      filter(is_open_source == 1)
    
  }
  
  
}

# Key finding:
contract_spending_overall_ongoing %>%
  filter_to_it_consulting_services_and_software_licensing() %>% 
  a711_references_open_source()

contract_spending_overall_ongoing %>%
  filter_to_it_consulting_services_and_software_licensing() %>% 
  a712_references_standing_offer()

contract_spending_overall_active %>%
  filter_to_it_consulting_services() %>% 
  a712_references_standing_offer()


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
      contracts_vendor_owned_ip = n() - contracts_crown_owned_ip,
      crown_owned_percentage = contracts_crown_owned_ip / contracts,
      vendor_owned_percentage = 1 - crown_owned_percentage
    ) %>%
    exports_round_percentages()
  
}

# Key finding:
contract_spending_overall_active %>%
  filter_to_it_consulting_services_and_software_licensing() %>% 
  a712_crown_versus_contractor_owned_intellectual_property()


# Overall summary-based calculations ============

# Get percentage changes by fiscal year

# Note that the functions in exports.R are named
# get_summary_overall_by_category
# Be careful not to overwrite them!
retrieve_summary_overall_by_category <- function(summary_type = "all") {
  
  df <- summary_overall %>% 
    filter(summary_type == !!summary_type) %>% 
    select(summary_by_fiscal_year_by_category) %>% 
    unnest(cols = c(summary_by_fiscal_year_by_category)) %>%
    mutate(
      # Converts to double to avoid string type issues in future numeric operations
      total = as.double(total),
      total_constant_2019_dollars = as.double(total_constant_2019_dollars)
    )
  
  df
  
}

retrieve_summary_overall_by_it_subcategory <- function(summary_type = "all") {
  
  df <- summary_overall %>% 
    filter(summary_type == !!summary_type) %>% 
    select(summary_by_fiscal_year_by_it_subcategory) %>% 
    unnest(cols = c(summary_by_fiscal_year_by_it_subcategory)) %>%
    mutate(
      # Converts to double to avoid string type issues in future numeric operations
      total = as.double(total),
      total_constant_2019_dollars = as.double(total_constant_2019_dollars)
    )
  
  df
  
}

# This compares the first and last entries in "total_constant_2019_dollars" to each other
percent_changes_in_2019_dollars_by_fiscal_year_and_group_category <- function(df, grouping_column) {
  
  df <- df %>%
    group_by(across(all_of(grouping_column))) %>%
    arrange(!!!grouping_column, d_fiscal_year) %>%
    mutate(
      total_constant_2019_dollars = as.double(total_constant_2019_dollars),
      change_percentage = (last(total_constant_2019_dollars) - first(total_constant_2019_dollars)) / first(total_constant_2019_dollars)
    )
  
  df %>%
    select(!!!grouping_column, change_percentage) %>%
    distinct() %>%
    ungroup() %>%
    arrange(desc(change_percentage)) %>%
    exports_round_percentages()
  
}

# Retrieve an individual value from a combination of first-column search, and requested column pull
# Usage is e.g. df %>% pull_specific_first_column_value("it_consulting_services", "change_percentage")
# Thanks to
# https://stackoverflow.com/a/66822684/756641
pull_specific_first_column_value <- function(df, first_col_row_match, pull_column) {
  
  df %>%
    filter(if_any(1, ~ . == !!first_col_row_match)) %>%
    pull(!!pull_column)
  
}


# Key finding:
retrieve_summary_overall_by_category() %>%
  percent_changes_in_2019_dollars_by_fiscal_year_and_group_category("d_most_recent_category")

# Key finding:
# "spending on IT consulting services has grown by [54] percent, after correcting for inflation"
retrieve_summary_overall_by_it_subcategory() %>%
  percent_changes_in_2019_dollars_by_fiscal_year_and_group_category("d_most_recent_it_subcategory")

# "after correcting for inflation, from a total of [$1.17B] to [$1.79B] per year"
retrieve_summary_overall_by_it_subcategory()

# "The total spent on IT consulting services over the five year period under examination was [$7.7B]"
retrieve_summary_overall_by_it_subcategory() %>%
  filter(d_most_recent_it_subcategory == "it_consulting_services") %>%
  summarize(
    overall_total_constant_2019_dollars = sum(total_constant_2019_dollars)
  )

# "Over the five years of the analysis, Government of Canada departments spent [$7.7B] on IT consulting services, [$4.1B] on software licensing, [$3.6B] on devices and equipment, and [$3.9B] on Other, including telecommunications."
retrieve_summary_overall_by_it_subcategory() %>%
  group_by(d_most_recent_it_subcategory) %>%
  summarize(
    overall_total_constant_2019_dollars = sum(total_constant_2019_dollars)
  )

# Charts based on the overall summary trends ==============

filter_by_highest_2019_dollars_most_recent_fiscal_year <- function(df, limit_n = 5) {
  
  # Automatically uses the first column as the grouping category:
  grouping_category <- names(df)[[1]]
  
  largest_categories <- df %>%
    group_by(across(all_of(grouping_category))) %>%
    mutate(
      most_recent_2019_dollars = last(total_constant_2019_dollars, order_by = d_fiscal_year)
    ) %>% 
    select(!!!grouping_category, most_recent_2019_dollars) %>%
    distinct() %>%
    ungroup() %>%
    arrange(desc(most_recent_2019_dollars)) %>%
    slice_head(n = limit_n) %>%
    pull(grouping_category)
  
  # Thanks to
  # https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html#indirection
  df %>%
    filter(.data[[grouping_category]] %in% largest_categories)
  
}

plot_fiscal_year_2019_dollars <- function(df, custom_labels = labs(), num_legend_rows = 2) {
  

  # Automatically uses the first column as the grouping category:
  grouping_category <- names(df)[[1]]
  
  # Thanks to
  # https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html
  df <- df %>%
    mutate(
      year = convert_fiscal_year_to_start_year(d_fiscal_year),
      category = first(across({{ grouping_category}}, ~ as.factor(.)))
    ) %>%
    mutate(
      # Converts to double to avoid string type issues in future numeric operations
      # (In case this wasn't done previously.)
      total = as.double(total),
      total_constant_2019_dollars = as.double(total_constant_2019_dollars)
    )
  
  df <- df %>%
    group_by(category) %>%
    mutate(
      most_recent_total_constant_2019_dollars = last(total_constant_2019_dollars, order_by = d_fiscal_year)
    )
  
  ggplot(df, aes(
    x = year, 
    y = total_constant_2019_dollars, 
    color = reorder(category, desc(most_recent_total_constant_2019_dollars)), 
    shape = reorder(category, desc(most_recent_total_constant_2019_dollars))
    )) +
    geom_point(size = 2) +
    geom_line(size = 0.7) + 
    theme(
      axis.text = element_text(colour = "black"),
      aspect.ratio=3/4,
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.margin=margin()
      ) + 
    # Thanks to
    # https://stackoverflow.com/a/48252093/756641
    guides(
      color = guide_legend(nrow = num_legend_rows),
      shape = guide_legend(nrow = num_legend_rows)
    ) +
    # Thanks to
    # https://www.tidyverse.org/blog/2022/04/scales-1-2-0/#numbers
    scale_y_continuous(
      limits = c(0, NA),
      labels = label_dollar(scale_cut = cut_short_scale())
    ) +
    # 17 options for scale icons, hopefully distinct enough to avoid confusion
    # Thanks to
    # https://stackoverflow.com/a/41148368/756641
    # https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1003833#s9
    scale_shape_manual(values = c(16, 17, 15, 18, 3, 4, 8, 1, 2, 0, seq(5, 7), 9, 10, 12, 14)) +
    custom_labels +
    # Thanks to the "Hue and grey scales" section of the ggplot book.
    scale_fill_grey() +
    scale_color_grey()
  
  # df
  
}

update_category_names <- function(df) {
  
  df <- df %>%
    left_join(category_labels, by = c(d_most_recent_category = "original_category")) %>%
    select(! c(d_most_recent_category, leading_zero_category, category_path)) %>%
    rename(
      d_most_recent_category = "category_name"
    ) %>%
    relocate(d_most_recent_category)
  
  df
  
}

update_it_subcategory_names <- function(df) {
  
  df <- df %>%
    left_join(it_subcategory_labels, by = c(d_most_recent_it_subcategory = "original_it_subcategory")) %>%
    select(! c(d_most_recent_it_subcategory)) %>%
    rename(
      d_most_recent_it_subcategory = "it_subcategory_name"
    ) %>%
    relocate(d_most_recent_it_subcategory)
  
  df
  
}

ggsave_default_options <- function(filename, custom_height = 6.5) {
  ggsave(filename, dpi = "print", width = 6.5, height = custom_height, units = "in")
  
  # Also save a vector copy (note: .wmf format is Windows-only!)
  ggsave(str_replace(filename, ".png", ".wmf"), dpi = "print", width = 6.5, height = custom_height, units = "in")
  
  ggsave(str_replace(filename, ".png", ".svg"), device = "svg", dpi = "print", width = 6.5, height = custom_height, units = "in")
  
}

# retrieve_summary_overall_by_category() %>%
#   plot_fiscal_year_2019_dollars()

retrieve_summary_overall_by_category() %>%
  update_category_names() %>%
  # filter_by_highest_2019_dollars_most_recent_fiscal_year(6) %>%
  plot_fiscal_year_2019_dollars(labs(
    title = "Estimated government-wide contract spending \nby category",
    x = "Fiscal year",
    y = "Total estimated contract spending \n(constant 2019 dollars)",
    color = "Category",
    shape = "Category"
  ), 5)

ggsave_default_options("plots/p001_categories_by_fiscal_year.png", 6.5)

retrieve_summary_overall_by_it_subcategory() %>%
  update_it_subcategory_names() %>%
  plot_fiscal_year_2019_dollars(labs(
    title = "Estimated government-wide contract spending \nby IT subcategory",
    x = "Fiscal year",
    y = "Total estimated contract spending \n(constant 2019 dollars)",
    color = "IT subcategory",
    shape = "IT subcategory"
  ))

ggsave_default_options("plots/p002_it_subcategories_by_fiscal_year.png", 5.8)


# Charts focusing on specific vendors ===========

retrieve_summary_vendors_by_it_subcategories <- function(requested_vendors_list = c()) {
  
  if(is_empty(requested_vendors_list)) {
    
    requested_vendors_list <- summary_vendors %>%
      select(vendor, summary_by_fiscal_year_by_it_subcategory) %>%
      unnest(cols = c(summary_by_fiscal_year_by_it_subcategory)) %>%
      filter_to_it_consulting_services %>%
      mutate(
        total = as.double(total),
        total_constant_2019_dollars = as.double(total_constant_2019_dollars)
      ) %>%
      group_by(vendor) %>%
      mutate(
        overall_total_constant_2019_dollars = sum(total_constant_2019_dollars)
      ) %>%
      ungroup() %>%
      select(vendor, overall_total_constant_2019_dollars) %>%
      distinct() %>%
      arrange(desc(overall_total_constant_2019_dollars)) %>%
      slice_head(n = 10) %>%
      pull(vendor)
    
  }

  
  df <- summary_vendors %>%
    filter(vendor %in% requested_vendors_list) %>%
    select(vendor, summary_by_fiscal_year_by_it_subcategory) %>%
    unnest(cols = c(summary_by_fiscal_year_by_it_subcategory)) 
  
  
  df
  
}

requested_vendors_list <- c(
  "VERITAAQ TECHNOLOGY HOUSE",
  "IBM CANADA",
  "SI SYSTEMS",
  "RANDSTAD",
  "MODIS CANADA",
  "DELOITTE",
  "ACCENTURE",
  "PRICEWATERHOUSE COOPERS",
  "KPMG",
  "ERNST YOUNG"
)

retrieve_summary_vendors_by_it_subcategories(requested_vendors_list) %>% 
  filter_to_it_consulting_services() %>%
  select(! d_most_recent_it_subcategory) %>%
  plot_fiscal_year_2019_dollars(labs(
    title = "Estimated IT consulting services contract spending \nby vendor (specific vendor subset)",
    x = "Fiscal year",
    y = "Total estimated IT consulting services \ncontract spending (constant 2019 dollars)",
    color = "Vendor",
    shape = "Vendor"
  ), 5)

ggsave_default_options("plots/p003_it_consulting_services_key_vendors_by_fiscal_year.png")

# Same as above but with the top 10 overall IT consulting firms, rather than a preset list:
retrieve_summary_vendors_by_it_subcategories() %>% 
  filter_to_it_consulting_services() %>%
  select(! d_most_recent_it_subcategory) %>%
  plot_fiscal_year_2019_dollars(labs(
    title = "Estimated IT consulting services contract spending \nby vendor (top 10 vendors by dollar value)",
    x = "Fiscal year",
    y = "Total estimated IT consulting services \ncontract spending (constant 2019 dollars)",
    color = "Vendor",
    shape = "Vendor"
  ), 5)

ggsave_default_options("plots/p006_it_consulting_services_top_10_overall_vendors_by_fiscal_year.png")

# Total across that time span
# "In total, the federal government spent [$2.79B] on IT consulting services contracts with these firms over the period of analysis"
retrieve_summary_vendors_by_it_subcategories() %>% 
  filter_to_it_consulting_services() %>%
  mutate(
    total = as.double(total),
    total_constant_2019_dollars = as.double(total_constant_2019_dollars)
  ) %>%
  mutate(
    overall_total_constant_2019_dollars = sum(total_constant_2019_dollars, na.rm = TRUE)
  ) %>%
  select(overall_total_constant_2019_dollars) %>%
  distinct() %>%
  pull(overall_total_constant_2019_dollars)


# Major management consulting firms
# Same approach as above but not limited to IT

retrieve_summary_vendors_overall <- function(requested_vendors_list = c()) {
  
  if(is_empty(requested_vendors_list)) {
    
    requested_vendors_list <- summary_vendors %>%
      select(vendor, summary_by_fiscal_year) %>%
      unnest(cols = c(summary_by_fiscal_year)) %>%
      filter_to_it_consulting_services %>%
      mutate(
        total = as.double(total),
        total_constant_2019_dollars = as.double(total_constant_2019_dollars)
      ) %>%
      group_by(vendor) %>%
      mutate(
        overall_total_constant_2019_dollars = sum(total_constant_2019_dollars)
      ) %>%
      ungroup() %>%
      select(vendor, overall_total_constant_2019_dollars) %>%
      distinct() %>%
      arrange(desc(overall_total_constant_2019_dollars)) %>%
      slice_head(n = 10) %>%
      pull(vendor)
    
  }
  
  
  df <- summary_vendors %>%
    filter(vendor %in% requested_vendors_list) %>%
    select(vendor, summary_by_fiscal_year) %>%
    unnest(cols = c(summary_by_fiscal_year)) 
  
  
  df
  
}

requested_management_consultants_list <- c(
  "DELOITTE",
  "ACCENTURE",
  "PRICEWATERHOUSE COOPERS",
  "KPMG",
  "ERNST YOUNG",
  "MCKINSEY AND COMPANY"
)

retrieve_summary_vendors_overall(requested_management_consultants_list) %>% 
  write_csv(str_c("data/testing/tmp-", today(), "-major-management-consultants-overall.csv"))



# Top 10 IT vendors in 2021-2022 by IT subcategory

ggsave_stacked_bar_chart_options <- function(filename, custom_height = 6.5) {
  
  ggsave(filename, dpi = "print", width = 6.5, height = custom_height, units = "in")
  
  # Also save a vector copy (note: .wmf format is Windows-only!)
  ggsave(str_replace(filename, ".png", ".wmf"), dpi = "print", width = 6.5, height = custom_height, units = "in")
  
  ggsave(str_replace(filename, ".png", ".svg"), device = "svg", dpi = "print", width = 6.5, height = custom_height, units = "in")
  
}

# Returns the set of top 10 vendors in the most recent fiscal year, broken down by d_most_recent_it_subcategory
retrieve_overall_top_10_it_vendors_most_recent_fiscal_year_by_it_subcategory <- function() {
  
  all_it_vendors <- summary_categories %>%
    filter(category == "3_information_technology") %>%
    select(summary_by_fiscal_year_by_vendor) %>%
    unnest(cols = c(summary_by_fiscal_year_by_vendor)) %>%
    mutate(
      total = as.double(total),
      total_constant_2019_dollars = as.double(total_constant_2019_dollars)
    )
  
  most_recent_fiscal_year <- all_it_vendors %>%
    select(d_fiscal_year) %>%
    distinct() %>%
    arrange(desc(d_fiscal_year)) %>%
    pull(d_fiscal_year) %>%
    first()
  
  top_10_it_vendors <- all_it_vendors %>%
    filter(d_fiscal_year == !!most_recent_fiscal_year) %>%
    arrange(desc(total)) %>%
    slice_head(n = 10) %>%
    pull(d_vendor_name)
  
  # Retrieve the IT subcategory breakdown from summary_vendors
  df <- summary_vendors %>%
    filter(vendor %in% top_10_it_vendors) %>%
    select(vendor, summary_by_fiscal_year_by_it_subcategory) %>%
    unnest(cols = c(summary_by_fiscal_year_by_it_subcategory)) %>%
    mutate(
      total = as.double(total),
      total_constant_2019_dollars = as.double(total_constant_2019_dollars)
    )
  
  df <- df %>%
    filter(d_fiscal_year == !!most_recent_fiscal_year)
  
  df
  
}

# Returns the set of top 10 vendors across all 5 years of summary_categories$summary_by_fiscal_year_by_vendor, broken down by d_most_recent_it_subcategory
retrieve_overall_top_10_it_vendors_by_it_subcategory <- function() {
  
  all_it_vendors <- summary_categories %>%
    filter(category == "3_information_technology") %>%
    select(summary_by_fiscal_year_by_vendor) %>%
    unnest(cols = c(summary_by_fiscal_year_by_vendor)) %>%
    mutate(
      total = as.double(total),
      total_constant_2019_dollars = as.double(total_constant_2019_dollars)
    )
  
  all_it_vendors <- all_it_vendors %>%
    group_by(d_vendor_name) %>%
    mutate(
      overall_total_constant_2019_dollars = sum(total_constant_2019_dollars, na.rm = TRUE)
    ) %>%
    ungroup()
  
  top_10_it_vendors <- all_it_vendors %>%
    select(d_vendor_name, overall_total_constant_2019_dollars) %>%
    distinct() %>%
    arrange(desc(overall_total_constant_2019_dollars)) %>%
    slice_head(n = 10) %>%
    pull(d_vendor_name)
  
  # Retrieve the IT subcategory breakdown from summary_vendors
  df <- summary_vendors %>%
    filter(vendor %in% top_10_it_vendors) %>%
    select(vendor, summary_by_fiscal_year_by_it_subcategory) %>%
    unnest(cols = c(summary_by_fiscal_year_by_it_subcategory)) %>%
    mutate(
      total = as.double(total),
      total_constant_2019_dollars = as.double(total_constant_2019_dollars)
    )
  
  df <- df %>%
    group_by(vendor, d_most_recent_it_subcategory) %>%
    mutate(
      overall_total_constant_2019_dollars = sum(total_constant_2019_dollars, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    select(vendor, d_most_recent_it_subcategory, overall_total_constant_2019_dollars) %>%
    distinct()

  # To more easily reuse the plot function
  df <- df %>%
    rename(
      total = "overall_total_constant_2019_dollars"
    )
  
  df
  
}

retrieve_overall_top_10_it_vendors_by_it_subcategory_versus_total_it_spending <- function() {
  
  # Reuse the function above
  # This returns a table of vendors plus overall_total (in constant 2019 dollars, due to the function above)
  top_it_vendors <- retrieve_overall_top_10_it_vendors_by_it_subcategory() %>%
    group_by(vendor) %>%
    mutate(
      overall_total_constant_2019_dollars = sum(total, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    select(vendor, overall_total_constant_2019_dollars) %>%
    distinct()
  
  all_it_spending_overall_constant_2019_dollars <- summary_categories %>%
    filter(category == "3_information_technology") %>%
    select(summary_by_fiscal_year) %>%
    unnest(cols = c(summary_by_fiscal_year)) %>%
    mutate(
      total = as.double(total),
      total_constant_2019_dollars = as.double(total_constant_2019_dollars)
    ) %>%
    mutate(
      overall_total_constant_2019_dollars = sum(total_constant_2019_dollars)
    ) %>% 
    select(overall_total_constant_2019_dollars) %>%
    distinct() %>%
    pull(overall_total_constant_2019_dollars)
  
  top_it_vendors <- top_it_vendors %>%
    mutate(
      all_it_spending_overall_constant_2019_dollars = !!all_it_spending_overall_constant_2019_dollars
    ) %>%
    mutate(
      percentage = overall_total_constant_2019_dollars / all_it_spending_overall_constant_2019_dollars
    )
  
  # Return the cumulative percentage
  top_it_vendors %>%
    mutate(
      overall_percentage = sum(percentage)
    ) %>%
    select(overall_percentage) %>%
    distinct() %>%
    pull(overall_percentage)
  
}

retrieve_overall_top_10_it_consulting_services_vendors_versus_total_it_consulting_services_spending <- function() {
  
  # Updated function - get the top 10 specifically for IT consulting services
  # rather than a subset of the overall IT category top 10.
  
  top_it_vendors <- retrieve_summary_vendors_by_it_subcategories() %>% 
    filter_to_it_consulting_services() %>%
    select(! d_most_recent_it_subcategory) %>% 
    group_by(vendor) %>%
    mutate(
      total_constant_2019_dollars = as.double(total_constant_2019_dollars),
      overall_total_constant_2019_dollars = sum(total_constant_2019_dollars, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    arrange(desc(overall_total_constant_2019_dollars)) %>% 
    select(vendor, overall_total_constant_2019_dollars) %>% 
    distinct()
  
  all_it_consulting_services_spending_overall_constant_2019_dollars <- summary_it_subcategories %>%
    filter(it_subcategory == "it_consulting_services") %>%
    select(summary_by_fiscal_year) %>%
    unnest(cols = c(summary_by_fiscal_year)) %>%
    mutate(
      total = as.double(total),
      total_constant_2019_dollars = as.double(total_constant_2019_dollars)
    ) %>%
    mutate(
      overall_total_constant_2019_dollars = sum(total_constant_2019_dollars)
    ) %>% 
    select(overall_total_constant_2019_dollars) %>%
    distinct() %>%
    pull(overall_total_constant_2019_dollars)
  
  top_it_vendors <- top_it_vendors %>%
    mutate(
      all_it_consulting_services_spending_overall_constant_2019_dollars = !!all_it_consulting_services_spending_overall_constant_2019_dollars
    ) %>%
    mutate(
      percentage = overall_total_constant_2019_dollars / all_it_consulting_services_spending_overall_constant_2019_dollars
    )
  
  # Export a CSV of these 10 firms' IT consulting services totals for any additional analysis
  top_it_vendors %>% 
    write_csv(str_c("data/testing/tmp-", today(), "-top-10-it-consulting-firms-as-percentage-of-total.csv"))
  
  # Return the cumulative percentage
  top_it_vendors %>%
    mutate(
      overall_percentage = sum(percentage)
    ) %>%
    select(overall_percentage) %>%
    distinct() %>%
    pull(overall_percentage)
  
}

# Key finding
# "IT consulting services contracts with these 10 firms... amounting to 37% of total spending in this subcategory."
retrieve_overall_top_10_it_consulting_services_vendors_versus_total_it_consulting_services_spending()


plot_it_subcategory_breakdown <- function(df, custom_labels = labs(), num_legend_rows = 2) {
  
  # ggplot(df, aes(x = year, y = total_constant_2019_dollars, color = category, shape = category)) +
  #   geom_point() +
  #   geom_line() + 
  #   theme(aspect.ratio=1/1) + 
  #   # Thanks to
  #   # https://www.tidyverse.org/blog/2022/04/scales-1-2-0/#numbers
  #   scale_y_continuous(
  #     limits = c(0, NA),
  #     labels = label_dollar(scale_cut = cut_short_scale())
  #   )
  
  df <- df %>%
    group_by(vendor) %>%
    mutate(
      overall_total = sum(total)
    )
  
  ggplot(df) +
    geom_col(aes(
      x = reorder(vendor, overall_total),
      y = total,
      fill = d_most_recent_it_subcategory,
    )) +
    # theme(aspect.ratio=3/1) + 
    theme(
      # Thanks to
      # https://stackoverflow.com/a/42556457/756641
      # plot.title = element_text(hjust = 0.0),
      # Thanks to
      # https://stackoverflow.com/a/14942760/756641
      axis.text = element_text(size = rel(0.55), colour = "black"),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.margin=margin()
    ) + 
    guides(
      fill = guide_legend(nrow = num_legend_rows)
    ) + 
    scale_y_continuous(
      limits = c(0, NA),
      labels = label_dollar(scale_cut = cut_short_scale())
    ) +
    coord_flip() +
    custom_labels +
    # Thanks to the "Hue and grey scales" section of the ggplot book.
    scale_fill_grey() +
    scale_color_grey()

}


# Currently using p005 below instead of p004 here
retrieve_overall_top_10_it_vendors_most_recent_fiscal_year_by_it_subcategory() %>%
  update_it_subcategory_names() %>%
  plot_it_subcategory_breakdown(labs(
    title = "Top 10 IT vendors by estimated contract value \nand IT subcategory",
    x = NULL,
    y = "Total estimated contract value (2021-2022)",
    fill = "IT subcategory"
  ), 2)

ggsave_stacked_bar_chart_options("plots/p004_top_vendors_by_it_subcategories_most_recent_fiscal_year.png")

# Key finding:
retrieve_overall_top_10_it_vendors_by_it_subcategory() %>%
  update_it_subcategory_names() %>%
  plot_it_subcategory_breakdown(labs(
    title = "Top 10 IT vendors by estimated contract value \nand IT subcategory (2017-2018 to 2021-2022)",
    x = NULL,
    y = "Total estimated contract value (constant 2019 dollars)",
    fill = "IT subcategory"
  ), 2)

ggsave_stacked_bar_chart_options("plots/p005_top_vendors_by_it_subcategories_overall.png")


# Key finding:
# "the ten largest IT vendors measured by contract dollar value represent [36%] of the total estimated spending on IT contracts over 2017 to 2022"
retrieve_overall_top_10_it_vendors_by_it_subcategory_versus_total_it_spending()

# Tables for the Findings section =========================

retrieve_vendors_by_scale_segment <- function(use_most_recent_fiscal_year = FALSE) {
  
  # Adapted from get_summary_overall_by_fiscal_year_by_criteria
  # to include all vendors in the breakdown below.
  # Note: this is a bit more performance intensive than re-using the summary_categories function! 
  
  # all_it_vendors <- summary_categories %>%
  #   filter(category == "3_information_technology") %>%
  #   select(summary_by_fiscal_year_by_vendor) %>%
  #   unnest(cols = c(summary_by_fiscal_year_by_vendor)) %>%
  #   mutate(
  #     total = as.double(total),
  #     total_constant_2019_dollars = as.double(total_constant_2019_dollars)
  #   )
  
  all_it_vendors <- contract_spending_by_date %>%
    filter(d_most_recent_category == "3_information_technology") %>%
    group_by(d_vendor_name, d_fiscal_year_short) %>%
    summarize_fiscal_year_totals() %>%
    ungroup() %>%
    # mutate(
    #   d_fiscal_year = convert_start_year_to_fiscal_year(d_fiscal_year_short)
    # ) %>%
    select(d_vendor_name, d_fiscal_year_short, total, total_constant_2019_dollars) #%>%
    # exports_round_totals() %>%
    # mutate(
    #   total = as.double(total),
    #   total_constant_2019_dollars = as.double(total_constant_2019_dollars)
    # )
  
  if(use_most_recent_fiscal_year == TRUE) {
    
    most_recent_fiscal_year <- all_it_vendors %>%
      select(d_fiscal_year_short) %>%
      distinct() %>%
      arrange(desc(d_fiscal_year_short)) %>%
      pull(d_fiscal_year_short) %>%
      first() 
    
    all_it_vendors <- all_it_vendors %>%
      filter(d_fiscal_year_short == !!most_recent_fiscal_year) %>%
      arrange(desc(total))
    
  }
  else {
    
    all_it_vendors <- all_it_vendors %>%
      group_by(d_vendor_name) %>%
      mutate(
        # Note: confirm  if this should use constant 2019 dollars (we're summing together totals from multiple years)
        overall_total_constant_2019_dollars = sum(total_constant_2019_dollars, na.rm = TRUE),
        active_years = last(d_fiscal_year_short) - first(d_fiscal_year_short) + 1
      ) %>%
      ungroup() %>%
      select(d_vendor_name, overall_total_constant_2019_dollars, active_years) %>%
      distinct() %>%
      mutate(
        # This should be "overall_total_constant_2019_dollars per active year" but we're using "total" to reuse the code below / compatibility with the one-fiscal-year approach above.
        total = overall_total_constant_2019_dollars / active_years
      )
    
    
  }

  
  all_it_vendors <- all_it_vendors %>%
    mutate(
      scale_segment = case_when(
        total > 100000000 ~ "1_over_100m",
        total > 50000000 ~ "2_over_50m",
        total > 10000000 ~ "3_over_10m",
        TRUE ~ "4_below_10m"
      )
    )
  
  if(use_most_recent_fiscal_year == TRUE) {
    
    all_it_vendors <- all_it_vendors %>%
      group_by(scale_segment) %>%
      summarize(
        vendors_count = n(),
        total_value = sum(total, na.rm = TRUE)
      )
  }
  else {
    
    all_it_vendors <- all_it_vendors %>%
      group_by(scale_segment) %>%
      summarize(
        vendors_count = n(),
        total_value = sum(overall_total_constant_2019_dollars, na.rm = TRUE)
      )
    
  }
  
  all_it_vendors %>%
    mutate(
      overall_it_spending_value = sum(total_value, na.rm = TRUE),
      overall_it_spending_percentage = total_value / overall_it_spending_value
    ) %>%
    exports_round_totals() %>%
    exports_round_percentages()
  
}

# Key finding: market dynamics and scale segmentation
retrieve_vendors_by_scale_segment() %>%
  write_csv(str_c("data/testing/tmp-", today(), "-table-market-dynamics-scale-segmentation.csv"))


retrieve_duration_segments_by_it_subcategory <- function() {
  
  contract_spending_by_duration <- contract_spending_overall_ongoing %>%
    filter_to_information_technology() %>%
    add_total_number_of_days() %>%
    mutate(
      duration_segment = case_when(
        duration_years > 10 ~ "1_over_10_years", # Over 10 years
        duration_years > 5 ~ "2_over_5_years", # Less than 10 years
        duration_years > 3 ~ "3_over_3_years", # Less than 5 years
        duration_years > 1 ~ "4_over_1_year", # Less than 3 years
        TRUE ~ "5_no_more_than_1_year" # Less than 1 year
      )
    )
  
  duration_segments_by_it_subcategory <- contract_spending_by_duration %>%
    group_by(d_most_recent_it_subcategory, duration_segment) %>%
    summarize(
      contracts_count = n(),
      total_value = sum(d_overall_contract_value, na.rm = TRUE)
    ) %>%
    exports_fancy_totals()
  
  duration_segments_by_it_subcategory %>%
    pivot_wider(
      names_from = duration_segment,
      names_glue = "{duration_segment}_{.value}",
      values_from = c(contracts_count, total_value)
    ) %>%
    relocate(
      d_most_recent_it_subcategory,
      starts_with("5_"),
      starts_with("4_"),
      starts_with("3_"),
      starts_with("2_"),
      starts_with("1_"),
    )
  
}

retrieve_duration_segments_by_it_subcategory() %>%
  write_csv(str_c("data/testing/tmp-", today(), "-table-contract-duration-segmentation.csv"))


retrieve_it_consulting_staff_count_estimate_v1 <- function(fiscal_year = 2021, per_diem_low_end = 800, per_diem_high_end = 1400, output_overall_contractor_staff_counts = FALSE, output_by_spending_totals = FALSE) {
  
  # Thanks to
  # https://www.workingdays.ca/Federal%20Holidays.htm
  working_days_per_calendar_days = 249 / 365
  
  in_house_it_staff_by_department <- read_csv("data/owner_orgs/it_staff_by_department.csv") %>%
    clean_names() %>%
    # Note: currently this CSV includes 2020-2021 and 2021-2022 fiscal years (in "short" fiscal year form using the start year).
    filter(fiscal_year == !!fiscal_year)
  
  
  contract_spending_target_fiscal_year <- contract_spending_overall_ongoing %>% 
    filter_to_active_during_fiscal_year(fiscal_year) %>% 
    filter_to_it_consulting_services() %>%
    add_total_number_of_days() %>%
    mutate(
      d_fiscal_year_start_date =  ymd(str_c(fiscal_year,"04","01")),
      d_fiscal_year_end_date = ymd(str_c(fiscal_year + 1,"03","31"))
    ) %>%
    mutate(
      d_within_fiscal_year_start_date = case_when(
        d_overall_start_date < d_fiscal_year_start_date ~ d_fiscal_year_start_date,
        TRUE ~ d_overall_start_date,
      ),
      d_within_fiscal_year_end_date = case_when(
        d_overall_end_date > d_fiscal_year_end_date ~ d_fiscal_year_end_date,
        TRUE ~ d_overall_end_date,
      ),
    ) %>%
    mutate(
      d_within_fiscal_year_duration_days = as.integer(d_within_fiscal_year_end_date - d_within_fiscal_year_start_date + 1),
      d_within_fiscal_year_contract_value = d_daily_contract_value * d_within_fiscal_year_duration_days,
      d_within_fiscal_year_duration_working_days = d_within_fiscal_year_duration_days * !!working_days_per_calendar_days,
      d_value_per_working_day = d_within_fiscal_year_contract_value / d_within_fiscal_year_duration_working_days
    )
  
  contract_spending_target_fiscal_year <- contract_spending_target_fiscal_year %>%
    filter(
      # Avoid division by 0 errors
      d_within_fiscal_year_duration_working_days >= 1,
      # Skip contracts that are very, very low (likely miscategorizations or contracts that barely extend into the current fiscal year)
      d_value_per_working_day > !!per_diem_low_end / 2
    ) %>%
    mutate(
      # Note that these names are flipped (a low per diem estimate = a high contractor count estimate, and vice versa)
      contractor_staff_high_end_count = round(d_value_per_working_day / !!per_diem_low_end),
      contractor_staff_low_end_count = round(d_value_per_working_day / !!per_diem_high_end),
      
    )
  
  # Get a government-wide total for methodology details
  if(output_overall_contractor_staff_counts == TRUE) {
    
    contractor_staff_estimates <- contract_spending_target_fiscal_year %>%
      summarize(
        sum_consulting_services_value = sum(d_within_fiscal_year_contract_value, na.rm = TRUE),
        sum_contractor_staff_low_end_count = sum(contractor_staff_low_end_count, na.rm = TRUE),
        sum_contractor_staff_high_end_count = sum(contractor_staff_high_end_count, na.rm = TRUE),
      )
    
    return(contractor_staff_estimates)
    
  }
  
  # in_house_it_staff_by_department
  contractor_staff_estimates <- contract_spending_target_fiscal_year %>%
    group_by(owner_org) %>%
    summarize(
      sum_consulting_services_value = sum(d_within_fiscal_year_contract_value, na.rm = TRUE),
      sum_contractor_staff_low_end_count = sum(contractor_staff_low_end_count, na.rm = TRUE),
      sum_contractor_staff_high_end_count = sum(contractor_staff_high_end_count, na.rm = TRUE),
    )
  
  if(output_by_spending_totals == TRUE) {
    
    output <- contractor_staff_estimates %>%
      left_join(in_house_it_staff_by_department, by = c(owner_org = "department")) %>%
      arrange(desc(sum_consulting_services_value))
    
  }
  else {
    
    output <- in_house_it_staff_by_department %>%
      left_join(contractor_staff_estimates, by = c(department = "owner_org")) %>%
      rename(
        owner_org = "department"
      )
    
    
  }
  
  output %>%
    mutate(
      sum_contractor_staff_low_end_percentage = sum_contractor_staff_low_end_count / it_staff_count,
      sum_contractor_staff_high_end_percentage = sum_contractor_staff_high_end_count / it_staff_count
    ) %>%
    left_join(owner_org_names, by = "owner_org") %>%
    select(! c(fiscal_year, owner_org_name_fr)) %>%
    relocate(owner_org, owner_org_name_en) %>%
    exports_round_percentages() %>%
    exports_round_totals() %>%
    slice_head(n = 10)
    
    
  
}

# Key finding (v2 is used in the analysis paper table) 
retrieve_it_consulting_staff_count_estimate_v2 <- function(fiscal_year = 2021, per_diem_low_end = 800, per_diem_high_end = 1400, output_overall_contractor_staff_counts = FALSE, output_by_spending_totals = FALSE) {
  
  # Thanks to
  # https://www.workingdays.ca/Federal%20Holidays.htm
  working_days = 249
  working_days_per_calendar_days = working_days / 365
  
  in_house_it_staff_by_department <- read_csv("data/owner_orgs/it_staff_by_department.csv") %>%
    clean_names() %>%
    # Note: currently this CSV includes 2020-2021 and 2021-2022 fiscal years (in "short" fiscal year form using the start year).
    filter(fiscal_year == !!fiscal_year)
  
  contract_spending_target_fiscal_year <- contract_spending_by_date %>%
    filter(
      date >=  ymd(str_c(fiscal_year,"04","01")),
      date <= ymd(str_c(fiscal_year + 1,"03","31"))
    ) %>%
    filter_to_it_consulting_services()
  
  contract_spending_target_fiscal_year <- contract_spending_target_fiscal_year %>% 
    group_by(owner_org) %>%
    mutate(
      overall_value = sum(d_daily_contract_value, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    select(owner_org, overall_value) %>%
    distinct()
  
  contract_spending_target_fiscal_year <- contract_spending_target_fiscal_year %>%
    mutate(
      d_value_per_working_day = overall_value / !!working_days
    ) %>%
    mutate(
      # Note that these names are flipped (a low per diem estimate = a high contractor count estimate, and vice versa)
      contractor_staff_high_end_count = round(d_value_per_working_day / !!per_diem_low_end),
      contractor_staff_low_end_count = round(d_value_per_working_day / !!per_diem_high_end),
    ) %>%
    arrange(desc(overall_value))
  
  
  # Get a government-wide total for methodology details
  if(output_overall_contractor_staff_counts == TRUE) {
    
    contractor_staff_estimates <- contract_spending_target_fiscal_year %>%
      summarize(
        sum_contractor_staff_low_end_count = sum(contractor_staff_low_end_count, na.rm = TRUE),
        sum_contractor_staff_high_end_count = sum(contractor_staff_high_end_count, na.rm = TRUE),
      )
    
    return(contractor_staff_estimates)
    
  }
  
  
  if(output_by_spending_totals == TRUE) {
    
    output <- contract_spending_target_fiscal_year %>%
      left_join(in_house_it_staff_by_department, by = c(owner_org = "department")) %>%
      arrange(desc(d_value_per_working_day))
    
  }
  else {
    
    output <- in_house_it_staff_by_department %>%
      left_join(contract_spending_target_fiscal_year, by = c(department = "owner_org")) %>%
      rename(
        owner_org = "department"
      )
    
    
  }
  
  output %>%
    mutate(
      sum_contractor_staff_low_end_percentage = contractor_staff_low_end_count / it_staff_count,
      sum_contractor_staff_high_end_percentage = contractor_staff_high_end_count / it_staff_count
    ) %>%
    rename(
      # For consistency with v1
      sum_contractor_staff_low_end_count = "contractor_staff_low_end_count",
      sum_contractor_staff_high_end_count = "contractor_staff_high_end_count"
    ) %>%
    left_join(owner_org_names, by = "owner_org") %>%
    select(! c(fiscal_year, owner_org_name_fr)) %>%
    relocate(owner_org, owner_org_name_en) %>%
    exports_round_percentages() %>%
    exports_round_totals() %>%
    arrange(desc(it_staff_count)) %>%
    slice_head(n = 10)
  
}

retrieve_it_consulting_staff_count_estimate_v3 <- function(fiscal_year = 2021, per_diem_low_end = 800, per_diem_high_end = 1400) {
  
  # Estimated total # of IT contractors, approx. 60,000
  # based on https://itac.ca/wp-content/uploads/2019/05/ITAC-Commercial-first-doc-mar2019.pdf
  # 80k minus full-time public service IT staff of 15000 to 20000
  # Excluding DND (approx. 6% according to PIPSC research)
  estimated_contractor_staff_low_end_total = round( (80000 - 25000) * 0.94)
  estimated_contractor_staff_high_end_total = round( (80000 - 15000) * 0.94)
  
  in_house_it_staff_by_department <- read_csv("data/owner_orgs/it_staff_by_department.csv") %>%
    clean_names() %>%
    # Note: currently this CSV includes 2020-2021 and 2021-2022 fiscal years (in "short" fiscal year form using the start year).
    filter(fiscal_year == !!fiscal_year)
  
  contract_spending_target_fiscal_year <- contract_spending_by_date %>%
    filter(
      date >=  ymd(str_c(fiscal_year,"04","01")),
      date <= ymd(str_c(fiscal_year + 1,"03","31"))
    ) %>%
    filter_to_it_consulting_services()
  
  contract_spending_target_fiscal_year <- contract_spending_target_fiscal_year %>% 
    group_by(owner_org) %>%
    mutate(
      overall_value = sum(d_daily_contract_value, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    select(owner_org, overall_value) %>%
    distinct()
  
  contract_spending_target_fiscal_year <- contract_spending_target_fiscal_year %>% 
    mutate(
      total_overall_value = sum(overall_value, na.rm = TRUE),
      total_overall_estimated_contractor_staff_low_end = !!estimated_contractor_staff_low_end_total,
      total_overall_estimated_contractor_staff_high_end = !!estimated_contractor_staff_high_end_total,
    ) %>%
    mutate(
      overall_value_percentage = overall_value / total_overall_value,
      sum_contractor_staff_low_end_count = round(total_overall_estimated_contractor_staff_low_end * overall_value_percentage),
      sum_contractor_staff_high_end_count = round(total_overall_estimated_contractor_staff_high_end * overall_value_percentage)
    )
  
  
  in_house_it_staff_by_department %>%
    left_join(contract_spending_target_fiscal_year, by = c(department = "owner_org")) %>%
    mutate(
      sum_contractor_staff_low_end_percentage = sum_contractor_staff_low_end_count / it_staff_count,
      sum_contractor_staff_high_end_percentage = sum_contractor_staff_high_end_count / it_staff_count
    ) %>%
    rename(
      owner_org = "department"
    ) %>%
    left_join(owner_org_names, by = "owner_org") %>%
    select(! c(fiscal_year, owner_org_name_fr, starts_with("total_"))) %>%
    relocate(owner_org, owner_org_name_en) %>%
    exports_round_percentages() %>%
    exports_round_totals() %>%
    slice_head(n = 10)
  
}

helper_columns_it_consulting_staff <- function(df) {
  
  df <- df %>%
    mutate(
      sum_contractor_staff_low_end_percentage_formatted = str_c(round(sum_contractor_staff_low_end_percentage * 100), "%"),
      sum_contractor_staff_high_end_percentage_formatted = str_c(round(sum_contractor_staff_high_end_percentage * 100), "%"),
    ) %>%
    unite(
      col = "contractor_staff_range",
      c(sum_contractor_staff_low_end_count, sum_contractor_staff_high_end_count),
      sep = " to ",
      remove = FALSE
    ) %>%
    unite(
      col = "percentage_range",
      c(sum_contractor_staff_low_end_percentage_formatted, sum_contractor_staff_high_end_percentage_formatted),
      sep = " to "
    )
  
}

# Original estimate approach, identifying an estimated (high and low) count of contractors per IT consulting services contract (by converting each contract's value to a count using per diem estimates), then tallying them up by department:
retrieve_it_consulting_staff_count_estimate_v1() %>% 
  helper_columns_it_consulting_staff %>%
  write_csv(str_c("data/testing/tmp-", today(), "-consulting-staff-count-estimate-v1.csv"))

# Revised estimate approach, using the sum of a department's IT consulting services contract value (for 2021-2022), dividing it by the number of business days, and dividing it by (high and low) per diem estimates
# Results in slightly smaller (~20% lower) counts than v1
# Key finding (currently used in the analysis paper)
retrieve_it_consulting_staff_count_estimate_v2() %>% 
  helper_columns_it_consulting_staff %>%
  write_csv(str_c("data/testing/tmp-", today(), "-consulting-staff-count-estimate-v2.csv"))

# Third alternate approach, takes a published estimate of the number of contractors and weights it to departments based on their overall IT consulting services spending.
# Results in substantially higher counts, to the point that it's unrealistic that the total dollars spent on IT consulting services could match these counts (e.g. per diems would be much lower than expected).
retrieve_it_consulting_staff_count_estimate_v3() %>% 
  helper_columns_it_consulting_staff %>%
  write_csv(str_c("data/testing/tmp-", today(), "-consulting-staff-count-estimate-v3.csv"))

# Get the government-wide estimated IT contractor staff counts for methodology details
retrieve_it_consulting_staff_count_estimate_v1(output_overall_contractor_staff_counts = TRUE)

retrieve_it_consulting_staff_count_estimate_v2(output_overall_contractor_staff_counts = TRUE)

# Sort by total spending instead of in-house IT staff
retrieve_it_consulting_staff_count_estimate_v1(output_by_spending_totals = TRUE) %>% 
  helper_columns_it_consulting_staff %>%
  write_csv(str_c("data/testing/tmp-", today(), "-consulting-staff-count-estimate-v1-spend-sort.csv"))

retrieve_it_consulting_staff_count_estimate_v2(output_by_spending_totals = TRUE) %>% 
  helper_columns_it_consulting_staff %>%
  write_csv(str_c("data/testing/tmp-", today(), "-consulting-staff-count-estimate-v2-spend-sort.csv"))


# Miscellaeous extra findings =============================

# normalization table includes [6283] entries
vendor_matching %>%
  count()

# largest variation in names, by vendor
vendor_matching %>%
  count(parent_company, sort = TRUE)

# normalization table includes approximately 6,000 entries (across [823] vendors)
vendor_matching %>%
  select(parent_company) %>%
  distinct() %>%
  count()

# the vendor normalization process consolidated [167937] unique vendor names (in the source data)
contracts_individual_entries %>%
  select(vendor_name) %>%
  distinct() %>%
  count()

# ...into [115654] normalized vendor names
contracts_individual_entries %>%
  select(d_vendor_name) %>%
  distinct() %>%
  count()
