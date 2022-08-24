# Research paper calculations
# for Government of Canada Proactive Disclosure of Contracts Data

source("lib/_libraries.R")
source("lib/exports.R")

# Work in progress
# These functions will be called from load.R; safe to anticipate that they'll be running when the contracts data is loaded in memory.

# Dataframes:

# contract_spending_by_date is the per-day breakdown of spending,
# useful for calculating (estimated) actual spending over time.

# contract_spending_overall includes a row for each "group" representing
# a contract and its amendments.


# Contracts that were new since the start of the summary_start_fiscal_year_short fiscal year:
contract_spending_overall_initiated <- contract_spending_overall %>%
  filter(
    d_overall_start_date >= ymd(str_c(summary_start_fiscal_year_short,"04","01"))
  )

# Contracts that have been active since the start of the summary_start_fiscal_year_short fiscal year:
contract_spending_overall_ongoing <- contract_spending_overall %>%
  filter(
    d_overall_end_date >= ymd(str_c(summary_start_fiscal_year_short,"04","01"))
  )


# Research paper findings by section number ===============

# These re-use filter_by_summary_type and filter_vendors_if_required from exports.R

# 4.2.1 Mean contract value
s421_mean_contract_value <- function(df) {
  df %>%
    summarise(
      mean_original = mean(d_original_contract_value), 
      mean_overall = mean(d_overall_contract_value),
      n = n())
}

s422_max_contract_value <- function(df) {
  df %>%
    summarise(
      max_overall = max(d_overall_contract_value), 
      n = n())
}

s423_min_contract_value <- function(df) {
  df %>%
    summarise(
      min_overall = min(d_overall_contract_value), 
      n = n())
}

s42_calculate_duration <- function(df) {
  df <- df %>%
    mutate(
      duration_days = as.integer(d_overall_end_date - d_overall_start_date + 1),
      duration_years = duration_days / 365
    )
  
  df
}

s424_mean_duration <- function(df) {
  
  df <- df %>%
    s42_calculate_duration()
  
  df %>%
    summarise(
      mean_years = mean(duration_years), 
      n = n()) %>%
    exports_round_years()
  
}

s425_max_duration <- function(df) {
  
  df <- df %>%
    s42_calculate_duration()
  
  df %>%
    summarise(
      max_years = max(duration_years), 
      n = n()) %>%
    exports_round_years()

}

# Usage is e.g.
# do_research_findings_call("s421_mean_contract_value", "core")
# do_research_findings_call("s421_mean_contract_value", "core", "owner_org")
# do_research_findings_call("s421_mean_contract_value", "core", "d_vendor_name", TRUE)
# do_research_findings_call("s421_mean_contract_value", "core", "d_most_recent_category")
do_research_findings_call <- function(function_name, summary_type, grouping_column = FALSE, filter_vendors = FALSE) {
  
  output <- contract_spending_overall_initiated %>%
    filter_by_summary_type(summary_type) %>%
    filter_vendors_if_required(filter_vendors)
  
  if(grouping_column != FALSE) {
    output <- output %>%
      group_by(across(all_of(grouping_column)))
  }
  
  # Thanks to
  # https://stackoverflow.com/questions/62202574/how-to-safely-use-do-call-within-dplyr-pipe#comment110012904_62202574
  output %>%
    {exec(function_name, .)}
    
  
}

