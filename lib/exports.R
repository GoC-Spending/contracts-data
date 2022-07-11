# Helper functions for CSV exports for Government of Canada
# Proactive Disclosure of Contracts Data

source("lib/_libraries.R")

# Options =======================================

output_vendor_path <- "data/out/vendors/"
output_department_path <- "data/out/departments/"
output_overall_path <- "data/out/overall/"

option_round_totals_digits <- 2
option_round_percentages_digits <- 4

owner_org_type_file <- "data/owner_orgs/owner_orgs.csv"

# Import the CSV file
owner_org_types <- read_csv(
  owner_org_type_file,
) %>%
  clean_names()

# Rounding and number formatting ================

# Rounds any column ending in "total" to 2 decimal places
# Thanks to
# https://dplyr.tidyverse.org/reference/across.html
exports_round_totals <- function(input_df) {
  input_df <- input_df %>%
    mutate(
      across(ends_with("total"), ~ round(.x, digits = !!option_round_totals_digits))
    )
  
  return(input_df)
}

# Rounds any column ending in "percentage" to 4 decimal places
# Note: not sure yet if this is useful
exports_round_percentages <- function(input_df) {
  input_df <- input_df %>%
    mutate(
      across(ends_with("percentage"), ~ round(.x, digits = !!option_round_percentages_digits))
    )
  
  return(input_df)
}

# File and folder helpers =======================

# Helper function to quickly turn on and off CSV writes when testing
# Can be drop-in replaced for write_csv in other functions.
write_csv_if_enabled <- function(...) {
  if(option_update_summary_csv_files) {
    write_csv(...)
  } else {
    cat("Note: option_update_summary_csv_files is disabled; would have exported a CSV to:", list(...)[[2]], "\n")
  }
}

# If the option is enabled, then remove the summary folders if they exist (for vendors and departments, which might have old entities sitting around).
remove_existing_summary_folders <- function() {
  if(option_remove_existing_summary_folders) {
    print("Removing existing summary folders.")
    if(dir_exists(output_vendor_path)) {
      dir_delete(output_vendor_path)
    }
    if(dir_exists(output_department_path)) {
      dir_delete(output_department_path)
    }
    if(dir_exists(output_overall_path)) {
      dir_delete(output_overall_path)
    }
  } else {
    print("Not removing existing summary folders.")
  }
}

# Create summary folders
# output_path should be one of output_vendor_path, output_department_path, etc.
# entities should be a vector of e.g. department acronyms, or vendor name slugs, to use as folder names
create_summary_folders <- function(output_path, entities) {
  print(str_c("Generating summary folders in: ", output_path))
  # Note: if these directories already exist, this still works as-is.
  # https://fs.r-lib.org/reference/create.html
  owner_org_output_paths <- str_c(output_path, get_vendor_filename_from_vendor_name(entities))
  dir_create(owner_org_output_paths)
}

# Export functions for specific data types ======

get_summary_included_vendors <- function() {
  # Note: this includes all vendors with an average annual total of at least the summary_vendor_annual_total_threshold over the summary coverage range (start to end years).
  summary_overall_total_by_vendor <- contract_spending_by_date %>%
    group_by(d_vendor_name) %>%
    summarise(
      overall_total = sum(d_daily_contract_value)
    ) %>%
    arrange(desc(overall_total)) %>%
    filter(
      overall_total >= summary_vendor_annual_total_threshold * summary_total_years
    )
  
  top_n_vendors <- summary_overall_total_by_vendor %>%
    pull(d_vendor_name)
  
  return(top_n_vendors)
}

# TODO: Load these from a CSV file for easier updating, rather than hard-coding them here.
filter_by_summary_type <- function(input_df, summary_type) {
  if(summary_type == "core") {
    included_orgs <- owner_org_types %>%
      filter(!is.na(is_core)) %>%
      pull(department)
    
  } else if(summary_type == "dnd") {
    included_orgs <- owner_org_types %>%
      filter(!is.na(is_dnd)) %>%
      pull(department)
    
  } else {
    included_orgs <- owner_org_types %>%
      pull(department)
  }
  output <- input_df %>%
    filter(owner_org %in% included_orgs)
  
  return(output)
}

get_summary_overall_by_fiscal_year_by_vendor <- function(summary_type) {
  summary_total_by_vendor_and_fiscal_year <- contract_spending_by_date %>%
    filter(d_vendor_name %in% summary_included_vendors) %>%
    filter_by_summary_type(summary_type) %>%
    group_by(d_vendor_name, d_fiscal_year_short) %>%
    summarise(
      total = sum(d_daily_contract_value)
    ) %>%
    ungroup() %>%
    mutate(
      d_fiscal_year = convert_start_year_to_fiscal_year(d_fiscal_year_short)
    ) %>%
    select(d_vendor_name, d_fiscal_year, total) %>%
    exports_round_totals()
  
  return(summary_total_by_vendor_and_fiscal_year)
}

# Export all of the summary_overall list-columns at once
# TODO: abstract this into a function that can handle all listcolumns in summary_overall
export_summary_overall <- function(summary_overall) {
  
  # For example: summary_type
  entity_column <- names(summary_overall)[1]
  
  # List columns, e.g. everything except the first column
  listcolumns <- names(summary_overall)[2:length(summary_overall)]
  
  for(listcolumn in listcolumns) {
    export_individual_listcolumn(
      listcolumn, 
      summary_overall[[listcolumn]],
      output_overall_path,
      summary_overall[[entity_column]]
    )
  }
  
  # # Note: First of several planned listcolumns here
  # export_individual_listcolumn(
  #   "summary_overall_by_fiscal_year_by_vendor", 
  #   summary_overall$summary_overall_by_fiscal_year_by_vendor,
  #   output_overall_path,
  #   summary_overall$summary_type
  #   )
}

export_individual_listcolumn <- function(filename, listcolumn_data, output_path, entities) {
  
  pwalk(
    list(
      listcolumn_data,
      str_c(output_path, get_vendor_filename_from_vendor_name(entities), "/", filename, ".csv")
    ), 
    write_csv_if_enabled)
  
}
