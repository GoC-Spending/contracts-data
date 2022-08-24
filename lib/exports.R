# Helper functions for CSV exports for Government of Canada
# Proactive Disclosure of Contracts Data

source("lib/_libraries.R")

# Options =======================================

output_vendor_path <- "data/out/vendors/"
output_department_path <- "data/out/departments/"
output_category_path <- "data/out/categories/"
output_overall_path <- "data/out/overall/"
output_meta_path <- "data/out/meta/"

option_round_totals_digits <- 2
option_round_percentages_digits <- 4
option_round_years_digits <- 2

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
      across(ends_with("total"), ~ format(round(.x, digits = !!option_round_totals_digits), nsmall = !!option_round_totals_digits, trim = TRUE))
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

# Rounds any column ending in "years" to 2 decimal places
exports_round_years <- function(input_df) {
  input_df <- input_df %>%
    mutate(
      across(ends_with("years"), ~ round(.x, digits = !!option_round_years_digits))
    )
  
  return(input_df)
}

# File and folder helpers =======================

# Helper function to quickly turn on and off CSV writes when testing
# Can be drop-in replaced for write_csv in other functions.
write_csv_if_enabled <- function(...) {
  if(option_update_summary_csv_files == TRUE) {
    write_csv(...)
  } else {
    cat("Note: option_update_summary_csv_files is disabled; would have exported a CSV to:", list(...)[[2]], "\n")
  }
}

# If the option is enabled, then remove the summary folders if they exist (for vendors and departments, which might have old entities sitting around).
remove_existing_summary_folders <- function() {
  if(option_update_summary_csv_files == TRUE & option_remove_existing_summary_folders == TRUE) {
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
    if(dir_exists(output_category_path)) {
      dir_delete(output_category_path)
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

# Filter according to a set of departments/agencies.
filter_by_summary_type <- function(input_df, summary_type) {
  if(summary_type == "core") {
    included_orgs <- owner_org_types %>%
      filter(!is.na(is_core)) %>%
      pull(owner_org)
    
  } else if(summary_type == "dnd") {
    included_orgs <- owner_org_types %>%
      filter(!is.na(is_dnd)) %>%
      pull(owner_org)
    
  } else {
    # TODO: just return the full input_df here
    # to avoid filtering out departments that are new since
    # owner_org_types was generated.
    included_orgs <- owner_org_types %>%
      pull(owner_org)
  }
  output <- input_df %>%
    filter(owner_org %in% included_orgs)
  
  return(output)
}

# For operations that group by vendor_name, limit this to the threshold set of vendors to avoid giant CSV files with every single vendor.
filter_vendors_if_required <- function(input_df, filter_vendors = FALSE) {
  
  if(filter_vendors) {
    input_df <- input_df %>%
      filter(d_vendor_name %in% summary_included_vendors)
  }
  
  return(input_df)
  
}

# Reusable function for all "summary overall by fiscal year by X" functions
get_summary_overall_by_fiscal_year_by_criteria <- function(summary_type, grouping_column, filter_vendors = FALSE) {
  
  # Thanks to
  # https://stackoverflow.com/a/66253244/756641
  # https://stackoverflow.com/a/55165966/756641
  # https://stackoverflow.com/a/61180370/756641
  # re: variable handling for grouping_column
  # Note: not sure why the handling is different for group_by versus select, but the code below works as expected.
  # Using !!! in the group_by function (in place of across(all_of())) does not work.
  
  summary_overall_total_by_fiscal_year_by_criteria <- contract_spending_by_date %>%
    filter_by_summary_type(summary_type) %>%
    filter_vendors_if_required(filter_vendors) %>%
    group_by(across(all_of(grouping_column)), d_fiscal_year_short) %>%
    summarise(
      total = sum(d_daily_contract_value, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      d_fiscal_year = convert_start_year_to_fiscal_year(d_fiscal_year_short)
    ) %>%
    select(!!!grouping_column, d_fiscal_year, total) %>%
    exports_round_totals()
  
  return(summary_overall_total_by_fiscal_year_by_criteria)
  
}

get_summary_overall_by_fiscal_year <- function(summary_type) {
  
  summary_overall_total_by_fiscal_year <- contract_spending_by_date %>%
    filter_by_summary_type(summary_type) %>%
    group_by(d_fiscal_year_short) %>%
    summarise(
      total = sum(d_daily_contract_value, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      d_fiscal_year = convert_start_year_to_fiscal_year(d_fiscal_year_short)
    ) %>%
    select(d_fiscal_year, total) %>%
    exports_round_totals()
  
  return(summary_overall_total_by_fiscal_year)
  
}

get_summary_overall_by_fiscal_year_by_vendor <- function(summary_type) {
  
  return(get_summary_overall_by_fiscal_year_by_criteria(summary_type, "d_vendor_name", TRUE))
  
  # summary_total_by_vendor_and_fiscal_year <- contract_spending_by_date %>%
  #   filter(d_vendor_name %in% summary_included_vendors) %>%
  #   filter_by_summary_type(summary_type) %>%
  #   group_by(d_vendor_name, d_fiscal_year_short) %>%
  #   summarise(
  #     total = sum(d_daily_contract_value)
  #   ) %>%
  #   ungroup() %>%
  #   mutate(
  #     d_fiscal_year = convert_start_year_to_fiscal_year(d_fiscal_year_short)
  #   ) %>%
  #   select(d_vendor_name, d_fiscal_year, total) %>%
  #   exports_round_totals()
  # 
  # return(summary_total_by_vendor_and_fiscal_year)
}

get_summary_overall_by_fiscal_year_by_category <- function(summary_type) {
  
  return(get_summary_overall_by_fiscal_year_by_criteria(summary_type, "d_most_recent_category"))
  # 
  # summary_overall_total_by_category_and_fiscal_year <- contract_spending_by_date %>%
  #   filter_by_summary_type(summary_type) %>%
  #   group_by(d_most_recent_category, d_fiscal_year_short) %>%
  #   summarise(
  #     total = sum(d_daily_contract_value, na.rm = TRUE)
  #   ) %>%
  #   ungroup() %>%
  #   mutate(
  #     d_fiscal_year = convert_start_year_to_fiscal_year(d_fiscal_year_short)
  #   ) %>%
  #   select(d_most_recent_category, d_fiscal_year, total) %>%
  #   exports_round_totals()
  # 
  # return(summary_overall_total_by_category_and_fiscal_year)
}

get_summary_overall_by_fiscal_year_by_owner_org <- function(summary_type) {
  
  return(get_summary_overall_by_fiscal_year_by_criteria(summary_type, "owner_org"))
  
  # summary_overall_by_fiscal_year_by_owner_org <- contract_spending_by_date %>%
  #   filter_by_summary_type(summary_type) %>%
  #   group_by(owner_org, d_fiscal_year_short) %>%
  #   summarise(
  #     total = sum(d_daily_contract_value, na.rm = TRUE)
  #   ) %>%
  #   ungroup() %>%
  #   mutate(
  #     d_fiscal_year = convert_start_year_to_fiscal_year(d_fiscal_year_short)
  #   ) %>%
  #   select(owner_org, d_fiscal_year, total) %>%
  #   arrange(owner_org, d_fiscal_year) %>%
  #   exports_round_totals()
  # 
  # return(summary_overall_by_fiscal_year_by_owner_org)
}

# For overall totals (in the overall year range) without fiscal year grouping:
get_summary_overall_by_criteria <- function(summary_type, grouping_column, filter_vendors = FALSE) {

  
  summary_overall_total_by_criteria <- contract_spending_by_date %>%
    filter_by_summary_type(summary_type) %>%
    filter_vendors_if_required(filter_vendors) %>%
    group_by(across(all_of(grouping_column))) %>%
    summarise(
      total = sum(d_daily_contract_value, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    select(!!!grouping_column, total) %>%
    arrange(desc(total)) %>%
    exports_round_totals()
  
  return(summary_overall_total_by_criteria)
  
}

get_summary_overall_by_vendor <- function(summary_type) {
  
  return(get_summary_overall_by_criteria(summary_type, "d_vendor_name", TRUE))
  
}

get_summary_overall_by_category <- function(summary_type) {
  
  return(get_summary_overall_by_criteria(summary_type, "d_most_recent_category"))
  
}

get_summary_overall_by_owner_org <- function(summary_type) {
  
  return(get_summary_overall_by_criteria(summary_type, "owner_org"))
  
}

# Generic export listcolumn to CSV functions ====

# Export all of the summary_overall list-columns at once
# In a generic way that's reusable for (for example) summary_vendors and other future summary listcolumns.
export_summary <- function(summary_listcolumn_df, output_path) {
  
  # For example: summary_type
  entity_column <- names(summary_listcolumn_df)[1]
  
  # List columns, e.g. everything except the first column
  listcolumns <- names(summary_listcolumn_df)[2:length(summary_listcolumn_df)]
  
  for(listcolumn in listcolumns) {
    export_individual_listcolumn(
      listcolumn, 
      summary_listcolumn_df[[listcolumn]],
      output_path,
      summary_listcolumn_df[[entity_column]]
    )
  }

}

export_individual_listcolumn <- function(filename, listcolumn_data, output_path, entities) {
  
  pwalk(
    list(
      listcolumn_data,
      str_c(output_path, get_vendor_filename_from_vendor_name(entities), "/", filename, ".csv")
    ), 
    write_csv_if_enabled)
  
}


# Summary by fiscal year functions (reusable by entity type) ====

group_by_grouping_column_and_fiscal_year_if_required <- function(df, grouping_column) {
  
  if(grouping_column != FALSE) {
    df <- df %>%
      group_by(across(all_of(grouping_column)), d_fiscal_year_short)
  } else {
    df <- df %>%
      group_by(d_fiscal_year_short)
  }
  
  df
  
}

# Note: in hindsight, this could also potentially have been done using
# group_by(grouping_column, .add = TRUE) etc.
# https://dplyr.tidyverse.org/reference/group_by.html
select_by_grouping_column_if_required <- function(df, grouping_column) {
  
  if(grouping_column != FALSE) {
    df <- df %>%
      select(!!!grouping_column, d_fiscal_year, total)
  } else {
    df <- df %>%
      select(d_fiscal_year, total)
  }
  
  df
  
}

get_summary_by_fiscal_year_by_specific_entity <- function(filter_column, filter_search, grouping_column = FALSE, filter_vendors = FALSE) {
  
  output <- contract_spending_by_date %>%
    filter(across(all_of(filter_column)) == !!filter_search) %>%
    filter_vendors_if_required(filter_vendors) %>%
    group_by_grouping_column_and_fiscal_year_if_required(grouping_column) %>%
    summarise(
      total = sum(d_daily_contract_value, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      d_fiscal_year = convert_start_year_to_fiscal_year(d_fiscal_year_short)
    ) %>%
    select_by_grouping_column_if_required(grouping_column) %>%
    exports_round_totals()
  
  return(output)
  
}

# # Example usage
# 
# ## By org
# x <- get_summary_by_fiscal_year_by_specific_entity("owner_org", "tc", "d_most_recent_category")
# y <- get_summary_total_by_category_and_fiscal_year_by_owner_org("tc")
# setdiff(x,y)
# 
# get_summary_by_fiscal_year_by_specific_entity("owner_org", "tc", "d_vendor_name", TRUE)
# get_summary_total_by_vendor_and_fiscal_year_by_owner("tc")
# 
# 
# ## By vendor
# get_summary_by_fiscal_year_by_specific_entity("d_vendor_name", "ACCENTURE", "owner_org")
# get_summary_total_by_fiscal_year_and_owner_org_by_vendor("ACCENTURE")
# 
# get_summary_by_fiscal_year_by_specific_entity("d_vendor_name", "ACCENTURE", "d_most_recent_category")
# get_summary_total_by_fiscal_year_and_category_by_vendor("ACCENTURE")
# 
# 
# ## By category
# get_summary_by_fiscal_year_by_specific_entity("d_most_recent_category", "2_professional_services", "owner_org")
# get_summary_total_by_owner_org_and_fiscal_year_by_category("2_professional_services")
# 
# get_summary_by_fiscal_year_by_specific_entity("d_most_recent_category", "2_professional_services", "d_vendor_name", TRUE)
# get_summary_total_by_vendor_and_fiscal_year_by_category("2_professional_services")
# 
# ## Optionally skip a grouping column and just get the full fiscal year breakdown for the specific entity.
# get_summary_by_fiscal_year_by_specific_entity("d_most_recent_category", "2_professional_services")
# get_summary_by_fiscal_year_by_specific_entity("d_vendor_name", "ACCENTURE")


# Summary by owner_org (functions) ========================

# For each owner org, get a list of the top n companies

# Define a reusable function
get_summary_overall_total_by_vendor_by_owner <- function(owner_org) {
  
  # Thanks to
  # https://stackoverflow.com/a/46763370/756641
  # Note: this uses the same overall top_n_vendors (now summary_included_vendors) calculated previously (based on average annual spending above the summary threshold amount).
  output <- contract_spending_by_date %>%
    filter(owner_org == !!owner_org) %>%
    filter(d_vendor_name %in% summary_included_vendors) %>%
    group_by(d_vendor_name) %>%
    summarise(
      overall_total = sum(d_daily_contract_value)
    ) %>%
    arrange(desc(overall_total)) %>%
    exports_round_totals()
  
  return(output)
  
}

# Reusable function to get a per-fiscal year breakdown
# TODO: if this was called first, it'd be more efficient since it
# re-does the summary in the previous function above.
# In that case, you could do this first, and then sum it all up, 
# to get the results of the previous function.
get_summary_total_by_vendor_and_fiscal_year_by_owner <- function(owner_org) {
  
  # Note: update this to re-use the already generated shortlist (based on the annual threshold.)
  # This shortlist now lives in summary_included_vendors
  # top_n_vendors <- get_summary_overall_total_by_vendor_by_owner(owner_org) %>%
  #   pull(d_vendor_name)
  
  # Then, for those top n vendors, group by fiscal year
  output <- contract_spending_by_date %>%
    filter(owner_org == !!owner_org) %>%
    filter(d_vendor_name %in% summary_included_vendors) %>%
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
  
  return(output)
  
}

# Get a category summary by department or agency
get_summary_total_by_category_by_owner_org <- function(owner_org) {
  
  output <- contract_spending_by_date %>%
    filter(owner_org == !!owner_org) %>%
    group_by(d_most_recent_category) %>%
    summarise(
      total = sum(d_daily_contract_value, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    select(d_most_recent_category, total) %>%
    mutate(
      percentage = total / sum(total)
    ) %>%
    arrange(desc(total)) %>%
    exports_round_totals()
  
  return(output)
  
}

# Get a category and fiscal year summary by department or agency
get_summary_total_by_category_and_fiscal_year_by_owner_org <- function(owner_org) {
  
  output <- contract_spending_by_date %>%
    filter(owner_org == !!owner_org) %>%
    group_by(d_most_recent_category, d_fiscal_year_short) %>%
    summarise(
      total = sum(d_daily_contract_value, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      d_fiscal_year = convert_start_year_to_fiscal_year(d_fiscal_year_short)
    ) %>%
    select(d_most_recent_category, d_fiscal_year, total) %>%
    exports_round_totals()
  
  return(output)
  
}

# Get a fiscal year overall summary by department or agency
get_summary_total_by_fiscal_year_by_owner_org <- function(owner_org) {
  
  output <- contract_spending_by_date %>%
    filter(owner_org == !!owner_org) %>%
    group_by(d_fiscal_year_short) %>%
    summarise(
      total = sum(d_daily_contract_value, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      d_fiscal_year = convert_start_year_to_fiscal_year(d_fiscal_year_short)
    ) %>%
    select(d_fiscal_year, total) %>%
    exports_round_totals()
  
  return(output)
  
}


# Summary by vendor (functions) =================


# For each of the top n vendors, get a per-fiscal year breakdown
get_summary_total_by_fiscal_year_by_vendor <- function(requested_vendor_name) {
  
  output <- contract_spending_by_date %>%
    filter(d_vendor_name == !!requested_vendor_name) %>%
    group_by(d_fiscal_year_short) %>%
    summarise(
      total = sum(d_daily_contract_value, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      d_fiscal_year = convert_start_year_to_fiscal_year(d_fiscal_year_short)
    ) %>%
    select(d_fiscal_year, total) %>%
    exports_round_totals()
  
  return(output)
}

# For each of the top n vendors, get a per-fiscal year and per-owner_org breakdown
get_summary_total_by_fiscal_year_and_owner_org_by_vendor <- function(requested_vendor_name) {
  
  output <- contract_spending_by_date %>%
    filter(d_vendor_name == !!requested_vendor_name) %>%
    group_by(owner_org, d_fiscal_year_short) %>%
    summarise(
      total = sum(d_daily_contract_value)
    ) %>%
    ungroup() %>%
    mutate(
      d_fiscal_year = convert_start_year_to_fiscal_year(d_fiscal_year_short)
    ) %>%
    select(owner_org, d_fiscal_year, total) %>%
    exports_round_totals()
  
  return(output)
}

# For each of the top n vendors, get a per-fiscal year and per-category breakdown
get_summary_total_by_fiscal_year_and_category_by_vendor <- function(requested_vendor_name) {
  
  output <- contract_spending_by_date %>%
    filter(d_vendor_name == !!requested_vendor_name) %>%
    group_by(d_most_recent_category, d_fiscal_year_short) %>%
    summarise(
      total = sum(d_daily_contract_value)
    ) %>%
    ungroup() %>%
    mutate(
      d_fiscal_year = convert_start_year_to_fiscal_year(d_fiscal_year_short)
    ) %>%
    select(d_most_recent_category, d_fiscal_year, total) %>%
    exports_round_totals()
  
  return(output)
}

# For each of the top n vendors, get a per-category breakdown
get_summary_total_by_category_by_vendor <- function(requested_vendor_name) {
  
  output <- contract_spending_by_date %>%
    filter(d_vendor_name == !!requested_vendor_name) %>%
    group_by(d_most_recent_category) %>%
    summarise(
      total = sum(d_daily_contract_value)
    ) %>%
    ungroup() %>%
    select(d_most_recent_category, total) %>%
    mutate(
      percentage = total / sum(total)
    ) %>%
    arrange(desc(total)) %>%
    exports_round_totals()
  
  return(output)
}

get_original_vendor_names <- function(requested_vendor_name, sort_by_value = TRUE) {
  
  output <- contracts %>%
    filter(d_vendor_name == !!requested_vendor_name) %>%
    select(d_vendor_name, vendor_name, contract_value) %>%
    arrange(desc(contract_value)) %>%
    select(d_vendor_name, vendor_name) %>%
    distinct() %>%
    rename(
      normalized_vendor_name = "d_vendor_name",
      original_vendor_name= "vendor_name"
    )
  
  if(! sort_by_value) {
    output %>%
      arrange(original_vendor_name)
  }
  else {
    output
  }
  
}


# Category summary functions ====================

# TODO: Merge these into a common shared function (only distinction is grouping and selecting on d_vendor_name vs. owner_org)
get_summary_total_by_vendor_and_fiscal_year_by_category <- function(category) {
  
  output <- contract_spending_by_date %>%
    filter(d_most_recent_category == !!category) %>%
    filter(d_vendor_name %in% summary_included_vendors) %>%
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
  
  return(output)
  
}

get_summary_total_by_owner_org_and_fiscal_year_by_category <- function(category) {
  
  output <- contract_spending_by_date %>%
    filter(d_most_recent_category == !!category) %>%
    group_by(owner_org, d_fiscal_year_short) %>%
    summarise(
      total = sum(d_daily_contract_value)
    ) %>%
    ungroup() %>%
    mutate(
      d_fiscal_year = convert_start_year_to_fiscal_year(d_fiscal_year_short)
    ) %>%
    select(owner_org, d_fiscal_year, total) %>%
    exports_round_totals()
  
  return(output)
  
}

get_summary_total_by_fiscal_year_by_category <- function(category) {
  
  output <- contract_spending_by_date %>%
    filter(d_most_recent_category == !!category) %>%
    group_by(d_fiscal_year_short) %>%
    summarise(
      total = sum(d_daily_contract_value, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      d_fiscal_year = convert_start_year_to_fiscal_year(d_fiscal_year_short)
    ) %>%
    select(d_fiscal_year, total) %>%
    exports_round_totals()
  
  return(output)
  
}

get_summary_overall_total_by_vendor_by_category <- function(category) {
  
  output <- contract_spending_by_date %>%
    filter(d_most_recent_category == !!category) %>%
    filter(d_vendor_name %in% summary_included_vendors) %>%
    group_by(d_vendor_name) %>%
    summarise(
      overall_total = sum(d_daily_contract_value)
    ) %>%
    ungroup() %>%
    arrange(desc(overall_total)) %>%
    select(d_vendor_name, overall_total) %>%
    exports_round_totals()
  
  return(output)
  
}

get_summary_overall_total_by_owner_org_by_category <- function(category) {
  
  output <- contract_spending_by_date %>%
    filter(d_most_recent_category == !!category) %>%
    group_by(owner_org) %>%
    summarise(
      overall_total = sum(d_daily_contract_value)
    ) %>%
    ungroup() %>%
    arrange(desc(overall_total)) %>%
    select(owner_org, overall_total) %>%
    exports_round_totals()
  
  return(output)
  
}
