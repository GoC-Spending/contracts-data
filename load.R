# Load libraries and helper functions
source("lib/helpers.R")
source("lib/amendments.R")
source("lib/vendors.R")
source("lib/categories.R")
source("lib/exports.R")

# Start time
run_start_time <- now()
paste("Start time:", run_start_time)

# Options =======================================

# Summary parameters (used below)
summary_start_fiscal_year_short <- 2017
summary_end_fiscal_year_short <- 2020
summary_vendor_annual_total_threshold <- 1000000

# Note: to be deprecated
summary_total_vendor_rows <- 400
summary_per_owner_org_vendor_rows <- 100

# "data/source/2022-03-24-contracts.csv"
# "data/testing/2022-04-13-sample-contracts.csv"
# "data/testing/2022-06-28-contracts-key-amendment-testing.csv"
option_contracts_data_source <- "data/testing/2022-04-13-sample-contracts.csv"
option_download_remotely <- TRUE
option_update_summary_csv_files <- TRUE
option_remove_existing_summary_folders <- TRUE
option_remove_derived_columns <- TRUE

# Data import ===================================

# Define column types for each column in the contracts dataset
# Y/N values are converted to TRUE/FALSE further below
contract_col_types <- cols(
  
  reference_number = col_character(),
  procurement_id = col_character(),
  vendor_name = col_character(),
  vendor_postal_code = col_character(),
  buyer_name = col_character(),
  
  contract_date = col_date(),
  
  economic_object_code = col_character(),
  description_en = col_character(),
  description_fr = col_character(),
  
  contract_period_start = col_date(),
  delivery_date = col_date(),
  
  contract_value = col_number(),
  original_value = col_number(),
  amendment_value = col_number(),
  
  comments_en = col_character(),
  comments_fr = col_character(),
  additional_comments_en = col_character(),
  additional_comments_fr = col_character(),
  
  agreement_type_code = col_character(),
  trade_agreement = col_character(),
  land_claims = col_character(),
  commodity_type = col_factor(),
  commodity_code = col_character(),
  country_of_vendor = col_character(),
  solicitation_procedure = col_factor(),
  limited_tendering_reason = col_character(),
  trade_agreement_exceptions = col_character(),
  aboriginal_business = col_character(),
  aboriginal_business_incidental = col_character(), # Y/N
  intellectual_property = col_character(),
  potential_commercial_exploitation = col_character(), # Y/N
  former_public_servant = col_character(), # Y/N
  contracting_entity = col_character(),
  standing_offer_number = col_character(),
  instrument_type = col_factor(),
  ministers_office = col_character(), # Y/N
  number_of_bids = col_number(),
  article_6_exceptions = col_character(),
  award_criteria = col_character(),
  socioeconomic_indicator = col_character(),
  reporting_period = col_character(),
  
  owner_org = col_character(),
  owner_org_title = col_character(),
  
)


# Download the contracts csv file, list it for today (if not already downloaded) and then parse it:
if(option_download_remotely) {
  print("Downloading remotely (if not already downloaded today)")
  contracts <- get_contracts_csv_locally_or_from_url(contract_col_types)
} else {
  print("Using the local copy stored at:")
  print(option_contracts_data_source)
  # Previous version (for local operations)
  # Import the CSV file
  contracts <- read_csv(
    option_contracts_data_source,
    col_types = contract_col_types
  ) %>%
    clean_names()
  
  if(option_remove_derived_columns) {
    # If you're loading previously exported contracts testing data
    # remove the derived columns and the (joined) category column.
    # This avoids column name overlap conflicts later.
    # Note: This only applies to locally-loaded files (since the source data CSV from open.canada.ca doesn't include the derived columns.)
    contracts <- contracts %>%
      select(!starts_with("d_")) %>%
      select(!starts_with("category"))
  }
}

# Initial data mutations ========================

# Convert Y/N values to logical TRUE/FALSE values, for specific columns
contracts <- contracts %>%
  mutate(
    across(c(aboriginal_business_incidental, potential_commercial_exploitation, former_public_servant, ministers_office),
           ~ recode(., 
                    "N" = FALSE, 
                    "Y" = TRUE,
                    .default = as.logical(NA))
    )
  )

# Correct any accidentally-switched reference numbers and procurement IDs
contracts <- contracts %>%
  mutate(
    d_procurement_id = case_when(
      str_detect(procurement_id, "\\d{4}-\\d{4}-Q\\d-") ~ reference_number,
      TRUE ~ procurement_id
    ),
    d_reference_number = case_when(
      str_detect(procurement_id, "\\d{4}-\\d{4}-Q\\d-") ~ procurement_id,
      TRUE ~ reference_number
    )
  )

# Remove any suffixes (of 3 characters/digits or less) from procurement IDs, to improve the accuracy of procurement ID-based amendment grouping.
# Note: This could be refactored using regexes instead of a multi-stage mutate with string locations. The code below reverses the order of the procurement ID, finds the first "/" (used to denote suffixes in procurement IDs), and if it exists and is less than or equal to 4 characters (including the "/") then it uses the procurement ID with that suffix portion removed.
contracts <- contracts %>%
  mutate(
    d_procurement_id_suffix_reversed_id = stri_reverse(d_procurement_id),
    d_procurement_id_suffix_pos = str_locate(d_procurement_id_suffix_reversed_id, "/")[,"start"],
    d_procurement_id_suffix_remainder = str_sub(d_procurement_id_suffix_reversed_id, start = d_procurement_id_suffix_pos + 1L),
    d_procurement_id = case_when(
      d_procurement_id_suffix_pos <= 4 ~ stri_reverse(d_procurement_id_suffix_remainder),
      TRUE ~ d_procurement_id
    )
  ) %>%
  select(! starts_with("d_procurement_id_suffix"))

# Use the reporting period if it exists, otherwise get it from the reference number
# Store it in d_reporting_period (derived reporting period)
contracts <- contracts %>%
  mutate(
    d_reporting_period = case_when(
      is_valid_reporting_period(reporting_period) ~ reporting_period,
      !is.na(get_reporting_period_from_reference_number(d_reference_number)) ~ get_reporting_period_from_reference_number(d_reference_number),
      TRUE ~ get_reporting_period_from_date(contract_date)
    )
  )


# Sort the contracts dataset by ascending reporting period, then owner_org
contracts <- contracts %>%
  arrange(d_reporting_period, owner_org)
  

# For grouping/comparison purposes, get the fiscal year from the reporting period
contracts <- contracts %>%
  mutate(
    d_reporting_year = get_fiscal_year_from_reporting_period(d_reporting_period)
  )


# Create a (possibly) unique ID for each row
# Depends on each department maintaining unique reference_number entries
# which appears to be the case.
# Can be checked using janitor with 
# contracts %>% get_dupes(d_reference_number)
contracts <- contracts %>%
  mutate(
    d_reference_number = str_c(owner_org, "-", d_reference_number)
  )
 

# Create a start date using (in order of priority)
# contract_period_start, contract_date
# (original logic in assureRequiredContractValues in the PHP code)
contracts <- contracts %>%
  mutate(
    d_start_date = case_when(
      !is.na(contract_period_start) ~ contract_period_start,
      !is.na(contract_date) ~ contract_date,
      TRUE ~ NA_Date_
    )
  )


# Create an end date using the row-wise largest of:
# delivery_date, contract_period_start, contract_date
contracts <- contracts %>%
  rowwise() %>% 
  mutate(
    d_end_date = max(delivery_date, contract_period_start, contract_date, na.rm = TRUE)
  ) %>%
  ungroup()

# Categorizing into an industry category ========

# Get slightly cleaner versions of descriptions and object codes
# 1. Detect any object codes *in* the description text, based on 3- or 4-digit numbers.
# 2. Add missing object codes.
# 3. Remove object codes from description text.
# 3. Clean up description text with str_squish and str_to_sentence.
contracts <- contracts %>%
  mutate(
    d_description_en_economic_object_code = str_extract(description_en, "^\\d{3,4}"),
    #d_description_en = str_extract(description_en, "\\D+"),
    d_description_en = description_en,
  ) %>%
  mutate(
    d_economic_object_code = case_when(
      !is.na(economic_object_code) ~ economic_object_code,
      !is.na(d_description_en_economic_object_code) ~ d_description_en_economic_object_code,
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    d_economic_object_code = str_pad(d_economic_object_code, 4, side = "left", pad = "0"),
    d_description_en = str_to_sentence(str_squish(d_description_en))
  )


# Add in industry categories (where these exist) using the category matching table.
contracts <- contracts %>%
  left_join(category_matching, by = "d_economic_object_code")


# Note: to add here, additional category matching based on contract descriptions, typical company activities based on other contract entries, etc.

# Add in industry categories (where these exist) using the description matching table.
contracts <- contracts %>%
  left_join(description_matching, by = "d_description_en")

# Use the category derived from economic object code, if it exists
# Otherwise use the category derived from the description field.
contracts <- contracts %>%
  mutate(
    category = NA_character_,
    category = case_when(
      !is.na(category_by_economic_object_code) ~ category_by_economic_object_code,
      !is.na(category_by_description) ~ category_by_description,
      TRUE ~ NA_character_
    )
  )

# Temp: get descriptions and object codes
# Note: this only includes descriptions with at least 10 entries.
# (Shortens the total list from 12k to 3k rows.)
# Note: review if "category" should be included here first.
# For consistency, just the economic object codes are selected and counted at first.
# Then, the left join and distinct() calls below add in example descriptions for ease of reference, from the chronologically most recent time in the data set that the economic object code is used.
contract_descriptions_object_codes <- contracts %>%
  select(d_economic_object_code) %>%
  group_by(d_economic_object_code) %>%
  add_count(d_economic_object_code, name = "count") %>%
  distinct() %>%
  #filter(n >= 10) %>%
  arrange(desc(count))
  
# Use a version of contracts in reverse chronological order
# to get higher-quality descriptions (more recent seem to be higher-quality)
# TODO: Confirm if this is a computationally-expensive procedure!
# Note: only three fields are currently kept; revisit this if this is useful for other operations.
contracts_reversed <- contracts %>%
  arrange(desc(d_reporting_period)) %>%
  select(d_reference_number, d_reporting_period, d_economic_object_code, d_description_en, category)

# Thanks to
# https://www.marsja.se/how-to-remove-duplicates-in-r-rows-columns-dplyr/
contract_descriptions_object_codes <- contract_descriptions_object_codes %>%
  left_join(contracts_reversed, by = "d_economic_object_code") %>%
  select(d_economic_object_code, count, d_description_en, category) %>%
  distinct(d_economic_object_code, .keep_all = TRUE)


# Vendor name normalization =====================

# Add vendor name normalization here, before amendments are found and combined below.
# For simplicity, the normalized names are stored in d_vendor_name
contracts <- contracts %>%
  mutate(
    d_clean_vendor_name = clean_vendor_names(vendor_name)
  ) %>%
  left_join(vendor_matching, by = c("d_clean_vendor_name" = "company_name")) %>%
  rename(d_normalized_vendor_name = "parent_company")

# For companies that aren't in the normalization table
# include their regular names anyway:
contracts <- contracts %>%
  mutate(
    d_vendor_name = case_when(
      !is.na(d_normalized_vendor_name) ~ d_normalized_vendor_name,
      TRUE ~ d_clean_vendor_name,
    )
  )

# Amendment group identification ================

# Find amendment groups based on procurement_id, or on start date + contract value 
contracts <- find_amendment_groups_v2(contracts)


# Calculate contract spending over time =========

# Contract spending by day
# Simplified version (linearized across the total duration as per the last amendment; doesn't account for changes in spending per-amendment in stages)
# For each amendment group, find the original row's start date, and the last row's end date
contract_spending_overall <- contracts %>%
  arrange(d_reporting_period, owner_org) %>% # This is done above, but for safety, doing it again here to ensure that the first() and last() calls below work properly.
  select(d_reference_number, d_vendor_name, d_start_date, d_end_date, contract_value, d_amendment_group_id, owner_org, d_number_of_amendments, d_economic_object_code, d_description_en, category, comments_en, additional_comments_en) %>%
  group_by(d_amendment_group_id) %>%
  mutate(
    d_most_recent_category = last(category),
    d_most_recent_description_en = last(d_description_en),
    d_overall_start_date = first(d_start_date),
    d_overall_end_date = last(d_end_date),
    # In some amendment cases, later amendment entries mean that the last(d_end_date) is earlier in time than the first(d_start_date).
    # To handle those cases, we'll set the d_overall_start_date to be the same as the d_overall_end_date - before doing the d_daily_contract_value calculation below.
    d_overall_start_date = case_when(
      d_overall_start_date > d_overall_end_date ~ d_overall_end_date,
      TRUE ~ d_overall_start_date,
    ),
    d_overall_contract_value = last(contract_value),
    d_daily_contract_value = d_overall_contract_value / as.integer(d_overall_end_date - d_overall_start_date + 1) # The +1 is added so it's inclusive of the start and end dates themselves.
  ) %>%
  ungroup()

# Maintain a set of contract vendor names for normalization troubleshooting later
vendor_names <- contracts %>%
  select(vendor_name, d_clean_vendor_name, d_normalized_vendor_name, d_vendor_name) %>%
  distinct() %>%
  arrange(d_clean_vendor_name)

# Get a list of all owner organizations (departments/agencies)
owner_orgs <- contracts %>%
  select(owner_org) %>%
  distinct() %>%
  arrange(owner_org) %>%
  pull(owner_org)

# TODO: Confirm if this is unhelpful later.
# Removes the original "contracts" object to save on system memory:
print("Reminder: removing the 'contracts' data frame to save memory.")
#rm(contracts)


# With thanks to
# https://github.com/lchski/parliamentarians-analysis/blob/master/analysis/members.R#L7-L13
# Note: if new columns are added to input dataframes, 
# they must be added to the complete() function call below.
contract_spending_by_date <- contract_spending_overall %>%
  select(owner_org, d_vendor_name, d_amendment_group_id, d_most_recent_category, d_overall_start_date, d_overall_end_date, d_daily_contract_value) %>%
  distinct() %>% # Since the same data is now in each row, don't multiple-count contracts with amendments
  pivot_longer(
    c(d_overall_start_date, d_overall_end_date),
    values_to = "date",
    names_to = NULL
    ) %>%
  group_by(d_amendment_group_id) %>%
  complete(date = full_seq(date, 1), nesting(owner_org, d_vendor_name, d_daily_contract_value, d_most_recent_category)) %>%
  ungroup()

# Add (short) fiscal year, for grouping calculations
contract_spending_by_date <- contract_spending_by_date %>%
  # TODO: update or refactor get_short_fiscal_year_from_date to match this, if helpful.
  mutate(
    d_fiscal_year_short = case_when(
      month(date) <= 3 ~ year(date) - 1,
      month(date) <= 12 ~ year(date),
      TRUE ~ NA_real_,
    )
  )

# Filter to just the requested fiscal year range
# (defined in the parameters at the top)
# Note: this applies to all of the calculations that follow.
contract_spending_by_date <- contract_spending_by_date %>%
  filter(
    d_fiscal_year_short >= summary_start_fiscal_year_short,
    d_fiscal_year_short <= summary_end_fiscal_year_short,
  ) 
  


# Summaries =====================================

# Helper variables
# e.g. 4 (years inclusive from the start to end of the coverage range)
summary_total_years <- summary_end_fiscal_year_short - summary_start_fiscal_year_short + 1L
# e.g. "2017_to_2020"
summary_overall_years_file_suffix <- str_c(summary_start_fiscal_year_short, "_to_", summary_end_fiscal_year_short)

# Determine which vendors have enough spending (e.g. averaging $1M per year in the time range, as defined in summary_vendor_annual_total_threshold)
summary_included_vendors <- get_summary_included_vendors()

# For overall (government-wide) summaries that will appear on the homepage, first create a list-column with the different summary types (core public service departments, DND, and all departments)
summary_overall = tibble(summary_type = c("core", "dnd", "all"))

# Note: to add, summaries across "all time" of the specified time range
# plus filename labels for these that indicate the time range
summary_overall <- summary_overall %>%
  mutate(
    summary_overall_by_fiscal_year_by_vendor = map(summary_type, get_summary_overall_by_fiscal_year_by_vendor),
    summary_overall_by_fiscal_year_by_category = map(summary_type, get_summary_overall_by_fiscal_year_by_category),
    summary_overall_by_fiscal_year_by_owner_org = map(summary_type, get_summary_overall_by_fiscal_year_by_owner_org),
    !!str_c("summary_overall_by_vendor", "_", summary_overall_years_file_suffix) := map(summary_type, get_summary_overall_by_vendor),
    !!str_c("summary_overall_by_category", "_", summary_overall_years_file_suffix) := map(summary_type, get_summary_overall_by_category),
    !!str_c("summary_overall_by_owner_org", "_", summary_overall_years_file_suffix) := map(summary_type, get_summary_overall_by_owner_org),
  )

# Make output directories, if needed
create_summary_folders(output_overall_path, summary_overall$summary_type)

# Note: work in progress here on summary exports
export_summary(summary_overall, output_overall_path)


# Previous summary approach =====================
# Note: will replace this with map functions in the near future.

# Summary 1: overall total (over all time and all contracts) by vendor
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

# Breakdown by fiscal year (for selected fiscal years)
# First, pull out the top n vendors
top_n_vendors <- summary_overall_total_by_vendor %>%
  pull(d_vendor_name)

# Summary 2: Then, for those top n vendors, group by fiscal year
summary_total_by_vendor_and_fiscal_year <- contract_spending_by_date %>%
  filter(d_vendor_name %in% top_n_vendors) %>%
  group_by(d_vendor_name, d_fiscal_year_short) %>%
  summarise(
    total = sum(d_daily_contract_value)
  ) %>%
  ungroup() %>%
  mutate(
    d_fiscal_year = convert_start_year_to_fiscal_year(d_fiscal_year_short)
  ) %>%
  select(d_vendor_name, d_fiscal_year, total)
  

# Summary 3: Get an overall total by category
summary_overall_total_by_category <- contract_spending_by_date %>%
  group_by(d_most_recent_category) %>%
  summarise(
    overall_total = sum(d_daily_contract_value, na.rm = TRUE)
  ) %>%
  arrange(desc(overall_total))


# Summary 4: Get an overall total by fiscal year and category
summary_overall_total_by_category_and_fiscal_year <- contract_spending_by_date %>%
  group_by(d_most_recent_category, d_fiscal_year_short) %>%
  summarise(
    total = sum(d_daily_contract_value, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    d_fiscal_year = convert_start_year_to_fiscal_year(d_fiscal_year_short)
  ) %>%
  select(d_most_recent_category, d_fiscal_year, total)


# Summary by owner_org (functions) ========================

# For each owner org, get a list of the top n companies

# Define a reusable function
get_summary_overall_total_by_vendor_by_owner <- function(owner_org) {
  
  # Thanks to
  # https://stackoverflow.com/a/46763370/756641
  # Note: this uses the same overall top_n_vendors calculated previously (based on average annual spending above the summary threshold amount).
  output <- contract_spending_by_date %>%
    filter(owner_org == !!owner_org) %>%
    filter(d_vendor_name %in% top_n_vendors) %>%
    group_by(d_vendor_name) %>%
    summarise(
      overall_total = sum(d_daily_contract_value)
    ) %>%
    arrange(desc(overall_total))
    
  return(output)
  
}

# Reusable function to get a per-fiscal year breakdown
# TODO: if this was called first, it'd be more efficient since it
# re-does the summary in the previous function above.
# In that case, you could do this first, and then sum it all up, 
# to get the results of the previous function.
get_summary_total_by_vendor_and_fiscal_year_by_owner <- function(owner_org) {
  
  top_n_vendors <- get_summary_overall_total_by_vendor_by_owner(owner_org) %>%
    pull(d_vendor_name)
  
  # Then, for those top n vendors, group by fiscal year
  output <- contract_spending_by_date %>%
    filter(owner_org == !!owner_org) %>%
    filter(d_vendor_name %in% top_n_vendors) %>%
    group_by(d_vendor_name, d_fiscal_year_short) %>%
    summarise(
      total = sum(d_daily_contract_value)
    ) %>%
    ungroup() %>%
    mutate(
      d_fiscal_year = convert_start_year_to_fiscal_year(d_fiscal_year_short)
    ) %>%
    select(d_vendor_name, d_fiscal_year, total)
  
  return(output)
  
}

# Get a category summary by department or agency
get_summary_total_by_category_by_owner_org <- function(owner_org) {
  
  output <- contract_spending_by_date %>%
    filter(owner_org == !!owner_org) %>%
    group_by(d_most_recent_category) %>%
    summarise(
      total = sum(d_daily_contract_value)
    ) %>%
    ungroup() %>%
    select(d_most_recent_category, total) %>%
    mutate(
      percentage = total / sum(total)
    ) %>%
    arrange(desc(total))
  
  return(output)
  
}

# Get a fiscal year overall summary by department or agency
get_summary_total_by_fiscal_year_by_owner_org <- function(owner_org) {
  
  output <- contract_spending_by_date %>%
    filter(owner_org == !!owner_org) %>%
    group_by(d_fiscal_year_short) %>%
    summarise(
      total = sum(d_daily_contract_value)
    ) %>%
    ungroup() %>%
    mutate(
      d_fiscal_year = convert_start_year_to_fiscal_year(d_fiscal_year_short)
    ) %>%
    select(d_fiscal_year, total)
  
  return(output)
  
}



# Summary by owner_org ========================

# Create a list-column and provide calculations for each department
vendors_by_owner_org <- tibble(owner_org = owner_orgs)

# With thanks to
# https://jennybc.github.io/purrr-tutorial/index.html
vendors_by_owner_org <- vendors_by_owner_org %>%
    mutate(
      summary_overall_total_by_vendor = map(owner_org, get_summary_overall_total_by_vendor_by_owner),
      summary_total_by_vendor_and_fiscal_year = map(owner_org, get_summary_total_by_vendor_and_fiscal_year_by_owner),
      summary_total_by_fiscal_year_by_owner_org = map(owner_org, get_summary_total_by_fiscal_year_by_owner_org),
      summary_total_by_category_by_owner_org = map(owner_org, get_summary_total_by_category_by_owner_org),
      )



# For each of the top n vendors, get a per-fiscal year breakdown
get_summary_total_by_fiscal_year_by_vendor <- function(requested_vendor_name) {
  
  output <- contract_spending_by_date %>%
    filter(d_vendor_name == !!requested_vendor_name) %>%
    group_by(d_fiscal_year_short) %>%
    summarise(
      total = sum(d_daily_contract_value)
    ) %>%
    ungroup() %>%
    mutate(
      d_fiscal_year = convert_start_year_to_fiscal_year(d_fiscal_year_short)
    ) %>%
    select(d_fiscal_year, total)
  
  return(output)
}

# For each of the top n vendors, get a per-fiscal year breakdown
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
    select(owner_org, d_fiscal_year, total)
  
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
    arrange(desc(total))
  
  return(output)
}

# Summary by vendor
summary_vendors = tibble(vendor = top_n_vendors)

# Thanks to
# https://stackoverflow.com/a/26003971/756641
# For the concatenation below to include the "overall years" file suffix
summary_vendors <- summary_vendors %>%
  mutate(
    summary_by_fiscal_year = map(vendor, get_summary_total_by_fiscal_year_by_vendor),
    summary_by_fiscal_year_and_owner_org = map(vendor, get_summary_total_by_fiscal_year_and_owner_org_by_vendor),
    !!str_c("summary_by_category", "_", summary_overall_years_file_suffix) := map(vendor, get_summary_total_by_category_by_vendor),
  )


# Get a summary for each of the industry categories
industry_categories <- contracts %>%
  select(category) %>%
  distinct() %>%
  arrange(category) %>%
  pull(category)


# Export CSV files of summary tables =============

# Export CSV files of the summary tables
if(option_update_summary_csv_files) {
  
  summary_overall_total_by_vendor %>% 
    mutate(
      # For CSV purposes, at the very end, round numbers to two decimal points
      overall_total = round(overall_total, digits = 2)
    ) %>%
    write_csv(str_c("data/out/s01_summary_overall_total_by_vendor_", summary_overall_years_file_suffix, ".csv"))
  
  summary_total_by_vendor_and_fiscal_year %>% 
    mutate(
      # TODO: determine how to make this a reusable function later.
      total = round(total, digits = 2)
    ) %>%
    write_csv("data/out/s02_summary_total_by_vendor_and_fiscal_year.csv")
  
  summary_overall_total_by_category %>% 
    mutate(
      # TODO: determine how to make this a reusable function later.
      overall_total = round(overall_total, digits = 2)
    ) %>%
    write_csv(str_c("data/out/s03_summary_overall_total_by_category_", summary_overall_years_file_suffix, ".csv"))
  
  summary_overall_total_by_category_and_fiscal_year %>% 
    mutate(
      # TODO: determine how to make this a reusable function later.
      total = round(total, digits = 2)
    ) %>%
    write_csv("data/out/s04_summary_total_by_category_and_fiscal_year.csv")
  
  
  # If requested, first delete the vendors/ and departments/ folders recursively
  # See exports.R for more details
  remove_existing_summary_folders()
  
  # Make per-owner org output directories, if needed
  create_summary_folders(output_department_path, owner_orgs)
  
  # List-based per-owner summaries, from
  # vendors_by_owner_org
  summary_overall_total_by_vendor_paths <- str_c(output_department_path, vendors_by_owner_org$owner_org, "/", "summary_overall_total_by_vendor_", summary_overall_years_file_suffix, ".csv")
  pwalk(list(vendors_by_owner_org$summary_overall_total_by_vendor, summary_overall_total_by_vendor_paths), write_csv)
  
  # List-based per-owner, by year summaries
  # also from vendors_by_owner_org
  summary_total_by_vendor_and_fiscal_year_paths <- str_c(output_department_path, vendors_by_owner_org$owner_org, "/", "summary_total_by_vendor_and_fiscal_year", ".csv")
  pwalk(list(vendors_by_owner_org$summary_total_by_vendor_and_fiscal_year, summary_total_by_vendor_and_fiscal_year_paths), write_csv)
  
  # Per-fiscal year summaries by owner org
  pwalk(
    list(
      vendors_by_owner_org$summary_total_by_fiscal_year_by_owner_org,
      str_c(output_department_path, vendors_by_owner_org$owner_org, "/", "summary_total_by_fiscal_year_by_owner_org", ".csv")
    ), 
    write_csv)
  
  pwalk(
    list(
      vendors_by_owner_org$summary_total_by_category_by_owner_org,
      str_c(output_department_path, vendors_by_owner_org$owner_org, "/", "summary_total_by_category_by_owner_org_", summary_overall_years_file_suffix, ".csv")
    ), 
    write_csv)
  
  
  # Per-vendor summaries
  # Make directories if needed
  create_summary_folders(output_vendor_path, summary_vendors$vendor)
  
  # Export vendor summaries using the reusable function
  export_summary(summary_vendors, output_vendor_path)
  
  
  
  
  # Note: temporary for manual vendor name normalization work.
  # TODO: remove this later.
  vendor_names %>%
    write_csv("data/testing/tmp_vendor_names.csv")
  
  # TODO: remove this later.
  contract_descriptions_object_codes %>%
    write_csv("data/testing/tmp_descriptions_object_codes.csv")
  
  # TODO: remove this later
  contracts %>%
    relocate(
      d_reference_number,
      vendor_name,
      d_clean_vendor_name,
      d_normalized_vendor_name
    ) %>%
    write_csv("data/testing/tmp_contracts_clean_names.csv")
  
}

run_end_time <- now()
paste("Start time was:", run_start_time)
paste("End time was:", run_end_time)


# TESTING (2022-04-12)

# contracts %>% count(owner_org, sort=TRUE)
# 
# sum(is.na(contracts$reference_number))
# sum(is.na(contracts$reporting_period))
# 
# ggplot(contracts) + 
#   geom_bar(mapping = aes(x = reporting_period))


# TESTING (2022-04-13)

# sum(is.na(contracts$d_reporting_period))
# 
# ggplot(contracts) + 
#   geom_bar(mapping = aes(x = d_reporting_period))
# 
# ggplot(contracts) + 
#   geom_bar(mapping = aes(x = d_reporting_year))
# 
# contracts %>%
#   filter(is.na(d_reporting_year)) %>%
#   select(reference_number, vendor_name, owner_org, contract_date, d_reporting_period, d_reporting_year, everything()) #%>%
#   #count(owner_org, sort=TRUE)
# 
# sum(is.na(contracts$reference_number))
# 
# contracts %>% count(reference_number, sort=TRUE)
# contracts %>% count(d_reference_number, sort=TRUE)
# 
# contracts %>% 
#   get_dupes(d_reference_number)
# 
# 
# contracts %>%
#   select(vendor_name, owner_org, contract_date, contract_period_start, d_start_date, everything())
# 
# sum(is.na(contracts$contract_period_start))
# sum(is.na(contracts$contract_date))
# sum(is.na(contracts$d_start_date))
# 
# 
# contracts %>%
#   select(contract_date, contract_period_start, delivery_date, d_start_date, d_end_date, vendor_name, owner_org, everything())


# contracts %>% 
#   filter(contract_value > 1000000) %>%
#   filter(owner_org == "ssc-spc") %>%
#   relocate(d_reference_number, vendor_name, d_start_date, d_end_date, contract_value, original_value, amendment_value, procurement_id, description_en) %>%
#   sample_n(20) #%>%
#   #write_csv("data/testing/2022-04-13-sample-contracts.csv")


# Testing (2022-04-14)

# contracts %>% relocate(d_reference_number, d_amendment_group_id, d_is_amendment, d_number_of_amendments, d_amendment_via) %>% View()


# Testing (2022-04-18)

# contracts %>% select(vendor_name, d_clean_vendor_name, d_normalized_vendor_name, d_vendor_name) %>% View()


# Testing (2022-06-23)

# # Review for repeated amendment group IDs / incomplete grouping of related amendments
# # Note: come back to this and tweak the grouping parameters.
# contract_spending_overall %>%
#   select(owner_org, d_vendor_name, d_amendment_group_id, d_overall_contract_value, d_description_en, d_economic_object_code, d_most_recent_category, d_overall_start_date, d_overall_end_date, d_daily_contract_value) %>%
#   distinct() %>%
#   arrange(desc(d_overall_contract_value)) %>%
#   slice_head(n = 100) %>%
#   View()
#   #write_csv(str_c("data/testing/", today(), "-contracts-spending-overall-largest.csv"))
# # 
# # Specific large-scale vendors for testing purposes
# tmp_key_vendors = c(
#   "BGIS",
#   "SIGNATURE SUR LE SAINT LAURENT"
#   )
# 
# tmp_key_reference_numbers <- contract_spending_overall %>%
#   select(owner_org, d_vendor_name, d_amendment_group_id, d_overall_contract_value, d_description_en, d_economic_object_code, category, d_overall_start_date, d_overall_end_date, d_daily_contract_value) %>%
#   distinct() %>%
#   filter(d_vendor_name %in% tmp_key_vendors) %>%
#   arrange(desc(d_overall_contract_value)) %>%
#   slice_head(n = 30) %>%
#   pull(d_amendment_group_id)
# 
# contracts %>%
#   filter(d_amendment_group_id %in% tmp_key_reference_numbers) %>%
#   arrange(contract_date) %>%
#   select(
#     d_reference_number,
#     d_vendor_name,
#     d_amendment_group_id,
#     d_number_of_amendments,
#     procurement_id,
#     original_value,
#     starts_with("d_"), everything()
#     ) %>%
#   #View()
#   write_csv(str_c("data/testing/", today(), "-contracts-key-amendment-testing.csv"))
# 

# 
# contract_spending_overall %>%
#   select(owner_org, d_amendment_group_id, d_overall_contract_value, d_vendor_name, d_description_en, d_economic_object_code, category, d_overall_start_date, d_overall_end_date, d_daily_contract_value) %>%
#   distinct() %>%
#   arrange(desc(d_overall_contract_value)) %>% 
#   filter(is.na(d_economic_object_code)) %>% 
#   #View()
#   write_csv(str_c("data/testing/", today(), "-contracts-without-economic-object-codes.csv"))
# 
# # Temporarily exclude DND
# contract_spending_overall %>%
#   select(owner_org, d_amendment_group_id, d_overall_contract_value, d_vendor_name, d_description_en, d_economic_object_code, category, d_overall_start_date, d_overall_end_date, d_daily_contract_value, comments_en, additional_comments_en) %>%
#   distinct() %>%
#   arrange(desc(d_overall_contract_value)) %>% 
#   filter(is.na(d_economic_object_code)) %>% 
#   filter(owner_org != "dnd-mdn") %>% 
#   #View()
#   write_csv(str_c("data/testing/", today(), "-contracts-without-economic-object-codes-exclude-dnd.csv"))
# 
# # Filter for just more recent contracts
# contract_spending_overall %>%
#   select(owner_org, d_amendment_group_id, d_overall_contract_value, d_vendor_name, d_description_en, d_economic_object_code, category, d_overall_start_date, d_overall_end_date, d_daily_contract_value) %>%
#   distinct() %>%
#   arrange(desc(d_overall_contract_value)) %>% 
#   filter(is.na(d_economic_object_code)) %>% 
#   filter(d_overall_end_date > "2017-03-31") %>% 
#   #View()
#   write_csv(str_c("data/testing/", today(), "-contracts-without-economic-object-codes-recent.csv"))
# 
# # Filter for just more recent contracts, and only include vendor + description
# contract_spending_overall %>%
#   select(owner_org, d_amendment_group_id, d_overall_contract_value, d_vendor_name, d_description_en, d_economic_object_code, category, d_overall_start_date, d_overall_end_date, d_daily_contract_value) %>%
#   distinct() %>%
#   arrange(desc(d_overall_contract_value)) %>%
#   filter(is.na(d_economic_object_code)) %>%
#   filter(d_overall_end_date > "2017-03-31") %>%
#   select(owner_org, d_description_en) %>%
#   distinct() %>%
#   #View()
#   write_csv(str_c("data/testing/", today(), "-contracts-without-economic-object-codes-recent-abridged.csv"))

# Testing (2022-06-29)

# contract_spending_overall %>%
#   filter(
#     d_overall_start_date >= ymd(str_c(summary_start_fiscal_year_short,"04","01")),
#     d_overall_start_date <= ymd(str_c(summary_end_fiscal_year_short,"03","31")),
#   )  %>%
#   select(d_description_en, category) %>%
#   distinct() %>%
#   arrange(category) %>%
#   #View()
#   write_csv(str_c("data/testing/tmp-", today(), "-description-category-matching.csv"))

# contract_spending_overall %>%
#   filter(
#     d_overall_start_date >= ymd(str_c(summary_start_fiscal_year_short,"04","01")),
#     d_overall_start_date <= ymd(str_c(summary_end_fiscal_year_short,"03","31")),
#   )  %>%
#   select(category, d_description_en) %>%
#   distinct() %>%
#   arrange(category) %>%
#   filter(!is.na(category)) %>%
#   #View()
#   bind_rows(description_matching) %>%
#   arrange(category) %>%
#   distinct() %>%
#   #View()
#   # Note: overwrites the existing description matching file (!)
#   write_csv(description_matching_file)

# Testing (2022-06-30)

# contract_spending_overall %>% 
#   filter(is.na(category)) %>% 
#   arrange(desc(contract_value)) %>% 
#   View()
# 
# 
# contract_spending_overall %>% 
#   filter(is.na(category)) %>% 
#   filter(
#     d_overall_end_date >= ymd(str_c(summary_start_fiscal_year_short,"04","01")),
#     d_overall_start_date <= ymd(str_c(summary_end_fiscal_year_short,"03","31"))
#   ) %>%
#   arrange(desc(d_overall_contract_value)) %>% 
#   select(d_vendor_name, d_overall_contract_value, d_most_recent_description_en, d_amendment_group_id, d_overall_start_date, d_overall_end_date) %>%
#   distinct() %>%
#   #View()
#   write_csv(str_c("data/testing/tmp-", today(), "-description-category-matching.csv"))
# 
# contracts %>% 
#   filter(is.na(category)) %>% 
#   arrange(desc(contract_value)) %>% 
#   View()


# Testing (2022-07-04)

# contracts %>%
#   filter(d_economic_object_code == "3252") %>%
#   arrange(desc(contract_value)) %>%
#   select(owner_org, d_vendor_name, contract_value, d_description_en, comments_en, additional_comments_en, d_start_date, d_end_date, d_amendment_group_id, procurement_id, original_value, d_economic_object_code, starts_with("category")) %>%
#   View()
# 
# Export of the current state for reviewing without parsing runs:
# 
# contracts %>%
#   write_csv(str_c("data/testing/tmp-", today(), "-contracts.csv"))
# 
# contract_spending_overall %>%
#   write_csv(str_c("data/testing/tmp-", today(), "-contract-spending-overall.csv"))

# Testing (2022-07-05)

# vendor_name_to_look_up <- "AMAZON"
# 
# contracts %>%
#   filter(str_detect(d_vendor_name, vendor_name_to_look_up)) %>%
#   select(d_vendor_name, d_description_en, owner_org, contract_value) %>%
#   arrange(d_vendor_name) %>%
#   View()
# 
# contracts %>%
#   filter(str_detect(d_vendor_name, vendor_name_to_look_up)) %>%
#   select(d_vendor_name) %>%
#   arrange(d_vendor_name) %>%
#   View()
# 
# # See vendor-normalization.R for more

# # Procurement ID mixups (in which departments mistakenly switched the procurement_id and reference_number values)
# contracts %>%
#   filter(str_detect(procurement_id, "-Q")) %>%
#   arrange(desc(contract_value)) %>%
#   select(owner_org, d_vendor_name, procurement_id, reference_number, contract_value, d_description_en, comments_en, additional_comments_en, d_start_date, d_end_date, d_amendment_group_id, original_value, d_economic_object_code) %>%
#   slice_sample(n = 40) %>%
#   write_csv(str_c("data/testing/", today(), "-sample-contracts-procurement-id-mixup.csv"))
# 
# # Procurement ID "appendages" (suffixes at the end with e.g. the initials of the contracting officer at the time)
# example_contracts_appendages <- contracts %>%
#   filter(str_detect(procurement_id, "/")) %>%
#   filter(contract_value > 100000) %>%
#   filter(d_start_date > "2017-01-01") %>%
#   arrange(desc(contract_value)) %>%
#   select(owner_org, d_vendor_name, procurement_id, reference_number, contract_value, d_description_en, comments_en, additional_comments_en, d_start_date, d_end_date, d_amendment_group_id, original_value, d_economic_object_code) %>%
#   slice_sample(n = 40)
# 
# contracts %>%
#   filter(d_vendor_name == "D H PARTNERSHIP") %>%
#   arrange(desc(contract_value)) %>%
#   select(owner_org, d_vendor_name, procurement_id, reference_number, contract_value, d_description_en, comments_en, additional_comments_en, d_start_date, d_end_date, d_amendment_group_id, original_value, d_economic_object_code) %>%
#   bind_rows(example_contracts_appendages) %>%
#   write_csv(str_c("data/testing/", today(), "-sample-contracts-procurement-id-suffixes.csv"))

# # Load these back in for testing
# example_contracts <- read_csv("data/testing/2022-07-05-sample-contracts-procurement-id-mixup.csv")
# 
# example_contracts %>%
#   mutate(
#     d_procurement_id = case_when(
#       str_detect(procurement_id, "\\d{4}-\\d{4}-Q\\d-") ~ reference_number,
#       TRUE ~ procurement_id
#     ),
#     d_reference_number = case_when(
#       str_detect(procurement_id, "\\d{4}-\\d{4}-Q\\d-") ~ procurement_id,
#       TRUE ~ reference_number
#     )
#   ) %>%
#   View()

# # Load these back in for testing
# example_contracts <- read_csv("data/testing/2022-07-05-sample-contracts-procurement-id-suffixes.csv")
# 
# example_contracts %>%
#   rename(
#     d_procurement_id = "procurement_id"
#   ) %>%
#   mutate(
#     d_procurement_id_suffix_reversed_id = stri_reverse(d_procurement_id),
#     d_procurement_id_suffix_pos = str_locate(d_procurement_id_suffix_reversed_id, "/")[,"start"],
#     d_procurement_id_suffix_remainder = str_sub(d_procurement_id_suffix_reversed_id, start = d_procurement_id_suffix_pos + 1L),
#     d_procurement_id = case_when(
#       d_procurement_id_suffix_pos <= 4 ~ stri_reverse(d_procurement_id_suffix_remainder),
#       TRUE ~ d_procurement_id
#     )
#   ) %>%
#   relocate(owner_org, d_vendor_name, starts_with("d_procurement_id")) %>%
#   select(! starts_with("d_procurement_id_suffix")) %>%
#   View()

# Testing (2022-07-06)

# tibble(parent_company = top_n_vendors) %>%
#   write_csv(str_c("data/testing/", today(), "-vendors-above-annual-threshold.csv"))
# 
# contracts %>%
#   select(d_vendor_name) %>%
#   distinct() %>%
#   arrange(d_vendor_name) %>%
#   write_csv(str_c("data/testing/", today(), "-all-vendors.csv"))

# Testing (2022-07-11)

# contract_spending_by_date %>%
#   filter_by_summary_type("core") %>%
#   filter_vendors_if_required(TRUE) %>%
#   View()
# 
# contracts %>% 
#   filter(d_vendor_name == "VANCOUVER SHIPYARDS") %>% 
#   arrange(desc(contract_value )) %>% 
#   select(d_reference_number, d_procurement_id, contract_value, everything()) %>% 
#   View()

# contracts %>%
#   filter(d_vendor_name == "CAE") %>%
#   filter_by_summary_type("core") %>%
#   arrange(desc(contract_value )) %>%
#   select(d_reference_number, d_procurement_id, contract_value, everything()) %>%
#   View()

# contracts %>% 
#   select(owner_org, owner_org_title) %>% 
#   distinct() %>% 
#   arrange(owner_org) %>% 
#   write_csv(str_c("data/testing/tmp-", today(), "-owner-orgs.csv"))
