# Helper functions for analyzing Government of Canada
# Proactive Disclosure of Contracts Data

source("lib/_libraries.R")

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
  indigenous_business = col_character(),
  indigenous_business_excluding_psib = col_character(), # Y/N
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

# Downloads the contracts CSV file from the open government website if it hasn't already been downloaded today.
# Thanks to
# https://github.com/lchski/change-in-the-ranks-analysis/blob/main/load/download-announcement-pages.R#L4-L47
get_contracts_csv_locally_or_from_url <- function(contract_col_types) {
  url <- "https://open.canada.ca/data/dataset/d8f85d91-7dec-4fd1-8055-483b77225d8b/resource/fac950c0-00d5-4ec1-a4d3-9cbebf98a305/download/contracts.csv"
  local_path <- str_c("data/source/", today(), "-contracts.csv")
  
  add_log_entry("csv_date", today())
  
  file_is_already_downloaded <- file_exists(local_path)
  
  if (! file_is_already_downloaded) {
    print("Starting to download file.")
    add_log_entry("start_csv_download")
    add_log_entry("csv_source", "remote")
    
    # Thanks to
    # https://stackoverflow.com/a/35283374/756641
    options(timeout=2400) # 40 minutes
    # TODO: Add tryCatch error handling here.
    download.file(url, local_path)
    
    add_log_entry("finish_csv_download")
  } else {
    add_log_entry("csv_source", "pre-downloaded")
  }
  
  # Import the CSV file
  contracts <- read_csv(
    local_path,
    col_types = contract_col_types
  ) %>%
    clean_names()
  
  return(contracts)
  
}



# Generate the matching quarter ("Q3") from a given date ("2018-02-03")
get_quarter_from_date <- function(date) {
  
  # Quick check that the input date is valid:
  date <- ymd(date)
  
  # Months: 
  #  April to June = 04 to 06 = Q1
  #  July to September = 07 to 09 = Q2
  #  October to December = 10 to 12 = Q3
  #  January to March = 01 to 03 = Q4
  
  quarter <- case_when(
    month(date) <= 3 ~ "Q4",
    month(date) <= 6 ~ "Q1",
    month(date) <= 9 ~ "Q2",
    month(date) <= 12 ~ "Q3",
    TRUE ~ as.character(NA)
  )
  
  return(quarter)
}



# Generate the matching "short" fiscal year ("2021", the year the FY started in) 
# from a given date
get_short_fiscal_year_from_date <- function(date) {
  
  # Quick check that the input date is valid:
  date <- ymd(date)
  quarter <- get_quarter_from_date(date)
  
  year <- case_when(
    quarter == "Q4" ~ year(date) - 1, # FY started the previous year
    quarter %in% c("Q1", "Q2", "Q3") ~ year(date), # FY started this year
    TRUE ~ NA_real_
  )
  
  return(year)
  
}

# Create a typical fiscal year ("2021-2022") from a start year ("2021")
convert_start_year_to_fiscal_year <- function(start_year) {
  
  end_year <- as.integer(start_year) + 1
  return(str_c(start_year, "-", end_year))
  
}

# Take a fiscal year ("2021-2022")and output an integer start year (2021)
convert_fiscal_year_to_start_year <- function(fiscal_year) {
  
  # Extracts a 4-digit number; this works given the consistent reporting period format.
  year <- as.integer(str_extract(fiscal_year, "(\\d{4})"))
  
  year
  
}

# Generate the matching fiscal year ("2021-2022") from a given date
get_fiscal_year_from_date <- function(date) {
  
  start_year <- get_short_fiscal_year_from_date(date)
  return(convert_start_year_to_fiscal_year(start_year))
  
}



# Generate the matching reporting period ("2021-2022-Q3") from a given date
get_reporting_period_from_date <- function(date) {
  return(str_c(get_fiscal_year_from_date(date), "-", get_quarter_from_date(date)))
}


# Extract a reporting period ("2021-2022-Q3") from the "reference_number"
# entries in the dataset e.g. "C-2021-2022-Q2-00024"
get_reporting_period_from_reference_number <- function(reference_number) {
  
  # 4 digits, a dash, 4 digits, a dash, then a Q and another digit (e.g. "2021-2022-Q2")
  reporting_period <- str_extract(reference_number, "(\\d{4}-\\d{4}-Q\\d{1})")
  
  return(reporting_period)
  
}

# Checks that a reporting period (in the source data) is actually valid
# and not "Q1", "A", etc.
is_valid_reporting_period <- function(reporting_period) {
  
  # 4 digits, a dash, 4 digits, a dash, then a Q and another digit (e.g. "2021-2022-Q2")
  # TODO confirm if there's a better way of doing this; currently repeats the regex above.
  # Note that this would still accept e.g. "1870-2020-Q6" and other logically invalid examples.
  expected_reporting_period <- str_extract(reporting_period, "(\\d{4}-\\d{4}-Q\\d{1})")

  return(as.logical(
    is.na(expected_reporting_period) == FALSE & 
      expected_reporting_period == reporting_period)
  )
  
}


# Get the starting year from a reporting period (or fiscal year)
# for grouping purposes.
get_starting_year_from_reporting_period <- function(reporting_period) {
  
  # Extracts a 4-digit number; this works given the consistent reporting period format.
  year <- str_extract(reporting_period, "(\\d{4})")
  
  return(year)
  
}

# Get the fiscal year ("2021-2022") from a reporting period.
get_fiscal_year_from_reporting_period <- function(reporting_period) {
  
  start_year <- get_starting_year_from_reporting_period(reporting_period)
  return(convert_start_year_to_fiscal_year(start_year))
  
}


# Helper function to look up examples by economic object code
get_examples_by_economic_object_code <- function(requested_economic_object_code) {
  
  requested_economic_object_code <- str_pad(requested_economic_object_code, 4, side = "left", pad = "0")
  
  output <- contracts %>%
    filter(d_economic_object_code == !!requested_economic_object_code) %>%
    select(d_vendor_name, d_description_en, owner_org) %>%
    slice_sample(n = 10)
  
  return(output)
}

get_vendor_filename_from_vendor_name <- function(vendor_name) {
  return(str_to_lower(str_replace_all(vendor_name, " ", "_")))
}


# Calculate overall duration
calculate_overall_duration <- function(df, remove_outliers = FALSE) {
  
  df <- df %>%
    mutate(
      duration_days = as.integer(d_overall_end_date - d_overall_start_date + 1),
      duration_years = duration_days / 365
    )
  
  if(remove_outliers == TRUE) {
    # To remove outlier durations (e.g. 112 years, 1600 years, etc.)
    df <- df %>%
      filter(duration_years < summary_maximum_duration_cutoff_years)
  }

  
  df
  
  
}


# Low-key log handling (in a dataframe!)

run_log <- tibble_row(
  time = as.character(now()), 
  name = "load_helper_scripts", 
  value = as.character("")
  )

add_log_entry <- function(name, value = "") {
  input <- tibble_row(
    time = as.character(now()), 
    name = as.character(name), 
    value = as.character(value)
  )
  
  # Thanks to
  # https://stackoverflow.com/a/32759849/756641
  run_log <<- run_log %>%
    bind_rows(input)
  
  input
}

export_log_entries <- function(location = "data/out/run_log.csv") {
  run_log %>%
    write_csv(location)
}


# Reload and save CSV file ======================

# To avoid formatting issues introduced by Google Sheets, LibreOffice, or Excel when Git-committing CSV files
regenerate_csv_file <- function(filename, na = "NA") {
  data <- read_csv(filename)
  
  # Rudimentary error check to avoid writing out again if it didn't load properly:
  if(count(data) > 0) {
    data %>%
      write_csv(filename, na = na)
  }
}


# Find helpers for contract calculation troubleshooting ====

find_selection_helper <- function(df) {
  
  df <- df %>%
    select(
      owner_org,
      d_reference_number,
      d_vendor_name,
      d_procurement_id,
      d_contract_value,
      d_original_original_value,
      d_reporting_period,
      d_start_date,
      d_end_date,
      d_economic_object_code,
      category,
      # d_it_subcategory_via_gsin,
      # d_it_subcategory_via_individual_keyword,
      d_it_subcategory,
      # d_description_en,
      d_description_comments_extended_lower,
      d_amendment_group_id,
      d_is_amendment,
      # d_number_of_amendments,
      d_amendment_via
      )
  
  df
  
}

find_contracts_by_d_amendment_group_id <- function(d_amendment_group_id) {
  
  contracts_individual_entries %>%
    filter(d_amendment_group_id == !!d_amendment_group_id) %>%
    find_selection_helper %>%
    arrange(owner_org, d_reporting_period)
  
}

find_contracts_by_d_vendor_name <- function(d_vendor_name) {
  
  contracts_individual_entries %>%
    filter(d_vendor_name == !!d_vendor_name) %>%
    find_selection_helper %>%
    arrange(owner_org, d_reporting_period)
  
}

find_contracts_by_economic_object_code <- function(d_economic_object_code) {
  
  contracts_individual_entries %>%
    filter(d_economic_object_code == !!d_economic_object_code) %>%
    find_selection_helper %>%
    arrange(desc(d_contract_value))
  
}

# Note: currently uses contract_spending_overall_ongoing to
# help troubleshoot the research_findings.R functions
find_overall_contracts_by_d_vendor_name <- function(d_vendor_name) {
  
  contract_spending_overall_ongoing %>%
    filter(d_vendor_name == !!d_vendor_name) %>%
    arrange(owner_org, d_overall_start_date)
  
}


# TESTING (2022-04-12)

# get_fiscal_year_from_date("2022-03-31")
# get_fiscal_year_from_date("2022-04-01")

# test_run <- tribble(
#   ~date,~reporting_period,
#   "2021-03-02",NA,
#   "2021-04-07",NA,
#   "2021-10-22",NA,
# )
# 
# test_run <- test_run %>%
#   mutate(
#     reporting_period = get_reporting_period_from_date(date)
#   )


# get_reporting_period_from_reference_number("C-2021-2022-Q2-00024")
# get_reporting_period_from_reference_number("C-2021-2022-Q3-361")


# TESTING (2022-04-13)

# test_run <- tribble(
#   ~reference_number,~reporting_period,~contract_date,
#   "C-2021-2022-Q2-00024",NA,NA,
#   "C-2015-2016-00371",NA,"2016-03-01",
#   "C-2021-2022-Q3-361",NA,NA,
# )
# 
# # 'names' attribute [1] must be the same length as the vector [0]
# test_run <- test_run %>%
#   mutate(
#     d_reporting_period = case_when(
#       !is.na(reporting_period) ~ reporting_period,
#       !is.na(get_reporting_period_from_reference_number(reference_number)) ~ get_reporting_period_from_reference_number(reference_number),
#       TRUE ~ get_reporting_period_from_date(contract_date)
#     )
#   )


# TESTING (2022-04-13)

# get_starting_year_from_reporting_period("2022-2013-Q3")
# get_fiscal_year_from_reporting_period("2022-2023-Q4")

# is_valid_reporting_period("2021-2022-Q2")
# is_valid_reporting_period("1870-2020-Q6")
# is_valid_reporting_period("Q4")


# # Testing (2022-08-28)
# 
# filename <- "data/categories/combined_descriptions_to_category.csv"
# data <- read_csv(filename)
# 
# data <- data %>%
#   arrange(category, d_description_en) %>%
#   select(category, d_description_en) %>%
#   distinct()
# 
# data %>%
#   write_csv("data/categories/descriptions_to_category.csv")
