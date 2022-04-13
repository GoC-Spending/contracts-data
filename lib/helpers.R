# Helper functions for analyzing Government of Canada
# Proactive Disclosure of Contracts Data

# Load exploratory libraries
library(tidyverse)
library(lubridate)
library(skimr)
library(janitor)
library(purrr)


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
  
  if(is.na(expected_reporting_period)) {
    return(FALSE)
  }
  return(as.logical(expected_reporting_period == reporting_period))
  
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