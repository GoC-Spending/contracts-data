# Load libraries and helper functions
source("lib/helpers.R")
source("lib/amendments.R")

# Start time
run_start_time <- now()
paste("Start time:", run_start_time)

# "data/source/2022-03-24-contracts.csv"
# "data/testing/2022-04-13-sample-contracts.csv"
contracts_data_source <- "data/source/2022-03-24-contracts.csv"

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


# Import the CSV file
contracts <- read_csv(
  contracts_data_source,
  col_types = contract_col_types
) %>%
  clean_names()


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


# Use the reporting period if it exists, otherwise get it from the reference number
# Store it in d_reporting_period (derived reporting period)
contracts <- contracts %>%
  mutate(
    d_reporting_period = case_when(
      is_valid_reporting_period(reporting_period) ~ reporting_period,
      !is.na(get_reporting_period_from_reference_number(reference_number)) ~ get_reporting_period_from_reference_number(reference_number),
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
    d_reference_number = str_c(owner_org, "-", reference_number)
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


# Find amendment groups based on procurement_id, or on start date + contract value 
contracts <- find_amendment_groups_v2(contracts)


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
