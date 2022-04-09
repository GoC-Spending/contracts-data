# Load exploratory libraries
library(tidyverse)
library(lubridate)
library(skimr)
library(janitor)
library(purrr)

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
  "data/source/2022-03-24-contracts.csv",
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

