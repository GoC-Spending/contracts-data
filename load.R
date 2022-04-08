# Load exploratory libraries
library(tidyverse)
library(lubridate)
library(skimr)
library(janitor)
library(purrr)

# reference_number	procurement_id	vendor_name	vendor_postal_code	buyer_name	contract_date	economic_object_code	description_en	description_fr	contract_period_start	delivery_date	contract_value	original_value	amendment_value	comments_en	comments_fr	additional_comments_en	additional_comments_fr	agreement_type_code	trade_agreement	land_claims	commodity_type	commodity_code	country_of_vendor	solicitation_procedure	limited_tendering_reason	trade_agreement_exceptions	aboriginal_business	aboriginal_business_incidental	intellectual_property	potential_commercial_exploitation	former_public_servant	contracting_entity	standing_offer_number	instrument_type	ministers_office	number_of_bids	article_6_exceptions	award_criteria	socioeconomic_indicator	reporting_period	owner_org	owner_org_title

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
  commodity_type = col_character(),
  commodity_code = col_character(),
  country_of_vendor = col_character(),
  solicitation_procedure = col_character(),
  limited_tendering_reason = col_character(),
  trade_agreement_exceptions = col_character(),
  aboriginal_business = col_character(),
  aboriginal_business_incidental = col_character(),
  intellectual_property = col_character(),
  potential_commercial_exploitation = col_character(),
  former_public_servant = col_character(),
  contracting_entity = col_character(),
  standing_offer_number = col_character(),
  instrument_type = col_character(),
  ministers_office = col_character(),
  number_of_bids = col_number(),
  article_6_exceptions = col_character(),
  award_criteria = col_character(),
  socioeconomic_indicator = col_character(),
  reporting_period = col_character(),
  
  owner_org = col_character(),
  owner_org_title = col_character(),
  
)

contracts <- read_csv(
  "data/source/2022-03-24-contracts.csv",
  col_types = contract_col_types
) %>%
  clean_names()


