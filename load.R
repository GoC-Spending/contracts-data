# Load libraries and helper functions
source("lib/helpers.R")

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



# To identify amendment groups, the approach is:
# same owner_org
# same vendor (once normalized)
# same procurement_id
# OR
# same owner_org
# same vendor (once normalized)
# original_value == contract_value of the original row(?)
# same start date (to confirm if derived values / closest year helps)

# *not* part of an existing amendment group

# New columns added
# d_is_amendment
# d_amendment_group_id
contracts <- contracts %>%
  mutate(
    d_is_amendment = FALSE,
    d_amendment_group_id = NA_character_,
  )

# Test 1
reference_numbers <- contracts %>% pull(d_reference_number)

# Create a temporary array that matches origin contract reference numbers
# to amendment reference numbers.
# This would be a nested array of different lengths if we were doing it in PHP!
amendment_groups <- tibble(origin_reference_number = character(), amendment_reference_number = character())

for (i in seq_along(reference_numbers)) {
  #output[[i]] <- median(df[[i]])
  #sprintf("ref: %s", contracts[[i]])
  print("=======")
  print(reference_numbers[[i]])
  
  # 1. Pull up the current contract
  current_contract <- contracts %>%
    filter(
      d_reference_number == reference_numbers[[i]]
    ) %>%
    slice_head()
  
  #print(current_contract$owner_org)
  
  #break
  
  # 2A. Find the matching contracts by procurement_id
  matching_reference_numbers_by_procurement_id <- contracts %>%
    filter(
      d_reference_number != current_contract$d_reference_number, # Don't re-select the same row
      owner_org == current_contract$owner_org,
      vendor_name == current_contract$vendor_name,
      procurement_id == current_contract$procurement_id,
      is.na(d_amendment_group_id), # Don't re-run existing amendments
    ) %>%
    pull(d_reference_number)
    # mutate(
    #   d_is_amendment = TRUE,
    #   d_amendment_group_id = current_contract$d_reference_number,
    # )
  
  print("By procurement ID:")
  print(matching_reference_numbers_by_procurement_id)
  
  # 2B. Find the matching contracts by original value and start date
  matching_reference_numbers_by_original_value_and_start_date <- contracts %>%
    filter(
      d_reference_number != current_contract$d_reference_number, # Don't re-select the same row
      owner_org == current_contract$owner_org,
      vendor_name == current_contract$vendor_name,
      original_value == current_contract$contract_value,
      d_start_date == current_contract$d_start_date, # Note: does normalizing the start date have unexpected consequences here? Do amendments sometimes have new, later, start dates?
      is.na(d_amendment_group_id), # Don't re-run existing amendments
    ) %>%
    pull(d_reference_number)
  # mutate(
  #   d_is_amendment = TRUE,
  #   d_amendment_group_id = current_contract$d_reference_number,
  # )
  
  print("By original value and start date:")
  print(matching_reference_numbers_by_original_value_and_start_date)
  
  # Merge results from both groups
  # Note: does this need to remove duplicate entries? Or is it fine via use of %in% below? UPDATE: yes, it helps to remove duplicate entries with unique() to avoid redundant entries in amendment_groups.
  # Note: this currently doesn't let us compare how many amendment groups were generated by each method. We could check that by selecting amendment groups and finding non-matching procurement_id values.
  matching_reference_numbers <- unique(c(matching_reference_numbers_by_procurement_id, matching_reference_numbers_by_original_value_and_start_date))
  
  # Update the amendment groups tibble (not used beyond this yet)
  amendment_groups <- amendment_groups %>% 
    add_row(
      origin_reference_number = current_contract$d_reference_number, 
      amendment_reference_number = matching_reference_numbers, # (character vector with, in some cases, multiple amendment reference numbers)
      )
  
  # 3. Update the corresponding rows
  # Note: not sure if there's a way to avoid the repetition below.
  contracts <- contracts %>%
    mutate(
      d_amendment_group_id = case_when(
        d_reference_number %in% matching_reference_numbers ~ current_contract$d_reference_number,
        TRUE ~ d_amendment_group_id, # Leave it at its previous value if it doesn't match.
      ),
      d_is_amendment = case_when(
        d_reference_number %in% matching_reference_numbers ~ TRUE,
        TRUE ~ d_is_amendment, # Same here, leave it at its previous value if it doesn't match.
      ),
    )
  
  # 4. Update the original contract to include the d_amendment_group_id
  # (but not a d_is_amendment flag)
  if(length(matching_reference_numbers) >= 1) {
    print("... updating amendment group ID to")
    print(current_contract$d_reference_number)
    contracts <- contracts %>%
      mutate(
        d_amendment_group_id = case_when(
          d_reference_number == current_contract$d_reference_number ~ current_contract$d_reference_number,
          TRUE ~ d_amendment_group_id,
        ),
      )
  }


}


contracts %>% relocate(d_reference_number, d_amendment_group_id, d_is_amendment) %>% View()

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

