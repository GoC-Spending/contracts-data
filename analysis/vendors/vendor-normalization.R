# Load libraries and helper functions
source("lib/helpers.R")
source("lib/amendments.R")
source("lib/vendors.R")

# Set to TRUE to update the vendor normalization table
option_update_vendor_csv <- FALSE

vendor_names <- read_csv(
  "data/testing/tmp_vendor_names.csv",
)

vendor_names <- vendor_names %>%
  select(d_clean_vendor_name, d_normalized_vendor_name, d_vendor_name) %>%
  distinct() %>%
  arrange(d_clean_vendor_name)


# If the contracts data is already loaded, you can search it as follows
# Thanks to
# https://stackoverflow.com/a/24821141 and
# https://stackoverflow.com/a/40233929
contract_spending_overall %>%
  filter(str_detect(d_vendor_name, "TOROMONT")) %>%
  select(d_vendor_name) %>%
  distinct() %>%
  rename(
    company_name = d_vendor_name
  ) %>%
  mutate(
    parent_company = "TOROMONT"
  ) %>%
  relocate(parent_company, company_name) %>%
  write_csv("data/testing/tmp-vendor-exports.csv")


# vendor_matching <- vendor_matching %>%
#   add_row(parent_company = "SIGNATURE SUR LE SAINT LAURENT",
#           company_name = "GROUPE SIGNATURE SUR LE"
#   ) %>%
#   add_row(parent_company = "GENERAL DYNAMICS",
#           company_name = "GD OTS C"
#   ) %>%
#   add_row(parent_company = "TOP ACES",
#           company_name = "TOP ACES FORMALLY DISCOVERY AIR"
#   ) %>%
#   add_row(parent_company = "LEONARDO",
#           company_name = "LEONARDO GERMANY"
#   ) %>%
#   add_row(parent_company = "LEONARDO",
#           company_name = "LEONARDO GERMANY FKA SELEX ES GMBH"
#   )

vendors_to_add <- tribble(
  ~parent_company, ~company_name,
  "LEONARDO", "LEONARDO MW",
  "LEONARDO", "LEONARDO UK",
  "LEONARDO", "LEONARDO DRS",
  
)


# Update the vendor matching CSV file
if(option_update_vendor_csv) {
  vendor_matching %>% 
    distinct() %>% 
    arrange(parent_company) %>%
    write_csv(vendor_matching_file)
}

