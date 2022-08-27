# Load libraries and helper functions
source("lib/helpers.R")
source("lib/amendments.R")
source("lib/vendors.R")

# Set to TRUE to update the vendor normalization table
option_update_vendor_csv <- TRUE

vendor_names <- read_csv(
  "data/testing/tmp_vendor_names.csv",
)

vendor_names <- vendor_names %>%
  select(d_clean_vendor_name, d_normalized_vendor_name, d_vendor_name) %>%
  distinct() %>%
  arrange(d_clean_vendor_name)

vendor_name_to_look_up <- "AMAZON"

# Search for a specific keyword in the list of all (clean) vendor names.
# Thanks to
# https://stackoverflow.com/a/24821141 and
# https://stackoverflow.com/a/40233929
vendor_names %>%
  filter(str_detect(d_vendor_name, vendor_name_to_look_up)) %>%
  select(d_vendor_name) %>%
  distinct() %>%
  rename(
    company_name = d_vendor_name
  ) %>%
  mutate(
    parent_company = clean_vendor_names(vendor_name_to_look_up)
  ) %>%
  relocate(parent_company, company_name) %>%
  arrange(company_name) %>%
  write_csv("data/testing/tmp-vendor-exports.csv")
  # Note: to review manually before adding to the vendor matching CSV

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

# vendors_to_add <- tribble(
#   ~parent_company, ~company_name,
#   "LEONARDO", "LEONARDO MW",
#   "LEONARDO", "LEONARDO UK",
#   "LEONARDO", "LEONARDO DRS",
#   
# )


# # Update the vendor matching CSV file
# if(option_update_vendor_csv) {
#   vendor_matching %>% 
#     filter(parent_company != company_name) %>%
#     distinct() %>% 
#     arrange(parent_company, company_name) %>%
#     write_csv(vendor_matching_file)
# }



# Similar to the above, but using contracts data loaded in the environment:
vendor_name_to_look_up <- str_to_upper("COMPUTACEN")

look_up_vendor_name_and_save_tmp_output <- function(vendor_name_to_look_up, normalized_name = NULL) {
  
  vendor_name_to_look_up <- str_to_upper(vendor_name_to_look_up)
  
  if(is.null(normalized_name)) {
    normalized_name <- vendor_name_to_look_up
  }
  
  normalized_name <- str_to_upper(normalized_name)
  
  contracts %>%
    filter(str_detect(d_vendor_name, !!vendor_name_to_look_up)) %>%
    select(d_vendor_name) %>%
    distinct() %>%
    rename(
      company_name = d_vendor_name
    ) %>%
    mutate(
      parent_company = clean_vendor_names(!!normalized_name)
    ) %>%
    relocate(parent_company, company_name) %>%
    arrange(company_name) %>%
    write_csv("data/testing/tmp-vendor-exports.csv")
  
}

# Run the lookup and optionally provide the already-normalized name if it's already in the matching table
# look_up_vendor_name_and_save_tmp_output("qm", "QM Environmental")
look_up_vendor_name_and_save_tmp_output("9053 9776", "L Agence")



# Once reviewed, load this back into the vendor normalization table
new_vendor_matching_rows <- read_csv("data/testing/tmp-vendor-exports.csv")

vendor_matching <- vendor_matching %>%
  bind_rows(new_vendor_matching_rows)

if(option_update_vendor_csv) {
  regenerate_vendor_normalization_csv(FALSE)
}
