# Load the libraries
source("lib/helpers.R")
source("lib/vendors.R")

# Search input:
# redacted/caviardé
# (appears in the 2022-04-22 dataset)



# source_data_file_date is, for example, "2022-04-22"
find_vendor_in_specific_source_data <- function(source_data_file_date, vendor_name_to_look_up) {
  
  source_data_file <- str_c("data/source/", source_data_file_date, "-contracts.csv")
  
  contracts <- read_csv(
    source_data_file,
    col_types = contract_col_types
  ) %>%
    clean_names()
  
  
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
  
  contracts_matching <- contracts %>% 
    filter(str_detect(d_vendor_name, !!vendor_name_to_look_up))
  
  View(contracts_matching)
  
  contracts_matching
  
}


find_vendor_in_specific_source_data("2022-04-22", "REDACTED")
