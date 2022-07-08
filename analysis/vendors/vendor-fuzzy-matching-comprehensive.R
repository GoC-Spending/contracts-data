# Load libraries and helper functions
source("lib/helpers.R")
source("lib/amendments.R")
source("lib/vendors.R")

# Compare the vendor matching table against the full list of vendors
# Note: this uses the extended list of vendor name "options", the
# company_name column in the vendor matching table
# as a starting point.

# Set to TRUE to update the vendor normalization table
option_update_vendor_csv <- TRUE

all_vendor_names <- read_csv(str_c("data/testing/2022-07-06-all-vendors.csv"))

all_vendor_names <- all_vendor_names %>%
  rename(
    "company_name" = d_vendor_name
  )

matched_vendors <- all_vendor_names %>%
  stringdist_inner_join(vendor_matching, by = "company_name", method="jw", distance_col = "distance", max_dist = 0.2)


matched_vendors %>%
  select(company_name.x, company_name.y, distance, parent_company) %>%
  filter(distance < 0.09) %>%
  arrange(parent_company, company_name.x) %>%
  write_csv(str_c("data/testing/tmp-", today(), "-vendor-fuzzy-matching-comprehensive.csv"))


# Reimport manually matching-flagged data
matching_results <- read_csv(str_c("data/testing/tmp-2022-07-08-vendor-fuzzy-matching-comprehensive.csv"))

matching_results <- matching_results %>%
  filter(!is.na(matching))

matching_results_part1 <- matching_results %>%
  select(company_name.x, parent_company) %>%
  rename("company_name" = company_name.x)

# Note: can't remember if these are already included in the vendor_matching table or vice-versa
matching_results_part2 <- matching_results %>%
  select(company_name.y, parent_company) %>%
  rename("company_name" = company_name.y)

vendor_matching <- vendor_matching %>%
  bind_rows(matching_results_part1) %>%
  bind_rows(matching_results_part2) %>%
  distinct()

if(option_update_vendor_csv) {
  regenerate_vendor_normalization_csv(FALSE)
}
