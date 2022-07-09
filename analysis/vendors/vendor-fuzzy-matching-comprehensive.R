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
option_add_threshold_vendors <- TRUE

all_vendor_names <- read_csv(str_c("data/testing/2022-07-08-all-vendors.csv"))

all_vendor_names <- all_vendor_names %>%
  rename(
    "company_name" = d_vendor_name
  )


# In order to match against company_name entries,
# re-add in the parent_company cells as company_name entries too
parent_company_entries <- vendor_matching %>%
  select(parent_company) %>%
  mutate(
    company_name = parent_company
  )

if(option_add_threshold_vendors) {
  threshold_vendors <- read_csv(str_c("data/testing/2022-07-08-vendors-above-annual-threshold.csv")) %>%
    mutate(
      company_name = parent_company
    )
  
  vendor_matching <- vendor_matching %>%
    bind_rows(threshold_vendors) %>%
    arrange(parent_company, company_name) %>%
    distinct()
}



# This causes duplicates, but they're removed again in the
# regenerate_vendor_normalization_csv function
vendor_matching <- vendor_matching %>%
  bind_rows(parent_company_entries) %>%
  arrange(parent_company, company_name) %>%
  distinct()

# Note: this can be time and memory-intensive.
matched_vendors <- all_vendor_names %>%
  stringdist_inner_join(vendor_matching, by = "company_name", method="jw", distance_col = "distance", max_dist = 0.2)


matched_vendors %>%
  select(company_name.x, company_name.y, distance, parent_company) %>%
  filter(distance < 0.09) %>%
  filter(distance != 0) %>%
  distinct() %>%
  arrange(parent_company, company_name.x) %>%
  write_csv(str_c("data/testing/tmp-", today(), "-vendor-fuzzy-matching-comprehensive-02.csv"))


# ###########
# Pause here to manually determine matching rows, with a "matching" column
# in a spreadsheet editor.

# Reimport manually matching-flagged data
matching_results <- read_csv(str_c("data/testing/tmp-2022-07-08-vendor-fuzzy-matching-comprehensive-02.csv"))

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

# Note: after the vendor_matching table is regenerated, to do another comparison, we'll need to re-run the full data analysis first to create a new set of all vendor names (to avoid re-manually confirming non-normalized names).
