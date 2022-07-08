# Load libraries and helper functions
source("lib/helpers.R")
source("lib/amendments.R")
source("lib/vendors.R")

vendors_above_annual_threshold <- read_csv("data/testing/2022-07-06-vendors-above-annual-threshold.csv")

option_str_length_cutoff <- 20L

# Set to TRUE to update the vendor normalization table
option_update_vendor_csv <- TRUE

# Experiment 1:
# For vendor names that start with non-distinct components,
# switch them around to help with the Jaro-W calculation. 
# Note: function-ify this to be less cumbersome.
# vendors_above_annual_threshold_e1 <- vendors_above_annual_threshold %>%
#   mutate(
#     parent_company_adapted = case_when(
#       str_locate(parent_company, "UNIVERSITY OF ")[,"start"] == 1 ~ str_c(str_sub(parent_company, 15L), " ", str_sub(parent_company, 1L, 13L)),
#       str_locate(parent_company, "UNIVERSITE ")[,"start"] == 1 ~ str_c(str_sub(parent_company, 12L), " ", str_sub(parent_company, 1L, 10L)),
#       str_locate(parent_company, "CONSTRUCTION ")[,"start"] == 1 ~ str_c(str_sub(parent_company, 14L), " ", str_sub(parent_company, 1L, 12L)),
#       str_locate(parent_company, "UNITED STATES DEPARTMENT OF THE ")[,"start"] == 1 ~ str_c(str_sub(parent_company, 33L), " ", str_sub(parent_company, 1L, 31L)),
#       str_locate(parent_company, "CANADIAN ")[,"start"] == 1 ~ str_c(str_sub(parent_company, 10L), " ", str_sub(parent_company, 1L, 8L)),
#       TRUE ~ parent_company
#     )
#   )

#vendors_above_annual_threshold_e1 %>% arrange(desc(parent_company_adapted)) %>% View()

#vendors_above_annual_threshold_e1 %>% filter(str_detect(parent_company, "UNITED STATES")) %>% distinct() %>% View()

# vendors_above_annual_threshold_e1 <- vendors_above_annual_threshold_e1 %>%
#   mutate(
#     short_name = str_squish(str_sub(parent_company_adapted, 1L, option_str_length_cutoff))
#   )

# vendors_above_annual_threshold_no_spaces <- vendors_above_annual_threshold %>%
#   mutate(
#     parent_company = str_replace_all(parent_company, " ", "")
#   )

# # Fuzzy match this list against itself to look for close matches
# matched_vendors <- vendors_above_annual_threshold_e1 %>%
#   stringdist_inner_join(vendors_above_annual_threshold_e1, by = "short_name", method="jw", distance_col = "distance")
# 
# matched_vendors %>%
#   arrange(distance) %>%
#   filter(distance > 0) %>%
#   filter(distance < 0.1) %>%
#   #slice_sample(n = 50) %>%
#   arrange(short_name.x) %>%
#   distinct() %>%
#   select(starts_with("short_"), distance) %>%
#   View()

# matched_vendors %>%
#   filter(distance == 0) %>%
#   View()
# 
# matched_vendors %>%
#   filter(distance > 0) %>%
#   View()


# Experiment 2:
# Remove common keywords
# Then remove spaces
# Do matches
# and *then* do a fuzzy comparison

replace_common_keywords <- function(vendor_name) {
  
  # Adapted from clean_vendor_names in vendors.R
  # This represents frequently-misspelled or forgotten suffixes
  # or common (non-differentiating) words
  str <- list()
  str$pattern <- c(
    "UNIVERSITE",
    "UNIVERSITY",
    "OF",
    "CANADIAN",
    "CANADA",
    "QUEBEC",
    "BC",
    "ONTARIO",
    "CONSTRUCTION",
    "DEPARTMENT",
    "THE",
    "TECHNOLOGIES",
    "CONSULTING",
    "SOLUTIONS",
    "GROUP"
    
  )
  
  # Include a leading and trailing space for the suffixes below
  # on the rare chance that they form part of a company's actual name.
  str$pattern <- str_c(" ", str$pattern, " ")
  
  # Add a leading and trailing space to each string
  # As a simple way of accidentally removing "AB" etc. from the actual names of companies
  vendor_name <- str_c(" ", vendor_name, " ")
  
  # Replace with a space; use str_squish to remove extra spaces
  str$replacement <- rep(" ", length(str$pattern))
  
  # Thanks to
  # https://community.rstudio.com/t/replacing-multiple-patterns-via-str-replace/111642/4
  vendor_name <- reduce2(str$pattern, str$replacement, str_replace, .init = vendor_name)
  
  return(str_squish(vendor_name))
  
}


vendors_above_annual_threshold_e2 <- vendors_above_annual_threshold %>%
  mutate(
    short_name = replace_common_keywords(parent_company)
  )

# vendors_above_annual_threshold_e2 %>%
#   View()

matched_vendors <- vendors_above_annual_threshold_e2 %>%
  stringdist_inner_join(vendors_above_annual_threshold_e2, by = "short_name", method="jw", distance_col = "distance")

# matched_vendors %>%
#   arrange(distance) %>%
#   filter(distance > 0) %>%
#   filter(distance < 0.1) %>%
#   #slice_sample(n = 50) %>%
#   arrange(short_name.x) %>%
#   distinct() %>%
#   select(starts_with("short_"), distance) %>%
#   View()

# Filter down to a smaller set based on likely matches
# (without filtering, this ends up being an n*n length table)
matched_vendors <- matched_vendors %>%
  arrange(distance) %>%
  filter(distance > 0) %>%
  filter(distance < 0.1)

# Compensate for "duple" entries, like
# AES FUELS | AESFUELS
# AESFUELS | AES FUELS
matched_vendors <- matched_vendors %>%
  mutate(
    short_name_first = case_when(
      short_name.x < short_name.y ~ short_name.x,
      TRUE ~ short_name.y
    ),
    short_name_second = case_when(
      short_name.x < short_name.y ~ short_name.y,
      TRUE ~ short_name.x
    ),
    parent_company_first = case_when(
      parent_company.x < parent_company.y ~ parent_company.x,
      TRUE ~ parent_company.y
    ),
    parent_company_second = case_when(
      parent_company.x < parent_company.y ~ parent_company.y,
      TRUE ~ parent_company.x
    ),
  )

# matched_vendors_to_add <- matched_vendors %>% 
#   select(short_name_first, short_name_second) %>%
#   distinct()

# matched_vendors_to_add %>%
#   arrange(short_name_first) %>%
#   View()

matched_vendors_to_add <- matched_vendors %>% 
  select(parent_company_first, parent_company_second, short_name_first, short_name_second) %>%
  distinct()

# Adapted from vendor-normalization.R
matched_vendors_to_add <- matched_vendors_to_add %>%
  mutate(
    parent_company = parent_company_first,
    company_name = parent_company_second
  ) %>%
  select(parent_company, company_name) %>%
  arrange(parent_company) %>%
  write_csv(str_c("data/testing/tmp-", today(), "-vendor-fuzzy-matching.csv"))

# Re-import in after manually reviewing
# Note: if the date has changed the today() call here would be an issue
new_vendor_matching_rows <- read_csv(str_c("data/testing/tmp-", today(), "-vendor-fuzzy-matching.csv"))

vendor_matching <- vendor_matching %>%
  bind_rows(new_vendor_matching_rows)

if(option_update_vendor_csv) {
  regenerate_vendor_normalization_csv(FALSE)
}
