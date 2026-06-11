# Load libraries and helper functions
source("lib/helpers.R")
source("lib/amendments.R")
source("lib/vendors.R")

# vendors_above_annual_threshold <- read_csv("data/testing/2022-07-06-vendors-above-annual-threshold.csv")
vendors_above_annual_threshold <- read_csv("data/testing/2026-06-09-vendors-above-annual-threshold.csv")

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

replace_common_keywords <- function(vendor_name, expanded_list = TRUE) {
  
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
  
  # 2026-06-10 add additional common words here
  if(expanded_list) {
    str$pattern <- c(
      str$pattern,
      "SERVICES", 
      "SYSTEMS", 
      "AND", 
      "OTTAWA", 
      "TECHNOLOGY", 
      "DIVISION", 
      "COMPANY", 
      "IN", 
      "COMMUNICATIONS", 
      "MANAGEMENT", 
      "INTERNATIONAL", 
      "INFORMATION", 
      "DE", 
      "GLOBAL", 
      "IT", 
      "CONSULTANTS", 
      "JOINT", 
      "RESOURCES", 
      "HUMAN", 
      "VENTURE", 
      "ASSOCIATES", 
      "GENERAL", 
      "SECURITY", 
      "BUSINESS", 
      "ENGINEERING", 
      "CANADA", 
      "ENERGY", 
      "FUEL", 
      "TORONTO", 
      "DU", 
      "SOCIETY", 
      "LES", 
      "PRODUCTS", 
      "HR", 
      "ENVIRONMENTAL", 
      "AVIATION", 
      "CENTRE", 
      "PROFESSIONAL", 
      "SYSTEM", 
      "SERVICE", 
      "ALBERTA", 
      "SOFTWARE", 
      "ON", 
      "RESEARCH", 
      "DEFENCE", 
      "INTEGRATED", 
      "ADVANCED", 
      "WORLD", 
      "MOTOR", 
      "INDUSTRIES", 
      "EQUIPMENT", 
      "ELECTRONICS", 
      "ATLANTIC", 
      "OF", 
      "AIR", 
      "TRAINING", 
      "SOLUTION", 
      "ENTERPRISE", 
      "GROUPE", 
      "COMMUNICATION", 
      "EDMONTON", 
      "STAFFING", 
      "SCIENCES", 
      "BUILDING", 
      "MARINE", 
      "NORTHERN", 
      "CALGARY", 
      "ET", 
      "FOR", 
      "NATIONAL", 
      "MULTINATIONAL", 
      "PARTNERSHIP", 
      "INSTITUTE", 
      "NETWORK", 
      "DES", 
      "AS", 
      "DEFENSE", 
      "AEROSPACE", 
      "KINGSTON", 
      "WINNIPEG", 
      "CONTRACTING", 
      "RESELLER", 
      "OFFICE", 
      "OR"
    )
  }

  
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


# Experiment 3

# Take the list of large vendors above
# Also use vendor_matching table

source_vendor_matching <- vendor_matching

source_vendor_matching_canonical_names <- vendor_matching %>% 
  select(parent_company) %>% 
  mutate(
    company_name = parent_company
  ) %>% 
  distinct()

source_vendor_matching <- source_vendor_matching %>% 
  bind_rows(source_vendor_matching_canonical_names) %>% 
  distinct() %>% 
  arrange(parent_company, company_name)

# Source vendor matching includes parent/child rows for the canonical name
# which isn't included in the original vendor matching CSV
# but is useful here.

source_vendor_matching <- source_vendor_matching %>% 
  rename(
    canonical_name = "parent_company",
    variation_name = "company_name"
  ) 

# replace_common_keywords
# We'll do this here so we can still eventually match back to canonical_name values
source_vendor_matching <- source_vendor_matching %>% 
  mutate(
    variation_name = replace_common_keywords(variation_name)
  )


source_vendor_matching_variations <- source_vendor_matching %>% 
  select(variation_name) %>% 
  distinct()

# Experiment 3b. variation single words

source_vendor_matching_variations <- source_vendor_matching_variations %>% 
  mutate(
    variation_name_words = variation_name
  ) %>% 
  separate_longer_delim(variation_name_words, delim = " ")


vendors_above_annual_threshold_e3 <- vendors_above_annual_threshold %>% 
  rename(
    variation_name = "parent_company"
  ) %>% 
  mutate(
    variation_name_words = variation_name
  ) %>% 
  separate_longer_delim(variation_name_words, delim = " ")


matched_vendor_variation_name_words <- source_vendor_matching_variations %>%
  stringdist_inner_join(vendors_above_annual_threshold_e3, by = "variation_name_words", method="jw", distance_col = "distance")


# Steps: 
# 1. exclude highly common words
# 2. set a distance threshold and filter by it
# 3. merge back to just canonical_name and variation names
# 4. manually confirm in a Google spreadsheet

threshold_highly_common_percentage = 0.03
threshold_distance_maximum = 0.09

# matched_vendor_variation_name_words %>% count(variation_name_words.x, sort = TRUE) %>% View()

matched_vendor_variation_name_words <- matched_vendor_variation_name_words %>% 
  add_count(variation_name_words.x, name = "count") %>%
  mutate(
    percentage = count / n()
  )

# Before we filter, see if there are any common words we want to add to the exclude list above
common_words_to_add <- matched_vendor_variation_name_words %>% 
  arrange(desc(percentage)) %>% 
  select(variation_name_words.x) %>% 
  distinct() %>% 
  filter(
    str_length(variation_name_words.x) > 1
  ) %>% 
  slice_head(n = 200)

dput(common_words_to_add)

matched_vendor_variation_name_words <- matched_vendor_variation_name_words %>% 
  filter(
    percentage < threshold_highly_common_percentage
  )

matched_vendor_variation_name_words <- matched_vendor_variation_name_words %>% 
  filter(
    distance < threshold_distance_maximum
  )

matched_vendor_variation_name_words <- matched_vendor_variation_name_words %>% 
  filter(
    str_length(variation_name_words.x) > 1L
  )

# Prep for manual checking
matched_vendor_variation_name_words <- matched_vendor_variation_name_words %>% 
  mutate(
    matches = NA
  ) %>% 
  rename(
    source_variation_name = "variation_name.x",
    new_variation_name = "variation_name.y",
    keyword = "variation_name_words.x"
  ) %>% 
  select(
    ! any_of(
      c(
        "variation_name_words.y"
      )
    )
  ) %>% 
  relocate(
    matches,
    source_variation_name,
    new_variation_name,
    keyword,
    everything()
  ) %>% 
  arrange(
    source_variation_name,
    new_variation_name,
    keyword
  )

# Filter to remove tuples? Not necessary here. But we can remove identical entries.
matched_vendor_variation_name_words <- matched_vendor_variation_name_words %>% 
  filter(source_variation_name != new_variation_name)


matched_vendor_variation_name_words %>% 
  write_csv(str_c("data/testing/", today(), "-matched-vendor-variation-name-words-e3.csv"))


# Experiment 3a. don't match individual words
# Just match against the existing vendor matching CSV

threshold_distance_maximum = 0.25

source_vendor_matching_variations <- source_vendor_matching %>% 
  select(variation_name) %>% 
  distinct()


vendors_above_annual_threshold_e3 <- vendors_above_annual_threshold %>% 
  rename(
    variation_name = "parent_company"
  ) %>% 
  mutate(
    variation_name = replace_common_keywords(variation_name)
  )

matched_vendor_variation_name <- source_vendor_matching_variations %>%
  stringdist_inner_join(vendors_above_annual_threshold_e3, by = "variation_name", method="jw", distance_col = "distance")

matched_vendor_variation_name <- matched_vendor_variation_name %>% 
  filter(distance > 0) %>% 
  filter(distance < threshold_distance_maximum)

# matched_vendor_variation_name <- matched_vendor_variation_name %>% 
#   arrange(distance)

# Prep for manual checking
matched_vendor_variation_name <- matched_vendor_variation_name %>% 
  mutate(
    matches = NA
  ) %>% 
  rename(
    source_variation_name = "variation_name.x",
    new_variation_name = "variation_name.y",
  ) %>% 
  relocate(
    matches,
    source_variation_name,
    new_variation_name
  ) %>% 
  arrange(
    source_variation_name,
    new_variation_name
  )

matched_vendor_variation_name %>% 
  write_csv(str_c("data/testing/", today(), "-matched-vendor-variation-name-e3.csv"))
