# Handler functions to categorize industry categories
# in Government of Canada contracting data

source("lib/_libraries.R")

category_matching_file <- "data/categories/economic_object_codes_to_category.csv"

# Import the CSV file
# Rename the economic_object_code column for convenience with future join operations
category_matching <- read_csv(
  category_matching_file,
) %>%
  clean_names() %>%
  mutate(
    economic_object_code = str_pad(economic_object_code, 4, side = "left", pad = "0")
  ) %>%
  rename(
    d_economic_object_code = economic_object_code,
    category_by_economic_object_code = category
  )

# Note: revisit this with fuzzyjoin or similar tools to account for 
# typos or inconsistent descriptions in future contract data updates.
description_matching_file <- "data/categories/descriptions_to_category.csv"

description_matching <- read_csv(
  description_matching_file,
) %>%
  clean_names() %>%
  rename(
    category_by_description = category
  )


# For edge cases (large dollar value contracts with erroneous or generic economic object codes), match these here.

vendor_specific_category_matching_file <- "data/categories/economic_object_codes_and_vendors_to_category.csv"

vendor_specific_category_matching <- read_csv(
  vendor_specific_category_matching_file,
) %>%
  clean_names() %>%
  mutate(
    economic_object_code = str_pad(economic_object_code, 4, side = "left", pad = "0")
  ) %>%
  rename(
    d_economic_object_code = economic_object_code,
    category_by_vendor_and_economic_object_code = category
  )


# Category labels

# A variety of options on how to label categories
# (with and without leading zeroes, with no numbers, with friendly names, etc.)
category_labels_file <- "data/categories/category_labels.csv"

category_labels <- read_csv(
  category_labels_file,
)


# IT-specific categorizations =============================

gsin_it_subcategories_file <- "data/categories/gsin_it_subcategories.csv"
extended_description_it_subcategories_file <- "data/categories/extended_description_it_subcategories.csv"

gsin_it_subcategories <- read_csv(gsin_it_subcategories_file)
extended_description_it_subcategories <- read_csv(extended_description_it_subcategories_file)

# Avoid any accidental spacing issues in the source CSV input
extended_description_it_subcategories <- extended_description_it_subcategories %>%
  mutate(keyword = str_trim(keyword))

# Usage is contracts %>% identify_it_subcategories()
identify_it_subcategories <- function(df) {
  
  # By GSIN code
  df <- df %>%
    left_join(gsin_it_subcategories, by = c("commodity_code" = "nibs_gsin")) %>%
    rename(
      d_it_subcategory_via_gsin = "d_it_subcategory"
    )
  
  # By description (to override some generic GSIN codes like R019 and R199)
  # Prep for the function by adding an empty d_it_subcategory_via_individual_keyword so it can be re-run multiple times.
  df <- df %>%
    mutate(
      d_it_subcategory_via_individual_keyword = NA_character_
    )
  
  # ...this works! So fun.
  df <- reduce2(extended_description_it_subcategories$keyword, extended_description_it_subcategories$d_it_subcategory, identify_it_subcategory_via_individual_keyword, .init = df)
  
  # Consolidate into one column
  df <- df %>%
    mutate(
      d_it_subcategory = case_when(
        !is.na(d_it_subcategory_via_individual_keyword) ~ d_it_subcategory_via_individual_keyword,
        !is.na(d_it_subcategory_via_gsin) ~ d_it_subcategory_via_gsin,
        TRUE ~ NA_character_
      )
    )
  
  
  # Label as category = IT by subcategory
  df <- df %>%
    mutate(
      category_by_it_subcategory = case_when(
        !is.na(d_it_subcategory) ~ "3_information_technology",
        TRUE ~ NA_character_
      )
    )
  
  df
  
}


identify_it_subcategory_via_individual_keyword <- function(df, keyword, resulting_it_subcategory) {
  
  
  
  df <- df %>%
    mutate(
      d_it_subcategory_via_individual_keyword = case_when(
        str_detect(d_description_comments_extended_lower, pattern = !!keyword) ~ !!resulting_it_subcategory,
        TRUE ~ d_it_subcategory_via_individual_keyword
      )
    )
  
  df
  
}


# For a small set of keywords, revert it_other to it_consulting_services where the descriptions match
# Note: this updates the column directly, instead of creating an origin column like the earlier IT subcategory approaches
# This is limited to contracts that are already categorized as "it_other" to avoid overriding other areas.
update_it_other_subcategories <- function(df) {
  
  df <- df %>%
    mutate(
      d_it_subcategory = case_when(
        d_it_subcategory == "it_other" & str_detect(d_description_comments_extended_lower, pattern = "management consulting|research contracts|consulting services") ~ "it_consulting_services",
        TRUE ~ d_it_subcategory
      )
    )
  
  df
  
}

# IT subcategory labels =========================

it_subcategory_labels_file <- "data/categories/it_subcategory_labels.csv"

it_subcategory_labels <- read_csv(
  it_subcategory_labels_file,
)

# Testing (2022-08-31)

# gsin_it_subcategories <- read_csv(gsin_it_subcategories_file) %>%
#   clean_names() %>%
#   select(nibs_gsin, gsin_description_en, d_it_category) %>%
#   filter(!is.na(d_it_category)) %>% 
#   rename(
#     d_it_subcategory = "d_it_category"
#   )
# 
# gsin_it_subcategories %>%
#   write_csv(gsin_it_subcategories_file)

# extended_description_it_subcategories <- read_csv(extended_description_it_subcategories_file) %>%
#   clean_names() %>%
#   mutate(
#     keyword = str_to_lower(keyword)
#   )
# 
# extended_description_it_subcategories %>%
#   write_csv(extended_description_it_subcategories_file)

# x <- contracts %>% filter(!is.na(category_by_it_subcategory)) %>% find_selection_helper() %>% arrange(desc(d_contract_value))
