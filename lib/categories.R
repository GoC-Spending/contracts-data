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
