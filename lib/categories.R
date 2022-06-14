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
    d_economic_object_code = economic_object_code
  )
