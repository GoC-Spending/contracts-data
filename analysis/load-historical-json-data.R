# Imports the JSON repository located at
# https://github.com/GoC-Spending/goc-spending-data
# and converts it to a CSV file with similar column names as the open.canada.ca dataset

# Load data handling libraries
library(tidyverse)
library(lubridate)
library(skimr)
library(janitor)
library(purrr)
library(jsonlite)

# Thanks to
# https://stackoverflow.com/a/48069897/756641

# Requires the goc-spending-data repo to be cloned in a sibling directory to the working folder
path <- "../goc-spending-data/generated-data/"
files <- dir(path, 
             pattern = "*/*.json",
             recursive = TRUE,
             include.dirs = TRUE)

# CAUTION: This function may take 10+ minutes to run, and more than 1 GiB of memory
json_contracts_data <- files %>%
  map_df(~fromJSON(file.path(path, .), flatten = TRUE))

json_contracts_data_clean <- json_contracts_data %>%
  clean_names() %>%
  rename(
    owner_org = owner_acronym,
    economic_object_code = object_code,
    comments_en = comments,
    description_en = description,
    additional_comments_en = additional_comments,
  )

# Save to a CSV file to save on conversion time in the future
json_contracts_data_clean %>% write_csv("data/source/json-archive-contracts.csv")
