# Load exploratory libraries
library(tidyverse)
library(lubridate)
library(skimr)
library(janitor)
library(purrr)

contracts <- read_csv(
  "data/source/2022-03-24-contracts.csv"
) %>%
  clean_names()


