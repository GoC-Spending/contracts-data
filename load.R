# Load exploratory libraries
library(tidyverse)
library(lubridate)
library(skimr)
library(janitor)

contracts <- read_csv(
  "data/source/2022-03-11-contracts.csv"
)

