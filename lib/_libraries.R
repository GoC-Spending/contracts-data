# Load exploratory libraries
library(tidyverse)
library(lubridate)
library(skimr)
library(janitor)
library(purrr)
library(fs)
library(stringi)
library(fuzzyjoin)
library(scales)

# Include optional environment variables
# Thanks to
# https://stackoverflow.com/a/56262739/756641
if(file_exists(".env")) {
  readRenviron(".env")
}
