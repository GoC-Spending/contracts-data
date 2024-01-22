# Load exploratory libraries
library(tidyverse)
library(conflicted)
library(lubridate)
library(skimr)
library(janitor)
library(purrr)
library(fs)
library(stringi)
library(fuzzyjoin)
library(scales)
library(zip)

# Thanks to
# https://www.rebeccabarter.com/blog/2020-02-05_rstudio_conf/#tip-3-function-conflicts-workaround-no-more-dplyrselect
conflict_prefer("col_factor", "readr")
conflict_prefer("filter", "dplyr")

# Include optional environment variables
# Thanks to
# https://stackoverflow.com/a/56262739/756641
if(file_exists(".env")) {
  readRenviron(".env")
}
