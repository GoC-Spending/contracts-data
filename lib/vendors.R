# Handler functions to clean up vendor names
# in Government of Canada contracting data

source("lib/_libraries.R")

#vendor_matching_file <- "../goc-spending-vendors/vendor_data.csv"
vendor_matching_file <- "data/vendors/vendor_normalization_data.csv"


# Convert to consistent capitalization and spacing
# Remove unexpected characters, accents, and suffixes
clean_vendor_names <- function(vendor_name) {
  
  # Remove accented characters, and switch to uppercase
  # Thanks to
  # https://github.com/tidyverse/stringr/issues/149#issuecomment-289151373
  vendor_name <- stringi::stri_trans_general(vendor_name, "Latin-ASCII; upper")
  
  # Remove any punctuation
  vendor_name <- str_replace_all(vendor_name, "[[:punct:]]", " ")
  
  # Vendor suffixes previously listed here:
  # https://github.com/GoC-Spending/goc-spending-laravel/blob/master/app/VendorData.php#L17-L57
  str <- list()
  str$pattern <- c(
    "LIMITED",
    "LIMITEE",
    "LIMITE",
    "LIMITACE",
    "LTE",
    "LT",
    "LTEE",
    "LLP",
    "LP",
    "PLC",
    "LCC",
    "LLC",
    "INCORPORATED",
    "INC",
    "LTD",
    "LDT",
    "CO",
    "CORP",
    "CORPORATION",
    "PLC",
    "PTY",
    "ULC",
    "LP",
    "AB",
    "SENC",
    "SENCRL",
    "SENCRLSRL",
    "SRL",
    "LLPSEN",
    "LTACE",
    "GMBH",
    "SA",
    "SPZOO",
    "SP ZOO",
    "SP Z OO",
    "SP Z O O",
    "BV",
    "B V",
    "SAS",
    "S A",
    )
  
  # Include a leading and trailing space for the suffixes below
  # on the rare chance that they form part of a company's actual name.
  str$pattern <- str_c(" ", str$pattern, " ")
  
  # Add a trailing space to each string
  # As a simple way of accidentally removing "AB" etc. from the actual names of companies
  vendor_name <- str_c(vendor_name, " ")
  
  # Replace with a space; use str_squish to remove extra spaces
  str$replacement <- rep(" ", length(str$pattern))
  
  # Thanks to
  # https://community.rstudio.com/t/replacing-multiple-patterns-via-str-replace/111642/4
  vendor_name <- reduce2(str$pattern, str$replacement, str_replace, .init = vendor_name)
  
  return(str_squish(vendor_name))
  
}

# Import the CSV file
vendor_matching <- read_csv(
  vendor_matching_file,
) %>%
  clean_names()

