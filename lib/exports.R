# Helper functions for CSV exports for Government of Canada
# Proactive Disclosure of Contracts Data

source("lib/_libraries.R")

output_vendor_path <- "data/out/vendors/"
output_department_path <- "data/out/departments/"

option_round_totals_digits <- 2
option_round_percentages_digits <- 4

# Rounds any column ending in "total" to 2 decimal places
# Thanks to
# https://dplyr.tidyverse.org/reference/across.html
exports_round_totals <- function(input_df) {
  input_df <- input_df %>%
    mutate(
      across(ends_with("total"), ~ round(.x, digits = !!option_round_totals_digits))
    )
  
  return(input_df)
}

# Rounds any column ending in "percentage" to 4 decimal places
# Note: not sure yet if this is useful
exports_round_percentages <- function(input_df) {
  input_df <- input_df %>%
    mutate(
      across(ends_with("percentage"), ~ round(.x, digits = !!option_round_percentages_digits))
    )
  
  return(input_df)
}

# Helper function to quickly turn on and off CSV writes when testing
# Can be drop-in replaced for write_csv in other functions.
write_csv_if_enabled <- function(...) {
  if(option_update_summary_csv_files) {
    write_csv(...)
  } else {
    cat("Note: option_update_summary_csv_files is disabled; would have exported a CSV to:", list(...)[[2]])
  }
}

# If the option is enabled, then remove the summary folders if they exist (for vendors and departments, which might have old entities sitting around).
remove_existing_summary_folders <- function() {
  if(option_remove_existing_summary_folders) {
    print("Removing existing summary folders.")
    if(dir_exists(output_vendor_path)) {
      dir_delete(output_vendor_path)
    }
    if(dir_exists(output_department_path)) {
      dir_delete(output_department_path)
    }
  } else {
    print("Not removing existing summary folders.")
  }
}
