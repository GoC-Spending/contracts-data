# Load libraries and helper functions
source("lib/helpers.R")
source("lib/amendments.R")
source("lib/vendors.R")
source("lib/exports.R")

# Load vendors from the most recent "meta" vendor list CSV
latest_vendors_list <- read_csv(str_c(output_meta_path, "vendors.csv"))

latest_vendors_list <- latest_vendors_list %>%
  mutate(
    display_label = str_to_title(name),
    updated_display_label = ""
  ) %>%
  select(
    name,
    display_label,
    updated_display_label
  )

latest_vendors_list %>%
  write_csv("data/vendors/vendor_labels_extended.csv")
