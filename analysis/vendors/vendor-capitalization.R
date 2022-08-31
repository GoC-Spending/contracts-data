# Load libraries and helper functions
source("lib/helpers.R")
source("lib/amendments.R")
source("lib/vendors.R")
source("lib/exports.R")

# Load vendors from the most recent "meta" vendor list CSV
latest_vendors_list <- read_csv(str_c(output_meta_path, "vendors.csv"))

vendor_labels_extended_filepath <- "data/vendors/vendor_labels_extended.csv"
vendor_labels_filepath <- "data/vendors/vendor_labels.csv"

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
  write_csv(vendor_labels_extended_filepath)

# After manual updates, for clean Git updates
regenerate_csv_file(vendor_labels_extended_filepath, na = "")

# Bring in changes into a vendor_labels file
updated_vendor_labels <- read_csv(vendor_labels_extended_filepath) %>%
  filter(!is.na(updated_display_label)) %>%
  select(name, updated_display_label) %>%
  rename(
    display_label = "updated_display_label"
  ) %>%
  arrange(name)

# Avoid single quotes being an issue in future YAML files etc.
# Hooray for Unicode characters! 
updated_vendor_labels <- updated_vendor_labels %>%
  mutate(
    display_label = str_replace_all(display_label, "'", "’")
  )

updated_vendor_labels %>%
  write_csv(vendor_labels_filepath)
