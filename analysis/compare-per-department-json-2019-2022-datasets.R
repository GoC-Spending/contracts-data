# Import previous libraries and data
source("analysis/compare-json-data-to-2019-and-2022-dataset-downloads.R")

# Create a conversion table for department acronyms
department_conversion_table = tribble(
  ~old_name, ~new_name,
  # -------,----------
  "agr","aafc",
  "feddev","feddevontario",
  "inac","isc", # Review how to deal with aandc and isc later
  "infra","infc",
  "ircc","cic",
  "irccpc","pptc",
  "just","jus",
  "pspc","pwgsc",
  "sc","esdc", # Looks like ServiceCanada was previously published separately from ESDC, but not anymore (to confirm)
  "stats","statcan",
)

# Thanks to
# https://stackoverflow.com/a/67081617/756641
department_conversion_lookup <- setNames(department_conversion_table$new_name, department_conversion_table$old_name)

# Note: "cc" (Canada Council for the Arts) is the only department in the JSON data that does not exist (in some form) in the 2022 data

# Create a consistent owner_org column
contracts_json <- contracts_json %>%
  # Use the lookup table to switch to the new names
  # Note that this slightly reduces the number of departments (since SC and ESDC are merged, for example)
  mutate(gen_owner_org_short = coalesce(department_conversion_lookup[ owner_org ], owner_org))
  
  
# Get the list of departments in the JSON dataset
departments_json <- contracts_json %>%
  select("gen_owner_org_short") %>%
  distinct() %>%
  pull("gen_owner_org_short")

# (temp) Quickly determine the acronyms and department names in each dataset
# contracts_2022 %>%
#   select("owner_org","owner_org_title") %>%
#   distinct() %>%
#   View()

# Create short-form owner_org columns to match the JSON departmental acronyms (which are unilingual)
# Thanks to
# https://stackoverflow.com/a/41987195/756641
contracts_2022 <- contracts_2022 %>%
  mutate(gen_owner_org_short = str_extract(owner_org, "^[^-]+"))

# Use the short version of owner_org generated above
departments_2022 <- contracts_2022 %>%
  select("gen_owner_org_short") %>%
  distinct() %>%
  pull("gen_owner_org_short")

# Filter to departments that are in *both* datasets (using intersect())
contracts_2022_limited_to_common_departments <- contracts_2022 %>%
  filter(gen_owner_org_short %in% intersect(departments_json, departments_2022))
  
contracts_json_limited_to_common_departments <- contracts_json %>%
  filter(gen_owner_org_short %in% intersect(departments_json, departments_2022))



# Repeat the previous analysis (todo: move this into a function)
# using the datasets limited to original departments

# Note: not repeating the gen_contract_year calculation here since it is run in the source file at the start.

contracts_json_by_year <- contracts_json_limited_to_common_departments %>%
  group_by(gen_owner_org_short, gen_contract_year) %>%
  summarize(count = n()) %>%
  rename(
    count_json = count,
    year = gen_contract_year
  )

# Recalaculating this with the smaller number of departments
contracts_2022_by_year <- contracts_2022_limited_to_common_departments %>%
  group_by(gen_owner_org_short, gen_contract_year) %>%
  summarize(count = n()) %>%
  rename(
    count_2022 = count,
    year = gen_contract_year
  )

# Merge into one tibble
# (temporarily) skipping contracts_2019_by_year
contracts_comparison <- bind_rows(contracts_2022_by_year, contracts_json_by_year) %>%
  arrange(gen_owner_org_short, year) %>%
  pivot_longer(
    cols = starts_with("count"),
    names_to = "dataset",
    values_drop_na = TRUE
  )

contracts_comparison_gc_wide <- contracts_comparison %>%
  ungroup() %>%
  select(year, dataset, value) %>%
  mutate(dataset = recode(dataset,
                          `count_2019` = "dataset_2019",
                          `count_2022` = "dataset_2022",
                          `count_json` = "dataset_json"
  )) %>%
  group_by(year, dataset) %>%
  summarize(value = sum(value))


# Plot a comparison of the scraped JSON data (circa 2017) vs. 2019 vs. 2022 dataset downloads:
ggplot(contracts_comparison_gc_wide) +
  geom_point(aes(x = year, y = value, color = dataset)) +
  geom_line(aes(x = year, y = value, color = dataset), linetype = "longdash", alpha = 0.5) + 
  xlim(c(2002, 2022)) +
  ylim(c(0, 75000))
