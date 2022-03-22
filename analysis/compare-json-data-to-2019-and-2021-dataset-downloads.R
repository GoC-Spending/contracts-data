# Compares the historical (scraped) JSON data, already converted to CSV
# to more recent downloads of the open.canada.ca data.

# Load the libraries and previous data
source("analysis/compare-2019-to-2021-dataset-downloads.R")

json_contracts_data_clean <- read_csv(
  "data/source/json-archive-contracts.csv"
)

# Compare to 2019 and 2021 datasets
# TODO review ("192876 failed to parse.")
contracts_json <- json_contracts_data_clean %>%
  mutate(contract_date = as_date(contract_date)) %>%
  mutate(gen_contract_year = year(`contract_date`))

contracts_json_by_year <- contracts_json %>%
  group_by(owner_org, gen_contract_year) %>%
  summarize(count = n()) %>%
  rename(
    count_json = count,
    year = gen_contract_year
  )

# Merge into one tibble
contracts_comparison <- bind_rows(contracts_2019_by_year, contracts_2021_by_year, contracts_json_by_year) %>%
  arrange(owner_org, year) %>%
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
                          `count_2021` = "dataset_2021",
                          `count_json` = "dataset_json"
  )) %>%
  group_by(year, dataset) %>%
  summarize(value = sum(value))


# Plot a comparison of the scraped JSON data (circa 2017) vs. 2019 vs. 2021 dataset downloads:
ggplot(contracts_comparison_gc_wide) +
  geom_point(aes(x = year, y = value, color = dataset)) +
  geom_line(aes(x = year, y = value, color = dataset), linetype = "longdash", alpha = 0.5) + 
  xlim(c(2002, 2022)) +
  ylim(c(0, 100000))
