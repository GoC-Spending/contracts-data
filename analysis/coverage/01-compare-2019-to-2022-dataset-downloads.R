# Load the libraries
source("lib/helpers.R")

# Load the latest CSV file (or download today's if it doesn't already exist)
contracts <- get_contracts_csv_locally_or_from_url(contract_col_types)


contracts_2022 <- contracts %>%
  mutate(gen_contract_year = year(`contract_date`))

ggplot(contracts_2022) +
  geom_bar(aes(x = gen_contract_year)) + 
  xlim(c(2002, 2022))


# Similar comparison for the 2019 contracts download
contracts_2019 <- read_csv(
  "data/source/2019-03-24-contracts.csv"
)

contracts_2019 <- contracts_2019 %>%
  mutate(gen_contract_year = year(`contract_date`))

ggplot(contracts_2019) +
  geom_bar(aes(x = gen_contract_year)) + 
  xlim(c(2002, 2022))


# Comparisons between the two datasets
contracts_2019_by_year <- contracts_2019 %>%
  group_by(owner_org, gen_contract_year) %>%
  summarize(count = n()) %>%
  rename(
    count_2019 = count,
    year = gen_contract_year
  )

contracts_2022_by_year <- contracts_2022 %>%
  group_by(owner_org, gen_contract_year) %>%
  summarize(count = n()) %>%
  rename(
    count_2022 = count,
    year = gen_contract_year
  )

# Merge the counts from both datasets into one tibble
contracts_comparison <- bind_rows(contracts_2019_by_year, contracts_2022_by_year) %>%
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
                          `count_2022` = "dataset_2022"
  )) %>%
  group_by(year, dataset) %>%
  summarize(value = sum(value))


# Plot a comparison of the 2019 vs. 2022 dataset downloads:
ggplot(contracts_comparison_gc_wide) +
  geom_point(aes(x = year, y = value, color = dataset)) +
  geom_line(aes(x = year, y = value, color = dataset), linetype = "longdash", alpha = 0.5) + 
  xlim(c(2002, 2022))

