# Load the libraries + the main CSV
source("load.R")

contracts_2021 <- contracts %>%
  mutate(gen_contract_year = year(`contract_date`))

ggplot(contracts_2021) +
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

contracts_2021_by_year <- contracts_2021 %>%
  group_by(owner_org, gen_contract_year) %>%
  summarize(count = n()) %>%
  rename(
    count_2021 = count,
    year = gen_contract_year
  )

# Merge the counts from both datasets into one tibble
contracts_comparison <- bind_rows(contracts_2019_by_year, contracts_2021_by_year) %>%
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
                          `count_2021` = "dataset_2021"
  )) %>%
  group_by(year, dataset) %>%
  summarize(value = sum(value))


# Plot a comparison of the 2019 vs. 2021 dataset downloads:
ggplot(contracts_comparison_gc_wide) +
  geom_point(aes(x = year, y = value, color = dataset)) +
  geom_line(aes(x = year, y = value, color = dataset), linetype = "longdash", alpha = 0.5) + 
  xlim(c(2002, 2022))

