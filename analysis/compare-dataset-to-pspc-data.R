# Load the libraries + the main CSV
source("load.R")
library(janitor)

pspc_contracts <- read_csv(
  "data/source/2022-03-24-tpsgc-pwgsc_co-ch_tous-all.csv"
) %>%
  clean_names() %>%
  mutate(gen_contract_year = year(`award_date`))

contracts_2022 <- contracts %>%
  mutate(gen_contract_year = year(`contract_date`))


# Comparisons between the two datasets
contracts_2022_by_year <- contracts_2022 %>%
  group_by(gen_contract_year) %>%
  summarize(count = n()) %>%
  rename(
    dataset_2022 = count,
    year = gen_contract_year
  )

pspc_contracts_by_year <- pspc_contracts %>%
  group_by(gen_contract_year) %>%
  summarize(count = n()) %>%
  rename(
    dataset_2022_pspc = count,
    year = gen_contract_year
  )

# Merge the counts from both datasets into one tibble
contracts_comparison <- bind_rows(contracts_2022_by_year, pspc_contracts_by_year) %>%
  arrange(year) %>%
  pivot_longer(
    cols = starts_with("dataset"),
    names_to = "dataset",
    values_drop_na = TRUE
  )

# Plot a comparison of the two dataset downloads:
ggplot(contracts_comparison) +
  geom_point(aes(x = year, y = value, color = dataset)) +
  geom_line(aes(x = year, y = value, color = dataset), linetype = "longdash", alpha = 0.5) + 
  xlim(c(2002, 2022))
