# Load the libraries + the main CSV
source("load.R")

# Minor mutate for the contract loaded in the source file
contracts_2022 <- contracts %>%
  mutate(gen_contract_year = year(`contract_date`))

# Import function
import_and_format <- function(filename) {
  csv_data <- read_csv(
    str_c("data/source/", filename)
  )
  
  # Thanks to
  # https://stackoverflow.com/questions/4374185/regular-expression-match-to-test-for-a-valid-year#comment49960661_4374209
  csv_data <- csv_data %>%
    mutate(gen_contract_year = str_extract(`contract_date`, "(?:19|20)\\d{2}")) %>%
    filter(! is.na(gen_contract_year)) %>%
    mutate(gen_contract_year = year(as.Date(`gen_contract_year`,format="%Y")))
  
  return(csv_data)
}


# Convert to per-year summary function
summarize_by_year <- function(data, year) {
  year_label <- str_c("dataset_", year)
  
  # Thanks to
  # https://community.rstudio.com/t/pass-a-variable-to-dplyr-rename-to-change-columnname/6907/2
  
  data <- data %>%
    group_by(owner_org, gen_contract_year) %>%
    summarize(count = n()) %>%
    rename(
      !!year_label := count,
      year = gen_contract_year
    ) %>%
    arrange(owner_org, year)
  
  return(data)
}


contracts_2018 <- import_and_format("2018-03-08-contracts.csv")
contracts_2019 <- import_and_format("2019-03-24-contracts.csv")
# No 2020 data available, unfortunately
contracts_2021 <- import_and_format("2021-03-20-contracts.csv")
# 2022 data is already loaded

contracts_2018_by_year <- summarize_by_year(contracts_2018, "2018")
contracts_2019_by_year <- summarize_by_year(contracts_2019, "2019")
contracts_2021_by_year <- summarize_by_year(contracts_2021, "2021")
contracts_2022_by_year <- summarize_by_year(contracts_2022, "2022")


# Merge the counts from both datasets into one tibble
contracts_comparison <- bind_rows(
  contracts_2018_by_year, 
  contracts_2019_by_year,
  contracts_2021_by_year,
  contracts_2022_by_year
  ) %>%
  arrange(owner_org, year) %>%
  pivot_longer(
    cols = starts_with("dataset_"),
    names_to = "dataset",
    values_drop_na = TRUE
  ) %>%
  arrange(owner_org, year, dataset)


contracts_comparison_gc_wide <- contracts_comparison %>%
  ungroup() %>%
  select(year, dataset, value) %>%
  group_by(year, dataset) %>%
  summarize(value = sum(value))

# Plot a comparison 
ggplot(contracts_comparison_gc_wide) +
  geom_point(aes(x = year, y = value, color = dataset)) +
  geom_line(aes(x = year, y = value, color = dataset), linetype = "longdash", alpha = 0.5) + 
  xlim(c(2002, 2022))

# Figure out per-year deltas (work in progress)
contracts_comparison_delta <- contracts_comparison_gc_wide %>%
  pivot_wider(
    names_from = dataset,
    values_from = value
  ) 
  
# Create a hypothetical 2020 dataset using the 2019 and 2021 averages
# Worth confirming if this is a valid approach here (likely isn't)
# when we're trying to model changes over time
contracts_comparison_delta <- contracts_comparison_delta %>%
  mutate(dataset_2020 = as.integer(mean(c(dataset_2019, dataset_2021)))) %>%
  relocate(
    year,
    dataset_2018,
    dataset_2019,
    dataset_2020,
    dataset_2021,
    dataset_2022
  ) %>%
  filter(
    year >= 2000,
    year <= 2025
  )

contracts_comparison_delta <- contracts_comparison_delta %>%
  mutate(
    delta_2019_from_2018 = dataset_2019 - dataset_2018,
    delta_2020_from_2019 = dataset_2020 - dataset_2019,
    delta_2021_from_2020 = dataset_2021 - dataset_2020,
    delta_2022_from_2021 = dataset_2022 - dataset_2021,
  )

safe_percent_comparison <- function(newer_dataset, older_dataset) {
  value <- if_else(older_dataset == 0, as.double(NA), as.double(((newer_dataset - older_dataset) / older_dataset)))
  
  # Because we're not interested in "new" years (where the percentage increase is 1000%+), skip those here:
  value <- if_else(value > 10, as.double(NA), value)
  return(value)
}

contracts_comparison_delta <- contracts_comparison_delta %>%
  mutate(
    percent_2019_from_2018 = safe_percent_comparison(dataset_2019, dataset_2018),
    percent_2020_from_2019 = safe_percent_comparison(dataset_2020, dataset_2019),
    percent_2021_from_2020 = safe_percent_comparison(dataset_2021, dataset_2020),
    percent_2022_from_2021 = safe_percent_comparison(dataset_2022, dataset_2021),
  )

contracts_comparison_delta %>%
  # select(
  #   starts_with("percent_")
  # ) %>%
  arrange(desc(year)) %>%
  View()

# Export a CSV for more analysis
contracts_comparison_delta %>% write_csv("data/out/multiple-dataset-comparison-delta.csv")


# Need to re-pivot before charting back into tidy data
contracts_comparison_delta_tidy <- contracts_comparison_delta %>%
  select(
    year,
    starts_with("percent_")
  ) %>%
  pivot_longer(
  cols = starts_with("percent_"),
  names_to = "dataset"
) %>%
  arrange(year, dataset)

# Plot a comparison of the scraped JSON data (circa 2017) vs. 2019 vs. 2022 dataset downloads:
ggplot(contracts_comparison_delta_tidy) +
  geom_point(aes(x = year, y = value, color = dataset)) +
  geom_line(aes(x = year, y = value, color = dataset), linetype = "longdash", alpha = 0.5) + 
  xlim(c(2002, 2022)) +
  ylim(c(0, 1))
