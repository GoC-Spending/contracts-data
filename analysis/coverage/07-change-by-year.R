# Load the libraries + the main CSV
source("load.R")

contracts <- contracts %>%
  mutate(gen_contract_year = year(`contract_date`))

# Note: all of the contracts in the open data set 
# include contract_date
# sum(is.na(contracts$gen_contract_year)) 
# [1] 0


# Attempt 1: in oldest to newest order
contracts_by_owner_and_year <- contracts %>%
  group_by(owner_org, gen_contract_year) %>%
  summarize(count = n()) %>%
  arrange(owner_org, gen_contract_year) %>%
  mutate(
    # cumulative_count = cumsum(count)
    count_increase = count - lag(count, default = 0),
    count_increase_percent = (count - lag(count, default = 0)) / lag(count, default = 1)
  )

# Attempt 2: newest to oldest order
# TODO: should change "count_decrease" to a more descriptive name
# When the year order is reversed back to normal, it isn't really
# a decrease anymore.
contracts_by_owner_and_year <- contracts %>%
  group_by(owner_org, gen_contract_year) %>%
  summarize(count = n()) %>%
  arrange(owner_org, desc(gen_contract_year)) %>%
  mutate(
    count_decrease = count - lead(count, default = 0),
    count_decrease_percent_1 = (count - lead(count, default = 0)) / count,
    count_decrease_percent_2 = (count - lead(count, default = 0)) / lead(count, default = 1),
    # count_decrease_percent_1_x100 = count_decrease_percent_1 * 100,
  ) %>%
  arrange(owner_org, gen_contract_year)

# Plot the percentage changes by year 
# (not a great visualization at the moment)
ggplot(contracts_by_owner_and_year) +
  geom_point(aes(x = gen_contract_year, y = count_decrease), alpha = 0.5) +
  #geom_line(aes(x = gen_contract_year, y = count_decrease), linetype = "longdash", alpha = 0.3) + 
  xlim(c(2002, 2022)) + 
  #ylim(c(-200, 200)) +
  theme(legend.position="none")
  