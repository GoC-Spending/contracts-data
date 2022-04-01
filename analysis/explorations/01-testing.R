source("load.R")

# Counting a specific variable
contracts %>% 
  group_by(former_public_servant) %>% 
  summarize(count = n())

# Shorter version of the same thing
contracts %>%
  count(former_public_servant)


# Filter to the 2017-18 to 2020-21 fiscal years
contracts_limited <- contracts %>%
  filter(contract_date >= "2017-04-01", contract_date <= "2021-03-31") %>%
  # Sort columns for what we're most interested in
  relocate(vendor_name, contract_value, contract_date, owner_org)

# Find largest contracts in that time range
contracts_limited %>%
  arrange(desc(contract_value)) %>%
  View()


# Find largest companies (not accounting for amendments yet)



# Find largest departments


