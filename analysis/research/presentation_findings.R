# Additional calculations and findings for upcoming presentations

# These are designed to be run after the main set of contracts is parsed, in load.R
# Note: if run directly here, this will take 3-4 hours to process first (!)
# source("load.R")

# FWD50 calculations ============================

retrieve_most_recent_it_total <- function() {
  
  # Get category breakdown by fiscal year across all depts
  summary_overall_by_category <- get_summary_overall_by_fiscal_year_by_criteria("all", "d_most_recent_category")
  
  most_recent_fiscal_year <- summary_overall_by_category %>%
    select(d_fiscal_year) %>%
    distinct() %>%
    arrange(desc(d_fiscal_year)) %>%
    pull(d_fiscal_year) %>%
    first()
  
  summary_overall_by_category %>%
    filter(d_most_recent_category == "3_information_technology") %>%
    filter(d_fiscal_year == !!most_recent_fiscal_year) %>%
    pull(total) %>%
    as.double()
  
  
}

it_total_most_recent_fiscal_year <- retrieve_most_recent_it_total()

# Comparisons to other items

# Flora footbridge at Bank and Fifth
# $19M in 2019 (originally budgeted at $21M)
# http://www.mainstreeter.ca/index.php/2019/08/23/flora-footbridge-opens-with-a-flourish/
# https://intheglebe.ca/blog/fifth-clegg-footbridge-win-entire-city/

# Whitehorse 47-unit community housing building
# $21M in 2022 (originally budged at $19M)
# https://www.whitehorsestar.com/News/housing-project-may-be-done-by-fall-2021
# https://www.whitehorsestar.com/News/delayed-project-s-cost-rises-to-21-7-million

# VIA Rail Siemens Venture trainsets
# $989M for 32 trainsets
# https://corpo.viarail.ca/en/news/via-rails-new-set-of-trains
# https://corpo.viarail.ca/en/projects-infrastructure/train-fleet/corridor-fleet

# F-35A fighter jet
# $78M USD per unit / $107M CAD per unit
# https://www.forbes.com/sites/sebastienroblin/2021/07/31/f-35a-jet-price-to-rise-but-its-sustainment-costs-that-could-bleed-air-force-budget-dry/?sh=5127ab4f32df
# https://www.airandspaceforces.com/massive-34-billion-f-35-contract-includes-price-drop-as-readiness-improves/


comparison_costs <- tribble(
  ~name, ~per_unit_cost,
  "Ottawa Flora Footbridge", 19000000,
  "YHC 47-unit Affordable Housing Project", 21000000,
  "VIA Rail Siemens Trainset", 989000000/32,
  "Lockheed Martin F-35A", 107000000
)

comparison_costs <- comparison_costs %>%
  mutate(
    equivalent_units = round(it_total_most_recent_fiscal_year / per_unit_cost)
  )

comparison_costs %>%
  write_csv(str_c("data/testing/tmp-", today(), "-comparison-cost-examples.csv"))

