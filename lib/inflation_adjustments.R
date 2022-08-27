# Inflation helper functions
# Thanks to
# https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=3610010601#tables
# https://dataenthusiast.ca/2019/statcan-data-in-r-part-1-cansim/

source("lib/_libraries.R")
library("cansim")

option_inflation_price_index_vector <- Sys.getenv("option_inflation_price_index_vector", "v62307266")
output_inflation_monthly_table_filepath <- "data/out/inflation_adjustments/constant_dollars_multiplier_monthly.csv"

# 2012=100 in the default index
gdp_price_index_quarterly <- get_cansim("36-10-0106-01") %>%
  clean_names()

generate_constant_dollars_multiplier_table <- function(vector = "v62307282") {
  
  # Potential vectors (Implicit price indexes)
  # v62307282 = Gross domestic product at market prices
  # v62307266 = General governments final consumption expenditure
  # v62307275 = General governments gross fixed capital formation
  gdp_market_prices_quarterly <- gdp_price_index_quarterly %>%
    filter(vector == !!vector) %>%
    filter(ref_date > "2000-01") %>%
    select(
      ref_date,
      val_norm,
      date,
      index,
      estimates
    )
  
  
  start_date <- as.Date(first(gdp_market_prices_quarterly$date))
  end_date <- as.Date(last(gdp_market_prices_quarterly$date))
  
  # Thanks to
  # https://stackoverflow.com/a/62268564/756641
  
  range <- tibble(
    month = seq( start_date, end_date, by = "1 month" )
  ) %>%
    separate(
      month,
      into = c("year", "month", "day"),
      sep = "-",
      remove = TRUE
    ) %>%
    select(year, month) %>%
    unite(col = "ref_date", c("year", "month"), sep = "-", remove = FALSE) %>%
    left_join(gdp_market_prices_quarterly, by = "ref_date") %>%
    select(ref_date, year, month, val_norm) %>%
    fill(val_norm) %>%
    rename(
      val_norm_2012 = val_norm
    )
  
  index_2019 <- range %>%
    filter(year == 2019) %>%
    summarise(
      mean = mean(val_norm_2012)
    ) %>%
    pull(mean)
  
  range <- range %>%
    mutate(
      constant_dollars_multiplier_2019 = (index_2019 / val_norm_2012)
    )
  
  # Just keep the date and multiplier columns (this removes year, month, and val_norm_2012)
  range <- range %>%
    select(
      ref_date,
      constant_dollars_multiplier_2019
    )
  
  range
  
}

constant_dollars_multiplier_table <- generate_constant_dollars_multiplier_table(option_inflation_price_index_vector)

# Write it to a CSV file for posterity (when changed)
constant_dollars_multiplier_table %>%
  write_csv(output_inflation_monthly_table_filepath)

