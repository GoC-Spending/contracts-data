# Additional calculations and findings for upcoming presentations

# These are designed to be run after the main set of contracts is parsed, in load.R
# Note: if run directly here, this will take 3-4 hours to process first (!)
# source("load.R")

# This also depends on some functions from additional_research_findings.R
# source("analysis/research/additional_research_findings.R")

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


# Updated plots for presentation graphics =================

ggsave_16_9_default_options <- function(filename, custom_height = 7) {
  ggsave(filename, dpi = "print", width = 11, height = custom_height, units = "in")
  
}

# Adapted slightly from the version in additional_research_findings, for a 16:9 slide layout
plot_16_9_fiscal_year_2019_dollars <- function(df, custom_labels = labs(), num_legend_rows = 2) {
  
  
  # Automatically uses the first column as the grouping category:
  grouping_category <- names(df)[[1]]
  
  # Thanks to
  # https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html
  df <- df %>%
    mutate(
      year = convert_fiscal_year_to_start_year(d_fiscal_year),
      category = first(across({{ grouping_category}}, ~ as.factor(.)))
    ) %>%
    mutate(
      # Converts to double to avoid string type issues in future numeric operations
      # (In case this wasn't done previously.)
      total = as.double(total),
      total_constant_2019_dollars = as.double(total_constant_2019_dollars)
    )
  
  df <- df %>%
    group_by(category) %>%
    mutate(
      most_recent_total_constant_2019_dollars = last(total_constant_2019_dollars, order_by = d_fiscal_year)
    )
  
  ggplot(df, aes(
    x = year, 
    y = total_constant_2019_dollars, 
    color = reorder(category, desc(most_recent_total_constant_2019_dollars)), 
    shape = reorder(category, desc(most_recent_total_constant_2019_dollars))
  )) +
    geom_point(size = 4) +
    geom_line(size = 0.7) + 
    theme(
      axis.text = element_text(size = rel(0.8), colour = "black"),
      axis.title = element_text(size = rel(0.8), colour = "black"),
      aspect.ratio=3/4,
      legend.position = "right",
      legend.direction = "horizontal",
      legend.margin=margin(),
      legend.text = element_text(size = rel(0.65)),
      panel.background = element_rect(fill="#FFFFFF", colour = "#f5f5f5"),
      plot.background = element_rect(fill="#FAFAFA", colour = "#FAFAFA"),
      
      panel.grid.major = element_line(size=0.7, colour = "#f5f5f5"),
      axis.ticks = element_line(size=0.7, colour = "#f5f5f5"),
      
      legend.background = element_rect(fill="#FAFAFA"),
      # legend.box.background = element_rect(fill="#FFFFFF"),
      legend.key = element_rect(fill = "#FFFFFF", colour = "#f5f5f5"),
      
    ) + 
    # Thanks to
    # https://stackoverflow.com/a/48252093/756641
    guides(
      color = guide_legend(nrow = num_legend_rows),
      shape = guide_legend(nrow = num_legend_rows)
    ) +
    # Thanks to
    # https://www.tidyverse.org/blog/2022/04/scales-1-2-0/#numbers
    scale_y_continuous(
      limits = c(0, NA),
      labels = label_dollar(scale_cut = cut_short_scale())
    ) +
    # 17 options for scale icons, hopefully distinct enough to avoid confusion
    # Thanks to
    # https://stackoverflow.com/a/41148368/756641
    # https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1003833#s9
    scale_shape_manual(values = c(16, 17, 15, 18, 3, 4, 8, 1, 2, 0, seq(5, 7), 9, 10, 12, 14)) +
    custom_labels
  
  # df
  
}

# Same as above but with the top 10 overall IT consulting firms, rather than a preset list:
retrieve_summary_vendors_by_it_subcategories() %>% 
  filter_to_it_consulting_services() %>%
  select(! d_most_recent_it_subcategory) %>%
  plot_16_9_fiscal_year_2019_dollars(labs(
    title = "",
    x = "Fiscal year",
    y = "Total estimated IT consulting services \ncontract spending (constant 2019 dollars)",
    color = "",
    shape = ""
  ), 10)

ggsave_16_9_default_options("plots/f002_it_consulting_services_top_10_overall_vendors_by_fiscal_year.png", 6)
