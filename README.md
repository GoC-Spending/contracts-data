# Government of Canada contract data analysis

This repository contains the R code to analyze Government of Canada procurement data published through the [Proactive Disclosure of Contracts](https://open.canada.ca/data/en/dataset/d8f85d91-7dec-4fd1-8055-483b77225d8b).

It uses the [Tidyverse collection of packages](https://www.tidyverse.org/) as well as [janitor](https://github.com/sfirke/janitor), [purrr](https://github.com/tidyverse/purrr) and several others. See [`_libraries.R`](https://github.com/goc-spending/contracts-data/blob/main/lib/_libraries.R) for the full set. 

Inflation adjustments are done using Statistics Canada data via the [cansim package](https://cran.r-project.org/web/packages/cansim/index.html), in [`inflation_adjustments.R`](https://github.com/goc-spending/contracts-data/blob/main/lib/inflation_adjustments.R).

## System requirements

This analysis uses the [R programming language](https://en.wikipedia.org/wiki/R_(programming_language)) and [R Studio](https://posit.co/products/open-source/rstudio/), although it could be run with other R environments.

Running this analysis in R Studio typically requires about 20 GB of RAM; **we’d recommend running it on a computer with at least 32 GB**. A typical run-through takes between 5 and 6 hours to complete.

## Running the analysis

The main analysis takes place in [`load.R`](https://github.com/goc-spending/contracts-data/blob/main/load.R). This does the following steps:

- Downloads the latest copy of the source data from [open.canada.ca](https://open.canada.ca/data/en/dataset/d8f85d91-7dec-4fd1-8055-483b77225d8b)
- Retrieves the latest inflation index data from Statistics Canada using [cansim](https://cran.r-project.org/web/packages/cansim/index.html)
- Conducts the analysis described on the [Methodology](https://govcanadacontracts.ca/methodology/) page
- Exports a variety of summary CSV files to the [`data/out`](https://github.com/goc-spending/contracts-data/tree/main/data/out) folder
- Updates the [run log](https://github.com/goc-spending/contracts-data/blob/main/data/out/run_log.csv) with metadata from the completed analysis run

## Data files

Summary CSV files are produced each time the analysis runs in the [`data/out`](https://github.com/goc-spending/contracts-data/tree/main/data/out) folder. This includes:

- overall trends [across core federal public service departments](https://github.com/goc-spending/contracts-data/tree/main/data/out/overall/core), the [Department of National Defence](https://github.com/goc-spending/contracts-data/tree/main/data/out/overall/dnd), and [all public sector departments and agencies included in the source dataset](https://github.com/goc-spending/contracts-data/tree/main/data/out/overall/all)
- [trends by department or agency](https://github.com/goc-spending/contracts-data/tree/main/data/out/departments)
- [trends by category](https://github.com/goc-spending/contracts-data/tree/main/data/out/categories)
- [trends by IT subcategory](https://github.com/goc-spending/contracts-data/tree/main/data/out/it_subcategories)
- [trends by vendor](https://github.com/goc-spending/contracts-data/tree/main/data/out/vendors)

A number of other analysis outputs are included in the overall trends folders, produced by [`research_findings.R`](https://github.com/goc-spending/contracts-data/blob/main/lib/research_findings.R) which is run automatically by `load.R`. 

Charts for specific presentations and other artefacts are produced by [`additional_research_findings.R`](https://github.com/goc-spending/contracts-data/blob/main/analysis/research/additional_research_findings.R) and [`presentation_findings.R`](https://github.com/goc-spending/contracts-data/blob/main/analysis/research/presentation_findings.R), which need to be run separately after contract data has already loaded in the environment.

You can see the resulting summary CSVs and other trend information in a user-friendly way at [govcanadacontracts.ca](https://govcanadacontracts.ca/). This website is produced from the CSVs generated by this repository, via the Blogdown code in [contracts-data-web](https://github.com/goc-spending/contracts-data-web).

## Feedback and improvements

You can get in touch with the research team via the [About the project](https://govcanadacontracts.ca/about/) page, or use the [feedback form](https://docs.google.com/forms/d/e/1FAIpQLSfHGzAQMaOkj4OD2Kc8Gw4ROChOfx6MKm5t2CSr6R4U2qupBQ/viewform) to send in your suggestions.

This repository is licensed under the [MIT license](https://github.com/GoC-Spending/contracts-data-web/blob/main/LICENSE). Source data from the Government of Canada is available under the [Open Government License – Canada](https://open.canada.ca/en/open-government-licence-canada). Please see the license details for specific R packages that are used for more information.

_A Carleton SPPA Research Project_
