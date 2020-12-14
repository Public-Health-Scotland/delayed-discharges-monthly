#########################################################################
# Name of file - 04_knit-summary.R
# Data release - Monthly Delayed Discharges publication
# Original Authors - Alice Byers
# Orginal Date - December 2020
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Server
# Version of R - 3.6.1
#
# Description - Knit markdown document to create summary
#########################################################################


### 0 - Load setup environment ----

source(here::here("code", "00_setup-environment.R"))


### 1 - Knit markdown document ----

render(
  input = here("markdown", "summary.Rmd"),
  output_file = here("output", format(start_month, "%Y-%m"),
                     paste0(format(start_month, "%Y-%m"), "_summary.docx"))
)


### END OF SCRIPT ###