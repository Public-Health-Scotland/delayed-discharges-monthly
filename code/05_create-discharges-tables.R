#########################################################################
# Name of file - 05_create-discharges-tables.R
# Data release - Monthly Delayed Discharges publication
# Original Authors - Alice Byers
# Original Date - April 2020
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Server
# Version of R - 3.6.1
#
# Description - Create discharges excel tables
#########################################################################


### 0 - Load setup environment ----

source(here::here("code", "00_setup-environment.R"))


### 1 - Read in trend file ----

trend <-
  read_rds(
    here("trend", 
         paste0(format(start_month, "%Y-%m"), "_trend.rds"))
  ) %>%
  
  # Remove Code 100's and data prior to April 2018
  filter(delay_reason_1 != 100 &
           month_date >= dmy(01042018))


### 2 - Data wrangling ----

discharges_hb <-
  
  trend %>%
  
  # Select discharges within month
  filter(disch_in_month == 1) %>%
  
  # Exclude deaths and not fit for discharge
  filter(!discharge_reason %in% 4:5) %>%
  
  # Add discharge reason description
  mutate(discharge_reason = 
           case_when(
             discharge_reason == 1 ~ "Placement",
             discharge_reason %in% 2:3 ~ "Home"
           )) %>%
  
  # Aggregate and restructure
  group_by(month_date, health_board, discharge_reason) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(values_from = n, names_from = discharge_reason, 
              values_fill = list(n = 0)) %>%
  mutate(total = reduce(select(., Home:Placement), `+`)) %>%
  
  # Complete for all health boards
  complete(month_date, health_board = unique(trend$health_board), 
           fill = list(home = 0, placement = 0, total = 0)) %>%
  
  # Add Scotland rows
  group_by(month_date) %>%
  group_modify(~ adorn_totals(., name = "Scotland")) %>%
  ungroup()


### 3 - Create excel output using template ----

# Read in template
wb <- loadWorkbook(here("templates", "discharges-template.xlsx"))

# Write publication webpage link to guide tab
link <- 
  paste0("https://beta.isdscotland.org/find-publications-and-data/",
         "health-and-social-care/delayed-discharges/",
         "delayed-discharges-in-nhsscotland-monthly/",
         format(pub_date(start_month), "%e-%B-%Y") %>% str_trim())
names(link) <- "Link to metadata document"
class(link) <- "hyperlink"

writeData(wb, 
          "Guide", 
          startCol = "J",
          startRow = 5,
          x = link)

# Lookup tab - List of months
months <-
  discharges_hb %>%
  count(month_date) %>%
  mutate(month = format(month_date, "%B %Y"),
         n = row_number()) %>%
  select(month, n)

writeData(
  wb,
  "lookup",
  startCol = "A",
  startRow = 1,
  months,
  name = "months",
  colNames = FALSE
)

# Lookup tab - List of FYs
fy <-
  discharges_hb %>%
  count(fy = fin_year(month_date)) %>%
  mutate(n = row_number()) %>%
  select(n, fy)

writeData(
  wb,
  "lookup",
  startCol = "E",
  startRow = 1,
  fy,
  name = "fy",
  colNames = FALSE
)

writeData(
  wb,
  "lookup",
  startCol = "G",
  startRow = 1,
  fy[, 2],
  name = "fy_list",
  colNames = FALSE
)

# Lookup tab - Latest month in various formats
latest_dates <-
  tibble(
    x = c(format(start_month, "%B"),
          fin_year(start_month),
          year(start_month),
          format(start_month, "%B %Y"))
  )

writeData(
  wb,
  "lookup",
  startCol = "I",
  startRow = 2,
  latest_dates,
  colNames = FALSE
)

# Data tab
data_tab <-
  discharges_hb %>%
  mutate(lookup = paste0(format(month_date, "%B %Y"), health_board), 
         .before = everything())

writeData(
  wb,
  "data",
  startCol = "A",
  startRow = 1,
  data_tab,
  name = "data"
)

# Chart data tab - Data for latest 25 months and Scotland only
chart_data <-
  discharges_hb %>%
  filter(health_board == "Scotland" &
           between(month_date, start_month - months(24), start_month)) %>%
  mutate(month_date = format(month_date, "%b %y")) %>%
  select(month_date, Home, Placement)

writeData(
  wb,
  "chart_data",
  startCol = "A",
  startRow = 1,
  chart_data,
  name = "chartdata"
)

# Hide data and lookup tabs
sheetVisibility(wb)[4:6] <- "hidden"

# Save excel workbook
saveWorkbook(
  wb,
  here("output", year(pub_date(start_month)),
       pub_date(start_month), "publication",
       paste0(pub_date(start_month), "_discharges-tables.xlsx")),
  overwrite = TRUE
)

# TO DO: The only manual change required to the resulting excel workbook is to
# change the drop down selections to the latest FY and month in both tabs
# and save.


### END OF SCRIPT ###