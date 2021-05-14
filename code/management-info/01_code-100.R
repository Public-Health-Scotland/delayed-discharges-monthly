#########################################################################
# Name of file - 01_code-100.R
# Data release - Monthly Delayed Discharges Management Information
# Original Authors - Peter McClurg
# Original Date - December 2020
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Server
# Version of R - 3.6.1
#
# Description - Create code 100 management information
#########################################################################


### 0 - Load setup environment ----

source(here::here("code", "00_setup-environment.R"))


### 1 - Read in trend file ----

trend <- read_rds(
  here("trend", paste0(format(start_month, "%Y-%m"), "_trend.rds"))
)

# Record full list of HB/LA to complete data later
all_hb <- unique(trend$health_board)
all_la <- unique(trend$local_authority)


### 2 - Select census delays for latest month and code 100 only ----

trend %<>%
  filter(month_date == start_month & 
           census_flag == 1 & 
           delay_reason_1 == "100")


### 3 - Aggregate and restructure ----

code_100_hb <-
  
  # Aggregate by HB and Specialty
  trend %>%
      group_by(health_board, specialty_desc) %>% 
      summarise(census_total = n(),
                .groups = "drop") %>%
  arrange(specialty_desc) %>%
  
  # Restructure so column for each specialty 
  pivot_wider(names_from = specialty_desc, 
              values_from = census_total,
              values_fill = list(census_total = 0)
  ) %>%
  
  # Make sure row for every health board
  complete(health_board = all_hb) %>%
  
  # Replace missing values with zeros
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  
  # Add total column and row
  mutate(Total = rowSums(select(., -health_board))) %>%
  adorn_totals(name = "Scotland")
  

code_100_la <-
  
  # Aggregate by LA and Specialty
  trend %>%
  group_by(local_authority, specialty_desc) %>% 
  summarise(census_total = n(),
            .groups = "drop") %>%
  arrange(specialty_desc) %>% 
  
  # Restructure so column for each specialty 
  pivot_wider(names_from = specialty_desc, 
              values_from = census_total,
              values_fill = list(census_total = 0)
  ) %>%
  
  # Make sure row for every local authority
  complete(local_authority = all_la) %>%
  
  # Replace missing values with zeros
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  
  # Add total column and row
  mutate(Total = rowSums(select(., -local_authority))) %>%
  adorn_totals(name = "Scotland")
    

### 4 - Save data to excel template ----

# Define styles for workbook

header_style <-
  createStyle(
    fontName = "Arial",
    fontSize = 9, 
    textDecoration = "bold", 
    fgFill = "#99CCFF",
    border = c("top", "bottom", "left", "right"),
    halign = "center",
    valign = "center",
    wrapText = TRUE
  )

body_style <-
  createStyle(
    fontName = "Arial",
    fontSize = 9, 
    border = c("top", "bottom", "left", "right")
  )

bold_style <-
  createStyle(
    textDecoration = "bold"
  )

# Load excel template and rename worksheet

wb <- loadWorkbook(here("templates", "code-100-template.xlsx"))

renameWorksheet(
  wb,
  "Sheet1",
  format(start_month, "%b %Y")
)

# Write month/year lookup
writeData(
  wb,
  sheet = 1,
  x = format(start_month, "'%B %Y"),
  startCol = "A",
  startRow = 4
)

# Write Health Board data
writeData(
  wb,
  sheet = 1,
  x = select(code_100_hb, -health_board),
  colNames = TRUE,
  headerStyle = header_style,
  startCol = "C",
  startRow = 8
)

# Write Local Authority data
writeData(
  wb,
  sheet = 1,
  x = select(code_100_la, -local_authority),
  colNames = TRUE,
  headerStyle = header_style,
  startCol = "C",
  startRow = 28
)

# Add styles

max_cols <- max(ncol(code_100_hb), ncol(code_100_la))

# Table body
addStyle(
  wb,
  sheet = 1,
  style = body_style,
  rows = c(9:23, 29:62),
  cols = 3:(max_cols + 1),
  gridExpand = TRUE,
  stack = TRUE
)

# Bold total rows
addStyle(
  wb,
  sheet = 1,
  style = bold_style,
  rows = c(23, 62),
  cols = 3:(max_cols + 1),
  gridExpand = TRUE,
  stack = TRUE
)

# Bold total columns
addStyle(
  wb,
  sheet = 1,
  style = bold_style,
  rows = c(9:23, 29:62),
  cols = max_cols + 1,
  gridExpand = TRUE,
  stack = TRUE
)

# Save workbook

saveWorkbook(
  wb,
  here("output", year(pub_date(start_month)), pub_date(start_month), 
       "management-info", 
       paste0(format(start_month, "%Y-%m"),"_code-100.xlsx")),
  overwrite = TRUE
)


### END OF SCRIPT ###