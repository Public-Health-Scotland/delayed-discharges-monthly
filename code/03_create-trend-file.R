#########################################################################
# Name of file - 03_create-trend-file.R
# Data release - Monthly Delayed Discharges publication
# Original Authors - Alice Byers
# Orginal Date - November 2020
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Server
# Version of R - 3.6.1
#
# Description - Add Scotland file for latest month to trend file
#########################################################################


### 0 - Load setup environment ----

source(here::here("code", "00_setup-environment.R"))


### 1 - Read in Scotland file and previous trend file ----

scotland <-
  read_rds(
    here("data", format(start_month, "%Y-%m"),
         paste0(format(start_month, "%Y-%m"), "_scotland.rds"))
  )

# Check that previous months trend file exists
if(!file.exists(
  here("trend", 
       paste0(format(start_month - months(1), "%Y-%m"), "_trend.rds"))
)){
  paste("The trend file for", format(start_month - months(1), "%B %Y"),
        "does not exist or has been archived. To continue running this",
        "script, the following file must exist in the trend folder:",
        format(start_month - months(1), "%Y-%m_trend.rds"))
}

trend <-
  here("trend", 
       paste0(format(start_month - months(1), "%Y-%m"), "_trend.rds")) %>%
  read_rds()


### 2 - Add latest month Scotland file to previous month trend ----

trend %<>%
  bind_rows(scotland)


### 3 - Match lookup files ----

trend %<>%
  
  # Remove existing matched variables
  select(-any_of(c("data_zone", "hscp", "locality", "location_name",
                   "health_board_code", "local_authority_code",
                   "specialty_desc"))) %>%
  
  # Match on new matched variables
  
  # Datazone
  left_join(pc_lookup(), by = c("patient_postcode" = "pc7")) %>%
  relocate(data_zone, .after = patient_postcode) %>%
  
  # HSCP and Locality
  left_join(hscp_locality_lookup(), by = "data_zone") %>%
  relocate(hscp, locality, .after = local_authority) %>%
  
  # Location
  left_join(location_lookup(), by = "location_code") %>%
  relocate(location_name, .after = location_code) %>%
  
  # Health Board and Local Authority codes
  left_join(hb_lookup(), by = "health_board") %>%
  left_join(la_lookup(), by = "local_authority") %>%
  relocate(health_board_code, .after = health_board) %>%
  relocate(local_authority_code, .after = local_authority) %>%
  
  # Specialty description
  left_join(spec_lookup(), by = "specialty_code") %>%
  relocate(specialty_desc, .after = specialty_code)


### 4 - Save file ----

write_rds(
  trend,
  here("trend", 
       paste0(format(start_month, "%Y-%m"), "_trend.rds")),
  compress = "gz"
)

write_csv(
  trend,
    here("trend", 
         paste0(format(start_month, "%Y-%m"), "_trend.csv"))
)


### 5 - Archive previous months trend file ----

# Save file to archive.zip
if("archive.zip" %in% list.files(here("trend"))){
  zipr_append(
    here("trend", "archive.zip"),
    here("trend", 
         paste0(format(start_month - months(1), "%Y-%m"), "_trend.rds")))
}else{
  zipr(
    here("trend", "archive.zip"),
    here("trend", 
         paste0(format(start_month - months(1), "%Y-%m"), "_trend.rds")))
}

# Delete file from main folder
unlink(
  here("trend", 
       paste0(format(start_month - months(1), "%Y-%m"), 
              "_trend", c(".rds", ".csv")))
)


### END OF SCRIPT ###