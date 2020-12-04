#########################################################################
# Name of file - archive_data.R
# Data release - Monthly Delayed Discharges publication
# Original Authors - Alice Byers
# Orginal Date - December 2020
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Server
# Version of R - 3.6.1
#
# Description - Function to archive data folders for previous 
#               financial year
#########################################################################


archive_data <- function(fy){
  
  if(!is.character(fy)){
    stop("Financial year must be provided in character format.")
  }
  
  if(!stringr::str_detect(fy, "^\\d{4}/\\d{2}$")){
    stop("Financial year must be provided in format YYYY/YY.")
  }
  
  fy_folder <- paste0(stringr::str_replace(fy, "/", "-"), ".zip")
  
  if(fy_folder %in% list.files(here::here("data"))){
    stop(paste("Folder for", fy, "already exists."))
  }
  
  `%>%` <- magrittr::`%>%`
  
  # Find monthly data folders in financial year
  months_to_archive <-
    tibble::tibble(x = list.files(here::here("data"))) %>%
    dplyr::filter(
      phsmethods::fin_year(lubridate::ymd(paste0(x, "-01"))) == fy
    ) %>%
    dplyr::pull(x) %>%
    suppressWarnings()
  
  if(length(months_to_archive) == 0){
    stop("No data folders exist for months in this financial year.")
  }
  
  # Add all months into zipped folder for financial year
  zip::zipr(
    here::here("data", fy_folder),
    here::here("data", months_to_archive)
  )
  
  # Delete folders that have been archived
  unlink(here::here("data", months_to_archive), recursive = TRUE)
  
}


### END OF SCRIPT ###