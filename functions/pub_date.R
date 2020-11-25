#########################################################################
# Name of file - pub_date.R
# Data release - Monthly Delayed Discharges publication
# Original Authors - Alice Byers
# Orginal Date - July 2020
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Server
# Version of R - 3.6.1
#
# Description - Function to derive publication date
#########################################################################


pub_date <- function(start_month){
  
  if(!lubridate::is.Date(start_month)){
    stop("start_month must be of Date class.")
  }
  
  if(lubridate::day(start_month) != 1){
    stop("start_month must be first day in month.")
  }
  
  `%>%` <- magrittr::`%>%`
  
  start_pub_month <- start_month + months(2)
  
  end_pub_month <-
    lubridate::ceiling_date(start_pub_month, "month") - lubridate::days(1)
  
  # Publication date is first Tuesday of month, two months after
  # reporting month. Date is second Tuesday if publication month is January.
  rule <- 
    almanac::monthly() %>%
    almanac::recur_on_wday(
      "Tuesday", 
      nth = dplyr::if_else(month(start_month) == 11, 2, 1)
    )
  
  almanac::alma_search(start_pub_month, end_pub_month, rule)
  
}


### END OF SCRIPT ###