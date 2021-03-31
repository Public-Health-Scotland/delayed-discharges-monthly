#########################################################################
# Name of file - census_date.R
# Data release - Monthly Delayed Discharges publication
# Original Authors - Alice Byers
# Original Date - July 2020
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Server
# Version of R - 3.6.1
#
# Description - Function to derive census date
#########################################################################


census_date <- function(start_month){
  
  if(!lubridate::is.Date(start_month)){
    stop("start_month must be of Date class.")
  }
  
  if(lubridate::day(start_month) != 1){
    stop("start_month must be first day in month.")
  }
  
  `%>%` <- magrittr::`%>%`
  
  end_month <-
    lubridate::ceiling_date(start_month, "month") - lubridate::days(1)
  
  # Census date is last Thursday of the month
  rule <- 
    almanac::monthly() %>%
      almanac::recur_on_wday("Thursday", nth = -1)
  
  almanac::alma_search(start_month, end_month, rule)
  
}


### END OF SCRIPT ###