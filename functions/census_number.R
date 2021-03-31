#########################################################################
# Name of file - census_number.R
# Data release - Monthly Delayed Discharges publication
# Original Authors - Alice Byers
# Original Date - July 2020
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Server
# Version of R - 3.6.1
#
# Description - Function to derive census number
#########################################################################


census_number <- function(start_month){
  
  if(!lubridate::is.Date(start_month)){
    stop("start_month must be of Date class.")
  }
  
  if(lubridate::day(start_month) != 1){
    stop("start_month must be first day in month.")
  }
  
  lubridate::time_length(
    lubridate::interval(lubridate::dmy(01072016), start_month),
    "months"
  ) + 128
  
}


### END OF SCRIPT ###