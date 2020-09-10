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
  
  rule <- 
    almanac::monthly() %>%
      almanac::recur_on_wday("Thursday", nth = -1)
  
  almanac::alma_search(start_month, end_month, rule)
  
}