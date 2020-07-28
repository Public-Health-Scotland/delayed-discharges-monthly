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
  
  rule <- 
    almanac::monthly() %>%
      almanac::recur_on_wday("Tuesday", nth = 1)
  
  almanac::alma_search(start_pub_month, end_pub_month, rule)
  
}