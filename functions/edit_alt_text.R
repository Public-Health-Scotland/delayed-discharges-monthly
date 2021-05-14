#########################################################################
# Name of file - edit_alt_text.R
# Data release - Monthly Delayed Discharges publication
# Original Authors - Alice Byers
# Original Date - December 2020
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Server
# Version of R - 3.6.1
#
# Description - Function to create text file to store alt text for 
#               summary/twitter chart and open file to edit.
#########################################################################


edit_alt_text <- function(start_month){
  
  if(!lubridate::is.Date(start_month)){
    stop("start_month must be of Date class.")
  }
  
  if(lubridate::day(start_month) != 1){
    stop("start_month must be first day in month.")
  }
  
  if(!exists("pub_date") || !inherits(pub_date, "function")){
    stop(paste0(
      "The pub_date function must be loaded to run the ",
      "`edit_alt_text` function. \n",
      "To do this, run the following code: ",
      "`source(here::here('functions', 'pub_date.R'))`"))
  }
  
  current_pub_date <- pub_date(start_month)
  prev_pub_date <- pub_date(start_month - months(1))
  
  # Create alt text file for current month
  
  if(
    # File already exists
    file.exists(
      here::here("output", lubridate::year(current_pub_date), 
                 current_pub_date, "publication",
                 paste0(current_pub_date, "_alt-text.txt")))
  ){
    print(paste0(current_pub_date, "_alt-text.txt already exists."))
  }else if(
    # File exists for previous month
    file.exists(
      here::here("output", lubridate::year(prev_pub_date), 
                 prev_pub_date, "publication",
                 paste0(prev_pub_date, "_alt-text.txt"))
    )){
      # Copy file from previous month
    print("Copying alt text file from previous month.")
    invisible(file.copy(
      here::here("output", lubridate::year(prev_pub_date),
                 prev_pub_date, "publication",
                 paste0(prev_pub_date, "_alt-text.txt")),
      here::here("output", lubridate::year(current_pub_date),
                 current_pub_date, "publication",
                 paste0(current_pub_date, "_alt-text.txt"))
    ))
  }else{
    # Create blank text file
    print("Creating blank text file for alt text.")
    invisible(file.create(
      here::here("output", lubridate::year(current_pub_date),
                 current_pub_date, "publication",
                 paste0(current_pub_date, "_alt-text.txt")))
    )
  }
  
  # Open alt text file to edit for current month
  file.edit(here::here("output", lubridate::year(current_pub_date),
                       current_pub_date, "publication",
                       paste0(current_pub_date, "_alt-text.txt")))
  
}


### END OF SCRIPT ###