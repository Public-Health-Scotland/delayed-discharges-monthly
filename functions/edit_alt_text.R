#########################################################################
# Name of file - edit_alt_text.R
# Data release - Monthly Delayed Discharges publication
# Original Authors - Alice Byers
# Orginal Date - December 2020
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
  
  # Create alt text file for current month
  
  if(
    # File already exists
    file.exists(
      here::here("output", format(start_month, "%Y-%m"), 
           paste0(format(start_month, "%Y-%m"), "_alt-text.txt")))
  ){
    print(paste0(format(start_month, "%Y-%m"), "_alt-text.txt ",
                 "already exists."))
  }else if(
    # File exists for previous month
    file.exists(
      here::here("output", format(start_month - months(1), "%Y-%m"), 
           paste0(format(start_month - months(1), "%Y-%m"), "_alt-text.txt"))
    )){
      # Copy file from previous month
    print("Copying alt text file from previous month.")
    invisible(file.copy(
      here::here("output", format(start_month - months(1), "%Y-%m"), 
           paste0(format(start_month - months(1), "%Y-%m"), "_alt-text.txt")),
      here::here("output", format(start_month, "%Y-%m"), 
           paste0(format(start_month, "%Y-%m"), "_alt-text.txt"))
    ))
  }else{
    # Create blank text file
    print("Creating blank text file for alt text.")
    invisible(file.create(
      here::here("output", format(start_month, "%Y-%m"), 
           paste0(format(start_month, "%Y-%m"), "_alt-text.txt")))
    )
  }
  
  # Open alt text file to edit for current month
  file.edit(here::here("output", format(start_month, "%Y-%m"), 
                 paste0(format(start_month, "%Y-%m"), "_alt-text.txt")))
  
}


### END OF SCRIPT ###