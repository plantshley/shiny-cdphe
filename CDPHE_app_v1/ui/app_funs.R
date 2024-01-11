#libraries
library(shiny)
library(purrr)
library(ggplot2)
library(dplyr)
library(plotly)
library(shinyWidgets)
library(fontawesome)
library(shinyjs)
library(bslib)
library(waiter) 
library(htmltools)
library(shinydashboard)
library(shinydashboardPlus)
#library(tidyverse)
#library(lubridate)
#library(knitr)
#library(grid)
#library(datawizard)
#library(reshape2)
library(scrollrevealR)
#library(fresh)
library(shinyBS)

#lists
vars <- c("Site Type" = "site_type", "Room Type" = "room_type", 
          "Season" = "season", "Building Leakiness" = "leakiness", 
          "Number of Air Handlers" = "ah_groups")
vars_mv <- c("Site Type" = "site_type", "Room Type" = "room_type", 
          "Season" = "season", "Building Leakiness" = "leakiness", 
          "Number of Air Handlers" = "ah_groups", "Day of Week" = "day_of_week", 
          "Time of Day" = "tod", "Hour of Day" = "hod")

room_types2 <-   c("Kitchen", "Dining", "Office", "Reception", "Classroom", "Meeting",
                   "Gathering", "Break Room" = "Breakroom", "Corridor", "Bedroom", "Storage", 
                   "Exam Room" = "Exam", "Medical Lab" = "Lab", "Other")

site_types <- c("Childcare", "Elderly Care", "Adult Care/Social Services" = "Adult Care", 
                "24-hr Shelters" = "Shelters", "Office Buildings" = "Offices", 
                "Food Services", "Religious Facilities" = "Religious")

leaky <- c("Leaky", "Moderate", "Tight")

seasons <- c("Winter", "Spring", "Summer", "Fall")

air_hands <- c("Less than 10", "10 to 25", "More than 25")

plot_types <- c("Hourly" = "1hr", "Daily" = "24hr", "Trend Line" = "trend", "Fluctuation Line" = "pattern")

plot_types_mv <- c("Cumulative Frequency Distribution" = "cfd", "Normalized Hourly" = "norm", 
                   "Scatter Plot" = "scat")

pols <- c("CO2" = "co2", "PM2.5" = "pm25", "TVOC" = "voc", "Temp" = "temp", "RH" = "RH")

dow_types <- c("Sunday", "Monday", "Tuesday", "Wednesday", 
               "Thursday", "Friday", "Saturday")

tod_types <- c("12am to 8am" = "12a-8a", "8am to 6pm" = "8a-6p", "6pm to 12am" = "6p-12a")

hod_types <- c("12am" = 0, "1am" = 1, "2am" = 2, "3am" = 3, "4am" = 4, "5am" = 5, "6am" = "6", "7am" = 7,
               "8am" = 8, "9am" = 9, "10am" = 10, "11am" = 11, "12pm" = 12, "1pm" = 13, "2pm" = 14,
               "3pm" = 15, "4pm" = 16, "5pm" = 17, "6pm" = 18, "7pm" = 19, "8am" = 20, "9pm" = 21,
               "10pm" = 22, "11pm" = 23)

#picker formatting fxn
pickerFormat01 <- function(hex, icon, label) {
  return(
  sprintf("<div
    style='background: %s; padding-left: 5px; padding-right: 5px; 
    padding-top: 1px; border-radius: 10px'>
    
    <i class='%s'></i></b></b> %s
   </div>", hex, icon, label)
  )
}
