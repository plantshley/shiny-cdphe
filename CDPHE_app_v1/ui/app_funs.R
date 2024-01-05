#libraries
library(shiny)
library(ggplot2)
library(plotly)
library(shinyWidgets)
library(fontawesome)
library(shinyjs)
library(bslib)
library(waiter)
library(htmltools)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyverse)
library(lubridate)
library(knitr)
library(grid)
library(rstudioapi)
library(datawizard)
library(reshape2)
library(scrollrevealR)
library(fresh)
library(DT)

#data
dataset2 <- readRDS(paste0(rstudioapi::getActiveProject(),"/CDPHE_app_v1/data/cdphe_Mega_hr_fullweek25.rds"))
dataset3 <- readRDS(paste0(rstudioapi::getActiveProject(),"/CDPHE_app_v1/data/cdphe_CFD_mega4.rds"))
dataset4 <- readRDS(paste0(rstudioapi::getActiveProject(),"/CDPHE_app_v1/data/cdphe_Mega_hr_scaled_full_melt2.rds")) #norm
dataset5 <- readRDS(paste0(rstudioapi::getActiveProject(),"/CDPHE_app_v1/data/cdphe_Mega_hr_scaled_full2.rds")) #scatter

#fivemin_1 <- readRDS(paste0(rstudioapi::getActiveProject(),"/data/5-min data/cdphe_MONGO_5min_scaled.rds"))
#fivemin_2 <- readRDS(paste0(rstudioapi::getActiveProject(),"/data/5-min data/cdphe_MONGO_5min_scaled_melt.rds"))
#fivemin_3 <- readRDS(paste0(rstudioapi::getActiveProject(),"/data/5-min data/cdphe_MONGO_5min.rds"))
#fivemin_4 <- readRDS(paste0(rstudioapi::getActiveProject(),"/data/5-min data/cdphe_MONGO_5min_melt.rds"))

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

#univariable tab fxn 
univ_tab <- function(tab_id) {
  
  tabPanel(value = tab_id, id = tab_id, 
           wellPanel(id = "top-well", 
                     
                     fluidRow(
                       column(
                         prettyRadioButtons(
                           inputId = "plot_type",
                           label = "Choose plot type:", 
                           choices = plot_types,
                           selected = "1hr", 
                           icon = icon("meteor","fa-xl", lib = "font-awesome"), 
                           bigger = TRUE,
                           status = "info",
                           animation = "jelly", 
                           inline = TRUE), 
                         width = 5), 
                       
                       column(
                         pickerInput("var", label = "Choose variable:", 
                                     choices = vars, selected = "site_type",
                                     choicesOpt = list(
                                       content = c(
                                         pickerFormat01("#E7BEF9", "fa fa-star fa-sm", "Site Type"),
                                         pickerFormat01("#F2BEF2", "fa fa-heart fa-sm", "Room Type"),
                                         pickerFormat01("#FFC9EF", "fa fa-cloud fa-xs", "Season"),
                                         pickerFormat01("#FFCEDA", "fa-solid fa-diamond fa-sm", "Leakiness"),
                                         pickerFormat01("#FFCFC5", "fa fa-clover", "Air Handlers"))),
                                     options = pickerOptions(container = "body", width = "fit", iconBase = "fas")),
                         width = 4, offset = 0)),
                     
                     fluidRow(
                       
                       column(  
                         prettyRadioButtons(
                           "colors", "Choose color scheme:", inline = TRUE,
                           choices = c("Default", "Rainbow", "Colorblind-Friendly"),
                           icon = icon("droplet", "fa-2xs", lib = "font-awesome"), 
                           bigger = FALSE,
                           status = "info",
                           animation = "jelly"
                         ), width = 5),
                       
                       column(
                         pickerInput("second_in", label = "Sub-variables", choices = NULL, 
                                     multiple =TRUE, options = pickerOptions(container = "body", iconBase = "fas",
                                                                             selectedTextFormat = "count >1",
                                                                             tickIcon = FALSE, width = "fit", inline = TRUE)), width = 4, offset = 0)),
                     
                     fluidRow( align = "center",
                               
                               actionBttn("go", "Generate Plots", style = "pill", icon("rocket","fa-lg", lib = "font-awesome")))
           ),
           
           plotlyOutput("plot"),
           
           wellPanel(id = "bottom-well", 
                     sliderInput("hour", "Choose hour range within the week:", min = 0, 
                                 max = 167, 
                                 animate = F, 
                                 value = c(0,167), step = 8, width = "100%"))
  )
}

#1-day average fxn
d_avg <- function(data){
  data %>%
    group_by(site_id, dow, id_room, day_of_week) %>%
    mutate(across(c(co2, pm25, voc, temp, RH), ~ mean(.x, na.rm = TRUE)))%>%
    slice(1)
  
}
  


