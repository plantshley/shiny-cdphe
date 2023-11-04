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

#data and lists
dataset2 <- readRDS(paste0(rstudioapi::getActiveProject(),"/data/cdphe_Mega_hr_fullweek23.rds"))

vars <- c("Site Type" = "site_type", "Room Type" = "room_type", 
          "Season" = "season", "Building Leakiness" = "leakiness", 
          "Number of Air Handlers" = "ah_groups")

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

#header java
jsCode <- "
shinyjs.initializeHeader = function() {
  var currentSlide = 0;
  var slides = $('.header-slide');
  var totalSlides = slides.length;

  function showSlide(index) {
    slides.hide();
    slides.eq(index).fadeIn();
  }

  // Show the initial slide
  showSlide(currentSlide);

  // Arrow click events
  $('#right-arrow').click(function() {
    currentSlide = (currentSlide + 1) % totalSlides;
    showSlide(currentSlide);
  });

  $('#left-arrow').click(function() {
    currentSlide = (currentSlide - 1 + totalSlides) % totalSlides;
    showSlide(currentSlide);
  });
}
"
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
  


