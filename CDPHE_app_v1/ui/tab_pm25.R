
## UI 

tabPanel(value = "pm25_tab", id = "pm25_tab", title = "PM2.5",
  wellPanel(id = "top-well", 
            
    fluidRow(
      column(
        prettyRadioButtons(
          inputId = "plot_type2",
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
        pickerInput("var2", label = "Choose variable:", 
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
                          "colors2", "Choose color scheme:", inline = TRUE,
                          choices = c("Default", "Rainbow", "Colorblind-Friendly"),
                          icon = icon("droplet", "fa-2xs", lib = "font-awesome"), 
                          bigger = FALSE,
                          status = "info",
                          animation = "jelly"
                        ), width = 5),
                      
                      column(
                        pickerInput("second_in2", label = "Sub-variables", choices = NULL, 
                                    multiple =TRUE, options = pickerOptions(container = "body", iconBase = "fas",
                                                                            selectedTextFormat = "count >1",
                                                                            tickIcon = FALSE, width = "fit", inline = TRUE)), width = 4, offset = 0)),
                    
                    fluidRow( align = "center",
                              
                              actionBttn("go2", "Generate Plots", style = "pill", icon("rocket","fa-lg", lib = "font-awesome")))
         ),
         
         plotlyOutput("plot2"),
         
         wellPanel(id = "bottom-well", 
                   sliderInput(id = "h2", inputId = "hour2", "Choose hour range within the week:", min = 0, 
                               max = 167, 
                               animate = F, 
                               value = c(0,167), step = 8, width = "100%"))
)


## Server function
