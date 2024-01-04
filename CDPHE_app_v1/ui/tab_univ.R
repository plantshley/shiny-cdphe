## UI 

tabPanel(value = "uni_tab", id = "uni_tab", title = "uni",
         wellPanel(id = "top-well", 
                   
                   fluidRow(
                     column(
                       prettyRadioButtons(
                         inputId = "plot_type",
                         label = "Choose plot type:", 
                         choices = plot_types,
                         selected = "1hr", 
                         icon = icon("meteor","fa-xl", lib = "font-awesome"), 
                         bigger = F,
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
                                       pickerFormat01("#FFCEDA", "fa-solid fa-moon", "Leakiness"),
                                       pickerFormat01("#FFCFC5", "fa fa-crown fa-sm", "Air Handlers"))),
                                   options = pickerOptions(container = "body", width = "fit", iconBase = "fas")),
                       width = 4, offset = 0)),
                   
                   fluidRow(
                     
                     column(  
                       prettyRadioButtons(
                         "colors", "Choose color scheme:", inline = TRUE,
                         choices = c("Default", "Rainbow", "Colorblind-Friendly"),
                         icon = icon("paintbrush", "fa-xl",lib = "font-awesome"), 
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
         
         wellPanel(id = "plot-well",
                   fluidRow(
                     id = "mv_site",
                     align = "right",
                     column(width = 10,
                            conditionalPanel(
                              condition = "output.plot !== undefined && output.plot !== null",
                              searchInput(
                                inputId = "siteIDu",
                                label = "Filter for Site(s)",
                                placeholder = "ex. 005a, 077",
                                btnSearch = icon("wand-magic-sparkles"),
                                btnReset = icon("circle-xmark")
                              )
                            )
                     )
                   ),
          plotlyOutput("plot")
         ),
         fluidRow(
         wellPanel(id = "bottom-well", 
                   sliderInput("hour", "Choose hour range within the week:", min = 0, 
                               max = 167, 
                               animate = F, 
                               value = c(0,167), step = 8, width = "90%"))),
         br()
)


## Server function
