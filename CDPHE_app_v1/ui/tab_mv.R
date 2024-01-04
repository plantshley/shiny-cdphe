## UI 

tabPanel(value = "mv_tab", id = "mv_tab", title = "mv",
         wellPanel(id = "top-well_mv", 
                   
                   fluidRow(
                     column(
                       prettyRadioButtons(
                         inputId = "plot_type_mv",
                         label = "Choose plot type:", 
                         choices = plot_types_mv,
                         selected = "cfd", 
                         icon = icon("meteor","fa-xl", lib = "font-awesome"), 
                         bigger = TRUE,
                         status = "info",
                         animation = "jelly", 
                         inline = TRUE), 
                       width = 5), 
                     
                     column(
                       pickerInput("var_mv", label = "Choose variable:", 
                                   choices = vars_mv, selected = "site_type",
                                   choicesOpt = list(
                                     content = c(
                                       pickerFormat01("#E7BEF9", "fa fa-star fa-sm", "Site Type"),
                                       pickerFormat01("#F2BEF2", "fa fa-heart fa-sm", "Room Type"),
                                       pickerFormat01("#FFC9EF", "fa fa-cloud fa-xs", "Season"),
                                       pickerFormat01("#FFCEDA", "fa-solid fa-moon fa-sm", "Leakiness"),
                                       pickerFormat01("#FFCFC5", "fa fa-clover", "Air Handlers"), 
                                       pickerFormat01("#FFE2BA", "fa fa-solid fa-calendar-week", "Day of Week"), 
                                       pickerFormat01("#F9EEAB", "fa fa-solid fa-clock", "Time of Day"),
                                       pickerFormat01("#E5EFAB", "fa fa-solid fa-clock", "Hour of Day"))),
                                   options = pickerOptions(container = "body", width = "fit", iconBase = "fas")),
                       width = 4, offset = 0)),
                   
                   fluidRow(
                     
                     column(  
                       prettyRadioButtons(
                         "colors_mv", "Choose color scheme:", inline = TRUE,
                         choices = c("Default", "Rainbow", "Colorblind-Friendly"),
                         icon = icon("droplet", "fa-2xs", lib = "font-awesome"), 
                         bigger = FALSE,
                         status = "info",
                         animation = "jelly"
                       ), width = 5),
                     
                     column(
                       pickerInput("second_in_mv", label = "Sub-variables", choices = NULL, 
                                   multiple =FALSE, options = pickerOptions(container = "body", iconBase = "fas",
                                                                           selectedTextFormat = "count >1",
                                                                           tickIcon = FALSE, width = "fit", inline = TRUE)), 
                       width = 4, offset = 0)),
                     
                   fluidRow(align = "center",
        
                             actionBttn("go_mv", "Generate Plots", style = "pill", icon("rocket","fa-lg", lib = "font-awesome")))
         ),
         
         wellPanel(id = "plot-well_mv", width = "90%",
                   fluidRow(id = "mv_plot_pols",
                            uiOutput("plot_pols")),
                   fluidRow(id = "mv_plot_row", 
                     column(width = 4, 
                       checkboxGroupButtons(
                       inputId = "pol",
                       choices = pols,
                       selected = c("co2", "pm25"), 
                       individual = TRUE,
                       checkIcon = list(
                         yes = tags$i(class = "fa fa-circle", 
                                      style = "color: #DF82E2"),
                         no = tags$i(class = "fa fa-circle-o", 
                                     style = "color: #DF82E2"))
                     )),
                     column(width = 8,
                       uiOutput("plot_title"))
                     ),
                   fluidRow(
                     id = "mv_site",
                     align = "right",
                     column(width = 10,
                            conditionalPanel(
                              condition = "output.plot_mv !== undefined && output.plot_mv !== null",
                              searchInput(
                                inputId = "siteID",
                                label = "Filter for Site(s)",
                                placeholder = "ex. 005a, 077",
                                btnSearch = icon("wand-magic-sparkles"),
                                btnReset = icon("circle-xmark")
                              )
                            )
                     )
                   ),
                   
                           

                   plotlyOutput("plot_mv")),
         br()
         
)


