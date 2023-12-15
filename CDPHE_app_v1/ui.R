source(paste0(rstudioapi::getActiveProject(),"/ui/app_funs.R"))

### UI Start ###

fluidPage(
  
  includeCSS(paste0(getActiveProject(), "/www/css_cdphe.css")),

  #tags$head(
    #tags$link(rel = "stylesheet", type = "text/css", href = paste0(getwd(), "/www/css_cdphe.css"))),

  waiter::useWaiter(), 
  # Dynamic Title
  uiOutput("title"), 
  
  #Sidebar layout  
  
  sidebarLayout(
    sidebarPanel(id="sidebar", width = 1,
                 actionBttn("home", "", icon = icon("house", lib = "font-awesome")),           
                 actionBttn("co2", "CO2"),
                 actionBttn("pm25", "PM2.5"),
                 actionBttn("voc", "TVOC"),
                 actionBttn("temp", "Temp"),
                 actionBttn("rh", "RH"),
                 actionBttn("mv", "ALL"), 
                 actionBttn("glossary", "", icon = icon("book", lib = "font-awesome"))
    ),
    
  mainPanel(id = "main", width = 11,
            
    tabsetPanel(id = "tabs", selected = "home_tab",
                
      source(paste0(getActiveProject(), "/ui/tab_home.R"))$value,
      source(paste0(getActiveProject(), "/ui/tab_univ.R"))$value,
      source(paste0(getActiveProject(), "/ui/tab_mv.R"))$value,
      source(paste0(getActiveProject(), "/ui/tab_gloss.R"))$value
      )
    )
  )
)