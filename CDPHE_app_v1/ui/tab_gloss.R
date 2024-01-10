#Glossary stuff

tabPanel(value = "glossary tab", title = "glossary tab", 
         
br(), 
useShinyjs(),



fluidRow(id = "gloss_row1",
         
wellPanel(id = "gloss_well", width = "100%", 
          
      searchInput(
            inputId = "search", label = "Search for Term",
            placeholder = "Type Here (ﾉ◕ヮ◕)ﾉ*:･ﾟ✧	                 ",
            btnSearch = icon("rocket"),
            btnReset = icon("circle-xmark"),
            width = "450px"
          ),
      br(),
          
      accordion(id = "accordion1",
  
        accordionItem(
          title = "IAQ Indictors",
          collapsed = T,
          
          tags$img(src = "iaq_ids.png", id = "a1", 
                   style = "width: 70%; margin-left: 30px"), 
          br(),
          "CO2", "PM2.5","TVOC", "Temp", "RH", "relative humidity"
          
        ),
        
        accordionItem(
          title = "IAQ Indicator Units of Measure",
          collapsed = T,
          
          tags$img(src = "iaq_un.png", id = "a2", 
                   style = "width: 70%; margin-left: 30px"),
          br(),
          "ppb","ppm","microgram","µg/m", "ug/m", "celsius", "degrees"
        ), 
        
        accordionItem(
          title = "Plot Types (single-indicator tab)",
          collapsed = T,
          
          tags$img(src = "plots_uni.png", id = "a3", 
                   style = "width: 70%; margin-left: 30px"),
          br(),
          "hourly", "daily", "trend","line", "fluctuation", "daily averaged", "hourly averaged"
      
        ),
        
        accordionItem(
          title = "Plot Types (multi-indicator tab)",
          collapsed = T,
          
          tags$img(src = "plots_mv.png", id = "a4", 
                   style = "width: 70%; margin-left: 30px"),
          br(),
          "CDF","scatter","cumulative distribution frequency", "normalized", "scale","time series", 
          "normal", "norm", "normalized hourly","normalized hourly plot", "normal hourly"
        ),
        
        accordionItem(
          title = "Room Types",
          collapsed = T,
          
          tags$img(src = "vars_room.png", id = "a5", 
                   style = "width: 70%; margin-left: 30px"),
          br(),
          "kitchen","dining","storage","office","restroom","reception",
          "lab","corridor","classroom","meeting","breakroom","bedroom",
          "garage","gathering","basement","exam","entry","other"
          
        ),
        
        accordionItem(
          title = "Site Types",
          collapsed = T,
          
          tags$img(src = "vars_site.png", id = "a6", 
                   style = "width: 70%; margin-left: 30px"),
          br(),
          "childcare","elderly care","residential","food services","shelters",
          "offices","medical","adult care","religious", "other"
          
        ),
        
        accordionItem(
          title = "Other Variables",
          collapsed = T,
          
          tags$img(src = "vars_other.png", id = "a7", 
                   style = "width: 70%; margin-left: 30px"),
          br(),
          "leaky","moderate","tight","leakiness","number of air handlers",
          "air handlers", "hour of day", "time of day", "day of week", "season"
          
        ))
      ))
)