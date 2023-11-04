# Home tab

tabPanel(value = "home_tab", id = "home_tab", title = 'Home',
         
  # Header section
  fluidRow(id = "hh_row", align = "center", 
  wellPanel(id = "hh_well", width = 10, 
  carousel(
    id = "mycarousel", 
    carouselItem(
      tags$img(src = "shiny-slide.png")),
    
    carouselItem(
      tags$img(src = "pm2.5.png")),
    
    carouselItem(
      tags$img(src = "tvoc.png")),
    
    carouselItem(
      tags$img(src = "temp2.png")),
    
    carouselItem(
      tags$img(src = "rh.png"))
    ))),
  
  br(),
  
  #body section
  fluidRow(id = "hb_row")
  )
