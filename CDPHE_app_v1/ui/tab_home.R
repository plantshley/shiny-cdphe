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
      tags$img(src = "temp.png")),
    
    carouselItem(
      tags$img(src = "rh.png")),
    
    carouselItem(
      tags$img(src = "refs.png"))
    ))),
  

  #body section
  fluidRow(id = "hb_row", width = 8, 
           
           tabsetPanel(id = "hb_tabs", type = "tabs", selected = "Home",
                       
                       tabPanel(title = "Home", align = "center",
                                tags$img(src = "home.png", id = "img0", 
                                         style = "width: 100%;")), 
                       
                       tabPanel(title = "About the App", align = "center",
                                tags$img(src = "iaqv.png", id = "img1", 
                                         style = "width: 80%;"),
                                tags$img(src = "purpose.png", id = "img2", 
                                          style = "width: 80%;")),
                       
                       tabPanel(title = "How to Use the App", align = "center", 
                                tags$img(src = "use1.png", id = "img3", 
                                         style = "width: 80%;"),
                                tags$img(src = "use2.png", id = "img4", 
                                         style = "width: 80%;"),
                                tags$img(src = "use3.png", id = "img5", 
                                         style = "width: 80%;"))
           )
  ),
  uiOutput("scroll")
  
)



