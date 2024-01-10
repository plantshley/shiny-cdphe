source(paste0(rstudioapi::getActiveProject(),"/CDPHE_app_v1/ui/app_funs.R"))
source("ui.R")

function(input, output, session) {
 
############Tab buttons############
  #HOME TAB
  observeEvent(input$home, {
    output$title <- renderUI({
      titlePanel(title = "")
    })
    updateTabsetPanel(session, "tabs", selected = "home_tab")
  })
   #properly display content images
  observe({
    
  if (input$hb_tabs == "About the App") {
    output$scroll <- renderUI({
      scroll_reveal(target = c("#img1", "#img2"), duration = 2000, distance = "300px") 
      
    })
  } else if (input$hb_tabs == "How to Use the App"){
    output$scroll <- renderUI({
      scroll_reveal(target = c("#img3", "#img4", "#img5"), duration = 2000, distance = "300px") 
    })
  } else {
    output$scroll <- renderUI({
      scroll_reveal(target = c("#img1", "#img2"), duration = 2000, distance = "300px")
    })
  } 
  })
  
###### MV TAB######
  observeEvent(input$mv, {
    output$title <- renderUI({
      h1(id = "mv_title", "Multi-Indicator Comparison")
    })
    output$plot_title <- renderUI({
      h4(id = "plot_title", plot_title())
    })
    output$plot_pols <- renderUI({
      h5(id = "mv_plot_pols", plot_pols())
    })
    updateTabsetPanel(session, "tabs", selected = "mv_tab")
    

    output$plot_mv <- renderPlotly({ res = 96
    req(input$go_mv, input$mv)
      
# CDF PLOT
    if (mv_pt() == "cfd") {
      validate(
        need(selected_mv()$val, "Sorry, there is no data available for your selection. (Or it could be there is a lot of data and I need more time to load...)"
        ))
    p6 <- ggplotly(height = 900, 
                  ggplot(data = selected_mv(), aes(x = val, y = ecdf, color = paste(site_id, id_room),
                                                   text = map(paste(  
                                                     "<b> Site ID: </b>", paste(selected_mv()$site_id, selected_mv()$id_room),"<br>",
                                                     "<b>Value:</b>", round(selected_mv()$val, digits = 1),"<br>",
                                                     "<b>Percentage:</b>", paste0(round(selected_mv()$ecdf*100, digits =1), "%"), "<br>",
                                                     "<b>Site Type:</b>", selected_mv()$site_type,"<br>",
                                                     "<b>Room Type:</b>", selected_mv()$room_type,"<br>",
                                                     "<b>Season:</b>", selected_mv()$season), HTML)), lwd = 0.25) +
                    geom_step(lwd = 0.25) +
                    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
                    scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits = c(0,1)) +
                    ylab("Percent of selected subvariable data\n") +
                    xlab("\nSelected indicator value (ppm, μg/m3, ppb, °C, etc)\n") +
                    scale_color_manual(guide = "legend",
                                       values = colorsch_mv2()) + 
                    facet_wrap(~pol, ncol=1, scales = "free_x",
                               labeller = as_labeller(c(co2 = "CO2 (ppm)", 
                                                        pm25 = "PM2.5 (μg/m3)",
                                                        voc = "TVOC (ppb)",
                                                        temp = "Tempurature (°C)",
                                                        RH = "Relative Humidity (%)"))) +
                    theme(
                      text = element_text(family = "Bahnschrift", margin = margin(b = 5, unit = "mm")), 
                      axis.text.x = element_text(family = "Bahnschrift", angle = 0, hjust = 0, size = 10, 
                                                 margin = margin(b = 1, unit = "mm")), 
                      axis.text.y = element_text(family = "Bahnschrift", size = 10, hjust = 0),
                      axis.title.y = element_text(family = "Bahnschrift", size = 12, 
                                                  margin = margin(r = 5, l = 5, t = 5, b = 5,  unit = "mm")),
                      axis.title.x = element_text(family = "Bahnschrift", size = 12, margin = margin(t = 5, b = 5, unit = "mm")),
                      strip.text = element_text(face = "bold", angle = 0, size = 10, color = "black", family = "Bahnschrift",
                                                margin = margin(t = 1.5, unit = "mm")),
                      strip.background = element_rect(fill = "#EDDFF9", size = 1),
                      panel.spacing.x = unit(0.5, "mm"),
                      plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
                      panel.background = element_rect(fill = "white", color = '#F0E7F1', linewidth =1.5),
                      panel.grid = element_blank(),
                      panel.grid.major.y = element_line(color = "#F0E7F1", size = 0.25),
                      panel.grid.minor.y = element_line(color = "#F0E7F1", size = 0.25),
                      panel.grid.major.x = element_line(color = "#F0E7F1", size = 0.25),
                      panel.grid.minor.x = element_line(color = "#F0E7F1", size = 0.25)),
                  tooltip = "text"
                  )
    p6 <- layout(
      p6, 
      legend = list(title = "", font = list(family = "Bahnschrift", size = 12),
                    itemsizing='constant')
    ) %>%
      config(displayModeBar = "static", displaylogo = FALSE, 
             modeBarButtonsToRemove = list("sendDataToCloud", "toImage", "autoScale2d", 
                                           "resetScale2d", 
                                           "select2d", "lasso2d", 
                                           "zoomIn2d", "zoomOut2d", "toggleSpikelines")) %>%
      event_register('plotly_legendclick')
    
    w_mv$hide()
    p6 
    
# NORMALIZED PLOT    
    } else if (mv_pt() == "norm") {
      validate(
        need(selected_mv()$val, "Sorry, there is no data available for your selection. (Or there is a lot of data and I need more time to load...)"
        ))
      
    p7 <- ggplotly(height = 800, 
                   ggplot() +
                     geom_line(data = selected_mv(), aes(x = how, y = val, color = paste(site_id, id_room),
                                                    text = map(paste(  
                                                      "<b> Site ID:</b>", paste(selected_mv()$site_id, selected_mv()$id_room),"<br>",
                                                      "<b>Scaled Value:</b>", round(selected_mv()$val, digits = 2),"<br>",
                                                      "<b>Hour:</b>", selected_mv()$how,"<br>",
                                                      "<b>Day of Week:</b>", selected_mv()$day_of_week,"<br>",
                                                      "<b>Site Type:</b>", selected_mv()$site_type,"<br>",
                                                      "<b>Room Type:</b>", selected_mv()$room_type,"<br>",
                                                      "<b>Season:</b>", selected_mv()$season, "<br>",
                                                      "<b>Datetime:</b>", selected_mv()$date), HTML)), 
                               lwd = 0.2) +
                     scale_x_continuous(breaks = seq(0, max(selected_mv()$how), by = 24)) +
                     scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits = c(0,1)) +
                     ylab(" ") + 
                     labs(x = "\nHour (0 = 12am the first Sunday of collected data)\n") +
                     scale_color_manual(guide = "legend",
                                        values = colorsch_mv2()) + 
                     facet_wrap(~pol, ncol=1, scales = "fixed", #strip.position = "bottom",
                                labeller = as_labeller(c(co2 = "CO2", 
                                                         pm25 = "PM2.5",
                                                         voc = "TVOC",
                                                         temp = "Tempurature",
                                                         RH = "Relative Humidity"))) +
                     theme(
                       axis.text.x = element_text(family = "Bahnschrift", angle = 0, hjust = 0, size = 10, 
                                                  margin = margin(b = 1, unit = "mm")), 
                       axis.text.y = element_text(family = "Bahnschrift", size = 10, hjust = 0),
                       axis.title.y = element_text(family = "Bahnschrift", size = 12, 
                                                   margin = margin(r = 5, l = 5, t = 5, b = 5,  unit = "mm")),
                       axis.title.x = element_text(family = "Bahnschrift", size = 12, margin = margin(t = 5, b = 5, unit = "mm")),
                       strip.text = element_text(face = "bold", angle = 0, size = 10, color = "black", family = "Bahnschrift",
                                                 margin = margin(t = 1.5, unit = "mm")),
                       strip.background = element_rect(fill = "#EDDFF9", size = 1), 
                       panel.spacing.x = unit(0.5, "mm"),
                       plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
                       panel.background = element_rect(fill = "white", color = '#F0E7F1', linewidth =1.5),
                       panel.grid = element_blank(),
                       panel.grid.major.y = element_line(color = "#F0E7F1", size = 0.25),
                       panel.grid.minor.y = element_line(color = "#F0E7F1", size = 0.25),
                       panel.grid.major.x = element_line(color = "#F0E7F1", size = 0.25),
                       panel.grid.minor.x = element_line(color = "#F0E7F1", size = 0.25)),
                   tooltip = "text"
    )
    p7 <- layout(
      p7, 
      legend = list(title = "", font = list(family = "Bahnschrift", size = 12),
                    itemsizing='constant'),
      xaxis = list(showgrid = TRUE, minorgridwidth = 0.25, minorgridcolor = "#F0E7F1")
    ) %>%
      config(displayModeBar = "static", displaylogo = FALSE, 
             modeBarButtonsToRemove = list("sendDataToCloud", "toImage", "autoScale2d", 
                                           "resetScale2d", 
                                           "select2d", "lasso2d", 
                                           "zoomIn2d", "zoomOut2d", "toggleSpikelines")
      ) %>%
      event_register('plotly_legendclick')
    
    w$hide()
    p7

# SCATTER PLOT
    } else if (mv_pt() == "scat") {
      validate(
        need(length(mv_pol()) == 2, "Please refresh the page and ensure you have TWO indicators selected for the Scatter Plot."), 
        need(nrow(selected_mv()) != 0, "Sorry, there is no data available for your selection. (Or there is a lot of data and I need more time to load...)"
        ))
      
      p8 <- ggplotly(height = 600,
                     ggplot() +
                       geom_abline(slope=1, intercept = 0, lty = "11", color = "lightgrey", alpha = 0.75) +
                       geom_point(data = selected_mv(), aes(x = .data[[mv_pol()[1]]], y = .data[[mv_pol()[2]]], 
                                                            color = paste(site_id, id_room), 
                                                           text = map(paste(  
                                                             "<b> Site ID: </b>", paste(selected_mv()$site_id, selected_mv()$id_room),"<br>",
                                                             "<b>Y-Value:</b>", round(selected_mv()[[mv_pol()[2]]], digits = 2),"<br>",
                                                             "<b>X-Value:</b>", round(selected_mv()[[mv_pol()[1]]], digits = 2),"<br>",
                                                             "<b>Day of Week:</b>", selected_mv()$day_of_week,"<br>",
                                                             "<b>Site Type:</b>", selected_mv()$site_type,"<br>",
                                                             "<b>Room Type:</b>", selected_mv()$room_type,"<br>",
                                                             "<b>Season:</b>", selected_mv()$season, "<br>",
                                                             #hour of day
                                                             "<b>Datetime:</b>", selected_mv()$date), HTML)), 
                                  lwd = 0.25, size = 0.5) +
                       geom_abline(slope=1, intercept = 0, lty = "11", color = "lightgrey", alpha = 0.75) +
                       scale_x_continuous(breaks = scales::pretty_breaks(n = 10), limits = c(0,1)) +
                       scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits = c(0,1)) +
                       ylab(paste(toupper(mv_pol()[2]), "\n")) + 
                       xlab(paste("\n",toupper(mv_pol()[1]))) +
                       scale_color_manual(guide = "legend",
                                          values = colorsch_mv2()) + 
                       theme(
                         axis.text.x = element_text(family = "Bahnschrift", angle = 0, hjust = 0, size = 10, 
                                                    margin = margin(b = 1, unit = "mm")), 
                         axis.text.y = element_text(family = "Bahnschrift", size = 10, hjust = 0),
                         axis.title.y = element_text(family = "Bahnschrift", size = 12, 
                                                     margin = margin(r = 5, l = 5, t = 5, b = 5,  unit = "mm")),
                         axis.title.x = element_text(family = "Bahnschrift", size = 12, margin = margin(t = 5, b = 5, unit = "mm")),
                         strip.text = element_text(face = "bold", angle = 0, size = 10, color = "black", family = "Bahnschrift",
                                                   margin = margin(t = 1.5, unit = "mm")),
                         strip.background = element_rect(fill = "#EDDFF9", size = 1),
                         #strip.placement = "outside", 
                         panel.spacing.x = unit(0.5, "mm"),
                         plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
                         panel.background = element_rect(fill = "white", color = '#F0E7F1', linewidth =1.5),
                         panel.grid = element_blank(),
                         panel.grid.major.y = element_line(color = "#F0E7F1", size = 0.25),
                         panel.grid.minor.y = element_line(color = "#F0E7F1", size = 0.25),
                         panel.grid.major.x = element_line(color = "#F0E7F1", size = 0.25),
                         panel.grid.minor.x = element_line(color = "#F0E7F1", size = 0.25)),
                     tooltip = "text"
      )
      p8 <- layout(
        p8, 
        legend = list(title = "", font = list(family = "Bahnschrift", size = 12),
                      itemsizing='constant'),
        xaxis = list(showgrid = TRUE, minorgridwidth = 0.25, minorgridcolor = "#F0E7F1")
      ) %>%
        config(displayModeBar = "static", displaylogo = FALSE, 
               modeBarButtonsToRemove = list("sendDataToCloud", "toImage", "autoScale2d", 
                                             "resetScale2d", 
                                             "select2d", "lasso2d", 
                                             "zoomIn2d", "zoomOut2d", "toggleSpikelines")
        ) %>%
        event_register('plotly_legendclick')
      
      w$hide()
      p8
      
    } else {
    p6
    }
    })
    }) 
######CO2 TAB######
  observeEvent(input$co2, {
    output$title <- renderUI({
      h1(id = "co2_title", "Carbon Dioxide Data")
    })
    
    updateTabsetPanel(session, "tabs", selected = "uni_tab")
    
    output$plot_titleu <- renderUI({
      h4(id = "plot_titleu", plot_titleu())
    })
    
    output$plot <- renderPlotly({ res = 96
    req(input$go, input$co2)
    validate(
      need(length(selected()$co2) != 0, "Sorry, there is no data available for your selection. (Or there is a lot of data and I need more time to load...)"
      ))
    p <- ggplotly(height = 800,  
      ggplot() +
        geom_line(data = selected(), aes(x = how, y = co2, color = paste(site_id, id_room),
                                         text = map(paste(  
                                           "<b> Site ID: </b>", paste(selected()$site_id, selected()$id_room),"<br>", 
                                           "<b>CO2 Concentration (ppm):</b>", round(selected()$co2, digits = 0),"<br>",
                                           "<b>Hour of Week:</b>", selected()$how,"<br>",
                                           "<b>Day of Week:</b>", selected()$day_of_week,"<br>",
                                           "<b>Site Type:</b>", selected()$site_type,"<br>",
                                           "<b>Room Type:</b>", selected()$room_type,"<br>",
                                           "<b>Season:</b>", selected()$season, "<br>",
                                           "<b>Datetime:</b>", selected()$date), HTML)), 
                  lwd = 0.25, show.legend = TRUE) + 
        scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
        scale_color_manual(guide = "legend",
                           values = colorsch2()) + 
        facet_wrap(~.data[[var_uni()]], nrow = 4, scales = "free_y") +
        ylab("CO2 Concentration (ppm)\n") + xlab("\n Hour of Week (Sun through Sat)") +
        scale_x_continuous(limits = t(), breaks = seq(min(t()[1]), max(t()[2]), by = 24),
                           minor_breaks = seq(min(t()[1]), max(t()[2]), by = 6), expand = c(0,0)) +
        
        theme( 
          axis.text.x = element_text(family = "Bahnschrift", angle = 0, hjust = 1, size = 10), 
          axis.text.y = element_text(family = "Bahnschrift", size = 10, hjust = 1),
          axis.title.y = element_text(family = "Bahnschrift", size = 12, margin = margin(r = 5, l = 1)),
          axis.title.x = element_text(family = "Bahnschrift", size = 12, margin = margin(t = 5, b = 1, unit = "mm")),
          strip.text = element_text(face = "plain", angle = 0, size = 10, color = "black", 
                                    margin = margin(t = 1.5, unit = "mm"), family = "Bahnschrift"),
          strip.background = element_rect(fill = "#EDDFF9", size = 1), 
          panel.spacing.x = unit(-4, "mm"),
          plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt"),
          panel.background = element_rect(fill = "white", color = '#F0E7F1', linewidth =1.5),
          panel.grid = element_blank(),
          panel.grid.major.y = element_line(color = "#F0E7F1", size = 0.25),
          panel.grid.minor.y = element_line(color = "#F0E7F1", size = 0.25),
          panel.grid.major.x = element_line(color = "#F0E7F1", size = 0.25),
          panel.grid.minor.x = element_line(color = "#F0E7F1", size = 0.25)),
      tooltip = "text"
    )
    p <- layout(
      p, 
      legend = list(title = "", font = list(family = "Bahnschrift", size = 12),
                    itemsizing='constant'),
      xaxis = list(showgrid = TRUE, minorgridwidth = 0.25, minorgridcolor = "#F0E7F1")
    ) %>%
      config(displayModeBar = "static", displaylogo = FALSE, 
             modeBarButtonsToRemove = list("sendDataToCloud", "toImage", "autoScale2d", 
                                           "resetScale2d", 
                                           "select2d", "lasso2d", 
                                           "zoomIn2d", "zoomOut2d", "toggleSpikelines")
      ) %>%
      event_register('plotly_legendclick')
    
    w$hide()
    p
    })
  })
  
######PM2.5 TAB######
  observeEvent(input$pm25, {
    output$title <- renderUI({
      h1(id = "pm25_title", "Fine Particulate Matter Data")
    })
    updateTabsetPanel(session, "tabs", selected = "uni_tab")
    
    output$plot_titleu <- renderUI({
      h4(id = "plot_titleu", plot_titleu())
    })
    
    output$plot <- renderPlotly({ res = 96
    
    req(input$go, input$pm25)
    validate(
      need(length(selected()$pm25) != 0, "Sorry, there is no data available for your selection. (Or there is a lot of data and I need more time to load...)"
      ))
    p2 <- ggplotly(height = 800,
      ggplot() +
        geom_line(data = selected(), aes(x = how, y = pm25, color = paste(site_id, id_room),
                                         text = map(paste(  
                                           "<b> Site ID: </b>", paste(selected()$site_id, selected()$id_room),"<br>", 
                                           "<b>PM2.5 Concentration (μg/m3):</b>", round(selected()$pm25, digits = 0),"<br>",
                                           "<b>Hour of Week:</b>", selected()$how,"<br>",
                                           "<b>Day of Week:</b>", selected()$day_of_week,"<br>",
                                           "<b>Site Type:</b>", selected()$site_type,"<br>",
                                           "<b>Room Type:</b>", selected()$room_type,"<br>",
                                           "<b>Season:</b>", selected()$season, "<br>",
                                           "<b>Datetime:</b>", selected()$date), HTML)),
                  lwd = 0.25, show.legend = TRUE) + 
        scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
        scale_color_manual(guide = "legend",
                           values = colorsch2()) + 
        facet_wrap(~.data[[var_uni()]], nrow = 4, scales = "free_y") +
        ylab("PM2.5 Concentration (μg/m3)\n") + xlab("\n Hour of Week (Sun ~ Sat)") +
        scale_x_continuous(limits = t(), breaks = seq(min(t()[1]), max(t()[2]), by = 24),
                           minor_breaks = seq(min(t()[1]), max(t()[2]), by = 6), expand = c(0,0)) +
        
        theme( 
          axis.text.x = element_text(family = "Bahnschrift", angle = 0, hjust = 1, size = 10), 
          axis.text.y = element_text(family = "Bahnschrift", size = 10, hjust = 1),
          axis.title.y = element_text(family = "Bahnschrift", size = 12, margin = margin(r = 5, l = 1)),
          axis.title.x = element_text(family = "Bahnschrift", size = 12, margin = margin(t = 5, b = 1, unit = "mm")),
          strip.text = element_text(face = "plain", angle = 0, size = 10, color = "black", 
                                    margin = margin(t = 1.5, unit = "mm"), family = "Bahnschrift"),
          strip.background = element_rect(fill = "#EDDFF9", size = 1), 
          panel.spacing.x = unit(0.5, "mm"),
          plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt"),
          panel.background = element_rect(fill = "white", color = '#F0E7F1', linewidth =1.5),
          panel.grid = element_blank(),
          panel.grid.major.y = element_line(color = "#F0E7F1", size = 0.25),
          panel.grid.minor.y = element_line(color = "#F0E7F1", size = 0.25),
          panel.grid.major.x = element_line(color = "#F0E7F1", size = 0.25),
          panel.grid.minor.x = element_line(color = "#F0E7F1", size = 0.25)),
      tooltip = "text"
    )
    p2 <- layout(
      p2,
      legend = list(title = "", font = list(family = "Bahnschrift", size = 12),
                    itemsizing='constant'),
      xaxis = list(showgrid = TRUE, minorgridwidth = 0.25, minorgridcolor = "#F0E7F1")
    ) %>%
      config(displayModeBar = "static", displaylogo = FALSE, 
             modeBarButtonsToRemove = list("sendDataToCloud", "toImage", "autoScale2d", 
                                           "resetScale2d", "select2d", "lasso2d", 
                                           "zoomIn2d", "zoomOut2d", "toggleSpikelines")
      ) %>%
      event_register('plotly_legendclick')
    
    w$hide()
    p2
    })
  })
  
######TVOC TAB######
  observeEvent(input$voc, {
    output$title <- renderUI({
      h1(id = "voc_title", "Total Volatile Organic Compounds Data")
    })
    updateTabsetPanel(session, "tabs", selected = "uni_tab")
    
    output$plot_titleu <- renderUI({
      h4(id = "plot_titleu", plot_titleu())
    })
    
    output$plot <- renderPlotly({ res = 96
    
    req(input$go, input$voc)
    validate(
      need(length(selected()$voc) != 0, "Sorry, there is no data available for your selection. (Or there is a lot of data and I need more time to load...)"
      ))
    p3 <- ggplotly(height = 800,
      ggplot() +
        geom_line(data = selected(), aes(x = how, y = voc, color = paste(site_id, id_room),
                                         text2 = map(paste(
                                           selected()$site_id, " ", selected()$room), HTML),
                                         text = map(paste(  
                                           "<b> Site ID: </b>", paste(selected()$site_id, selected()$id_room),"<br>", 
                                           "<b>TVOC Concentration (ppb):</b>", round(selected()$voc, digits = 0),"<br>",
                                           "<b>Hour of Week:</b>", selected()$how,"<br>",
                                           "<b>Day of Week:</b>", selected()$day_of_week,"<br>",
                                           "<b>Site Type:</b>", selected()$site_type,"<br>",
                                           "<b>Room Type:</b>", selected()$room_type,"<br>",
                                           "<b>Season:</b>", selected()$season, "<br>",
                                           "<b>Datetime:</b>", selected()$date), HTML)),
                  lwd = 0.25, show.legend = TRUE) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
        scale_color_manual(guide = "legend",
                           values = colorsch2()) +
        facet_wrap(~.data[[var_uni()]], nrow = 4, scales = "free_y") +
        ylab("TVOC Concentration (ppb)\n") + xlab("\n Hour of Week (Sun ~ Sat)") +
        scale_x_continuous(limits = t(), breaks = seq(min(t()[1]), max(t()[2]), by = 24),
                           minor_breaks = seq(min(t()[1]), max(t()[2]), by = 6), expand = c(0,0)) +
        
        theme( 
          axis.text.x = element_text(family = "Bahnschrift", angle = 0, hjust = 1, size = 10), 
          axis.text.y = element_text(family = "Bahnschrift", size = 10, hjust = 1),
          axis.title.y = element_text(family = "Bahnschrift", size = 12, margin = margin(r = 5, l = 1)),
          axis.title.x = element_text(family = "Bahnschrift", size = 12, margin = margin(t = 5, b = 1, unit = "mm")),
          strip.text = element_text(face = "plain", angle = 0, size = 10, color = "black", 
                                    margin = margin(t = 1.5, unit = "mm"), family = "Bahnschrift"),
          strip.background = element_rect(fill = "#EDDFF9", size = 1), 
          panel.spacing.x = unit(0.5, "mm"),
          plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt"),
          panel.background = element_rect(fill = "white", color = '#F0E7F1', linewidth =1.5),
          panel.grid = element_blank(),
          panel.grid.major.y = element_line(color = "#F0E7F1", size = 0.25),
          panel.grid.minor.y = element_line(color = "#F0E7F1", size = 0.25),
          panel.grid.major.x = element_line(color = "#F0E7F1", size = 0.25),
          panel.grid.minor.x = element_line(color = "#F0E7F1", size = 0.25)),
      tooltip = "text"
    )
    p3 <- layout(
      p3,
      legend = list(title = "", font = list(family = "Bahnschrift", size = 12),
                    itemsizing='constant'),
      xaxis = list(showgrid = TRUE, minorgridwidth = 0.25, minorgridcolor = "#F0E7F1")
    ) %>%
      config(displayModeBar = "static", displaylogo = FALSE, 
             modeBarButtonsToRemove = list("sendDataToCloud", "toImage", "autoScale2d", 
                                           "resetScale2d", "select2d", "lasso2d", 
                                           "zoomIn2d", "zoomOut2d", "toggleSpikelines")
      ) %>%
      event_register('plotly_legendclick')
    
    w$hide()
    p3
    })
  })
  
######Temp Tab###### 
  observeEvent(input$temp, {
    output$title <- renderUI({
      h1(id = "temp_title", "Temperature Data")
    })
    updateTabsetPanel(session, "tabs", selected = "uni_tab")
    output$plot <- renderPlotly({ res = 96
    
    output$plot_titleu <- renderUI({
      h4(id = "plot_titleu", plot_titleu())
    })
    
    req(input$go, input$temp)
    validate(
      need(length(selected()$temp) != 0, "Sorry, there is no data available for your selection. (Or there is a lot of data and I need more time to load...)"
      ))
    p4 <- ggplotly(height = 800, 
      ggplot() +
        geom_line(data = selected(), aes(x = how, y = temp, color = paste(site_id, id_room),
                                         text = map(paste(  
                                           "<b> Site ID: </b>", paste(selected()$site_id, selected()$id_room),"<br>", 
                                           "<b>Temperature (°C):</b>", round(selected()$temp, digits = 0),"<br>",
                                           "<b>Hour of Week:</b>", selected()$how,"<br>",
                                           "<b>Day of Week:</b>", selected()$day_of_week,"<br>",
                                           "<b>Site Type:</b>", selected()$site_type,"<br>",
                                           "<b>Room Type:</b>", selected()$room_type,"<br>",
                                           "<b>Season:</b>", selected()$season, "<br>",
                                           "<b>Datetime:</b>", selected()$date), HTML)), 
                  lwd = 0.25, show.legend = TRUE) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
        scale_color_manual(guide = "legend",
                           values = colorsch2()) + 
        facet_wrap(~.data[[var_uni()]], nrow = 4, scales = "free_y") +
        ylab("Temperature (°C)\n") + xlab("\n Hour of Week (Sun ~ Sat)") +
        scale_x_continuous(limits = t(), breaks = seq(min(t()[1]), max(t()[2]), by = 24),
                           minor_breaks = seq(min(t()[1]), max(t()[2]), by = 6), expand = c(0,0)) +
        
        theme( 
          axis.text.x = element_text(family = "Bahnschrift", angle = 0, hjust = 1, size = 10), 
          axis.text.y = element_text(family = "Bahnschrift", size = 10, hjust = 1),
          axis.title.y = element_text(family = "Bahnschrift", size = 12, margin = margin(r = 5, l = 1)),
          axis.title.x = element_text(family = "Bahnschrift", size = 12, margin = margin(t = 5, b = 1, unit = "mm")),
          strip.text = element_text(face = "plain", angle = 0, size = 10, color = "black", 
                                    margin = margin(t = 1.5, unit = "mm"), family = "Bahnschrift"),
          strip.background = element_rect(fill = "#EDDFF9", size = 1), 
          panel.spacing.x = unit(0.5, "mm"),
          plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt"),
          panel.background = element_rect(fill = "white", color = '#F0E7F1', linewidth =1.5),
          panel.grid = element_blank(),
          panel.grid.major.y = element_line(color = "#F0E7F1", size = 0.25),
          panel.grid.minor.y = element_line(color = "#F0E7F1", size = 0.25),
          panel.grid.major.x = element_line(color = "#F0E7F1", size = 0.25),
          panel.grid.minor.x = element_line(color = "#F0E7F1", size = 0.25)),
      tooltip = "text"
    )
    p4 <- layout(
      p4,
      legend = list(title = "", font = list(family = "Bahnschrift", size = 12),
                    itemsizing='constant'),
      xaxis = list(showgrid = TRUE, minorgridwidth = 0.25, minorgridcolor = "#F0E7F1")
    ) %>%
      config(displayModeBar = "static", displaylogo = FALSE, 
             modeBarButtonsToRemove = list("sendDataToCloud", "toImage", "autoScale2d", 
                                           "resetScale2d", "select2d", "lasso2d", 
                                           "zoomIn2d", "zoomOut2d", "toggleSpikelines")
      ) %>%
      event_register('plotly_legendclick')
    
    w$hide()
    p4
    })
  })
  
######RH TAB######
  observeEvent(input$rh, {
    output$title <- renderUI({
      h1(id = "rh_title", "Relative Humidity Data")
    })
    
    updateTabsetPanel(session, "tabs", selected = "uni_tab")
    
    output$plot_titleu <- renderUI({
      h4(id = "plot_titleu", plot_titleu())
    })
    
    output$plot <- renderPlotly({ res = 96
    
    req(input$go, input$rh)
    validate(
      need(length(selected()$RH) != 0, "Sorry, there is no data available for your selection. (Or there is a lot of data and I need more time to load...)"
      ))
    p5 <- ggplotly(height = 800, 
      ggplot() +
        geom_line(data = selected(), aes(x = how, y = RH, color = paste(site_id, id_room),
                                         text = map(paste(  
                                           "<b> Site ID: </b>", paste(selected()$site_id, selected()$id_room),"<br>", 
                                           "<b>Relative Humidity (%):</b>", round(selected()$RH, digits = 0),"<br>",
                                           "<b>Hour of Week:</b>", selected()$how,"<br>",
                                           "<b>Day of Week:</b>", selected()$day_of_week,"<br>",
                                           "<b>Site Type:</b>", selected()$site_type,"<br>",
                                           "<b>Room Type:</b>", selected()$room_type,"<br>",
                                           "<b>Season:</b>", selected()$season, "<br>",
                                           "<b>Datetime:</b>", selected()$date), HTML)), 
                  lwd = 0.25, show.legend = TRUE) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
        scale_color_manual(guide = "legend",
                           values = colorsch2()) + 
        facet_wrap(~.data[[var_uni()]], nrow = 4, scales = "free_y") +
        ylab("Relative Humidity (%)\n\n") + xlab("\n Hour of Week (Sun ~ Sat)") +
        scale_x_continuous(limits = t(), breaks = seq(min(t()[1]), max(t()[2]), by = 24),
                           minor_breaks = seq(min(t()[1]), max(t()[2]), by = 6), expand = c(0,0)) +
        
        theme( 
          axis.text.x = element_text(family = "Bahnschrift", angle = 0, hjust = 1, size = 10), 
          axis.text.y = element_text(family = "Bahnschrift", size = 10, hjust = 1),
          axis.title.y = element_text(family = "Bahnschrift", size = 12, margin = margin(r = 5, l = 1)),
          axis.title.x = element_text(family = "Bahnschrift", size = 12, margin = margin(t = 5, b = 1, unit = "mm")),
          strip.text = element_text(face = "plain", angle = 0, size = 10, color = "black", 
                                    margin = margin(t = 1.5, unit = "mm"), family = "Bahnschrift"),
          strip.background = element_rect(fill = "#EDDFF9", size = 1), 
          panel.spacing.x = unit(0.5, "mm"),
          #panel.spacing.y = unit(5, "mm"),
          plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt"),
          panel.background = element_rect(fill = "white", color = '#F0E7F1', linewidth =1.5),
          panel.grid = element_blank(),
          panel.grid.major.y = element_line(color = "#F0E7F1", size = 0.25),
          panel.grid.minor.y = element_line(color = "#F0E7F1", size = 0.25),
          panel.grid.major.x = element_line(color = "#F0E7F1", size = 0.25),
          panel.grid.minor.x = element_line(color = "#F0E7F1", size = 0.25)),
      tooltip = "text"
    )
    p5 <- layout(
      p5,
      legend = list(title = "", font = list(family = "Bahnschrift", size = 12),
                    itemsizing='constant'),
      xaxis = list(showgrid = TRUE, minorgridwidth = 0.25, minorgridcolor = "#F0E7F1")
    ) %>%
      config(displayModeBar = "static", displaylogo = FALSE, 
             modeBarButtonsToRemove = list("sendDataToCloud", "toImage", "autoScale2d", 
                                           "resetScale2d", "select2d", "lasso2d", 
                                           "zoomIn2d", "zoomOut2d", "toggleSpikelines")
      ) %>%
      event_register('plotly_legendclick')
    w$hide()
    p5
    })
  })
  
######Glossary Tab
  observeEvent(input$glossary, {
    shinyjs::enable(id = "accordion1")
    
    output$title <- renderUI({
      h1(id = "gloss_title", "Glossary")
    })  
    
    updateTabsetPanel(session, "tabs", selected = "glossary tab")
  })

#Reactives
  t <- eventReactive(input$go,{input$hour})
  var_uni <- eventReactive(input$go, {input$var})
  
  mv_pol <- eventReactive(input$go_mv, {input$pol})
  mv_pt <- eventReactive(input$go_mv, {input$plot_type_mv})
 
  colorsch2 <- reactiveVal(NULL)
  colorsch_mv2 <- reactiveVal(NULL)

#Subvariables (mv tab)
  observeEvent(input$var_mv, {
    if (input$var_mv == "site_type") {
      updatePickerInput(session, "second_in_mv", "Choose site type:",
                        choices = site_types,
                        choicesOpt = list(content = c(
                          pickerFormat01("#E7BEF9", "fa fa-child-reaching fa-bounce", "Childcare"),
                          pickerFormat01("#E7BEF9", "fa fa-person-cane", "Elderly Care"),
                          pickerFormat01("#E7BEF9", "fa fa-hand-holding-hand fa-sm", "Adult Care/Social Services"),
                          pickerFormat01("#E7BEF9", "fa fa-person-shelter", "24-hr Shelters"),
                          pickerFormat01("#E7BEF9", "fa fa-solid fa-tree-city fa-sm", "Office Buildings"),
                          pickerFormat01("#E7BEF9", "fa fa-bowl-food", "Food Services"),
                          pickerFormat01("#E7BEF9", "fa fa-mosque fa-sm", "Religious Facilities")))
      )
      
    } else if (input$var_mv == "room_type") {
      updatePickerInput(session,"second_in_mv", "Choose room type:",
                        choices = room_types2,
                        choicesOpt = list(content = c(
                          pickerFormat01("#F2BEF2", "fa fa-kitchen-set", "Kitchen"),
                          pickerFormat01("#F2BEF2", "fa fa-utensils", "Dining"),
                          pickerFormat01("#F2BEF2", "fa fa-stapler fa-sm", "Office"),
                          pickerFormat01("#F2BEF2", "fa fa-bell-concierge", "Reception"),
                          pickerFormat01("#F2BEF2", "fa fa-chalkboard-user", "Classroom"),
                          pickerFormat01("#F2BEF2", "fa fa-user-group", "Meeting"),
                          pickerFormat01("#F2BEF2", "fa fa-people-group", "Gathering"),
                          pickerFormat01("#F2BEF2", "fa fa-couch", "Break Room"),
                          pickerFormat01("#F2BEF2", "fa fa-door-closed", "Corridor"),
                          pickerFormat01("#F2BEF2", "fa fa-bed fa-sm", "Bedroom"),
                          pickerFormat01("#F2BEF2", "fa fa-box-archive", "Storage"),
                          pickerFormat01("#F2BEF2", "fa fa-stethoscope", "Exam Room"),
                          pickerFormat01("#F2BEF2", "fa fa-flask", "Medical Lab"),
                          pickerFormat01("#F2BEF2", "fa fa-ghost fa-bounce", "Other")))
      ) 
      
    } else if (input$var_mv == "leakiness") {
      updatePickerInput(session,"second_in_mv", "Building Leakiness:",
                        choices = leaky,
                        choicesOpt = list(content = c(
                          pickerFormat01("#FFCEDA", "fa fa-carrot", "Leaky"),
                          pickerFormat01("#FFCEDA", "fa fa-candy-cane", "Moderate"),
                          pickerFormat01("#FFCEDA", "fa fa-pepper-hot", "Tight")))
      ) 
      
    } else if (input$var_mv == "season") {
      updatePickerInput(session,"second_in_mv", "Choose season:",
                        choices = seasons,
                        choicesOpt = list(content = c(
                          pickerFormat01("#FFC9EF", "fa fa-solid fa-snowflake", "Winter"),
                          pickerFormat01("#FFC9EF", "fa fa-solid fa-leaf", "Spring"),
                          pickerFormat01("#FFC9EF", "fa fa-solid fa-sun", "Summer"),
                          pickerFormat01("#FFC9EF", "fa fa-crow", "Fall")))
      )
      
    } else if (input$var_mv == "ah_groups") {
      updatePickerInput(session,"second_in_mv", "Number of Airhandlers:",
                        choices = air_hands,
                        choicesOpt = list(content = c(
                          pickerFormat01("#FFCFC5", "fa fa-solid fa-fish", "Less than 10"),
                          pickerFormat01("#FFCFC5", "fa fa-solid fa-frog fa-bounce", "10 to 25"),
                          pickerFormat01("#FFCFC5", "fa fa-solid fa-dragon", "More than 25")))
      ) 
    } else if (input$var_mv == "day_of_week") {
      updatePickerInput(session,"second_in_mv", "Day of Week:",
                        choices = dow_types,
                        choicesOpt = list(content = c(
                          pickerFormat01("#FFE2BA", "fa fa-regular fa-sun fa-spin", "Sunday"),
                          pickerFormat01("#FFE2BA", "fa fa-solid fa-m", "Monday"),
                          pickerFormat01("#FFE2BA", "fa fa-solid fa-t", "Tuesday"),
                          pickerFormat01("#FFE2BA", "fa fa-solid fa-w", "Wednesday"),
                          pickerFormat01("#FFE2BA", "fa fa-solid fa-r", "Thursday"), 
                          pickerFormat01("#FFE2BA", "fa fa-solid fa-f", "Friday"),
                          pickerFormat01("#FFE2BA", "fa fa-regular fa-face-smile fa-spin", "Saturday")))
      )
    } else if (input$var_mv == "tod") {
      updatePickerInput(session,"second_in_mv", "Time of Day:",
                        choices = tod_types,
                        choicesOpt = list(content = c(
                          pickerFormat01("#F9EEAB", "fa fa-solid fa-mug-saucer", "12am to 8am"),
                          pickerFormat01("#F9EEAB", "fa fa-solid fa-briefcase", "8am to 6pm"),
                          pickerFormat01("#F9EEAB", "fa fa-solid fa-cloud-moon", "6pm to 12am")))
      )
      
        
    } else if (input$var_mv == "hod") {
      updatePickerInput(session,"second_in_mv", "Hour of Day:",
                        choices = hod_types,
                        #multiple = TRUE, 
                        choicesOpt = list(content = c(
                          pickerFormat01("#E5EFAB", "fa fa-solid fa-water", "12am"),
                          pickerFormat01("#E5EFAB", "fa fa-solid fa-worm", "1am"),
                          pickerFormat01("#E5EFAB", "fa fa-solid fa-sun", "2am"),
                          pickerFormat01("#E5EFAB", "fa fa-solid fa-cloud-rain", "3am"),
                          pickerFormat01("#E5EFAB", "fa fa-solid fa-rainbow fa-sm", "4am"),
                          pickerFormat01("#E5EFAB", "fa fa-solid fa-mound", "5am"),
                          pickerFormat01("#E5EFAB", "fa fa-solid fa-seedling fa-xs", "6am"),
                          pickerFormat01("#E5EFAB", "fa fa-solid fa-seedling fa-sm", "7am"),
                          pickerFormat01("#E5EFAB", "fa fa-solid fa-seedling", "8am"),
                          pickerFormat01("#E5EFAB", "fa fa-solid fa-seedling", "9am"),
                          pickerFormat01("#E5EFAB", "fa fa-solid fa-seedling", "10am"),
                          pickerFormat01("#E5EFAB", "fa fa-solid fa-seedling", "11am"),
                          pickerFormat01("#E5EFAB", "fa fa-solid fa-seedling", "12pm"),
                          pickerFormat01("#E5EFAB", "fa fa-solid fa-tree fa-xs", "1pm"),
                          pickerFormat01("#E5EFAB", "fa fa-solid fa-tree fa-sm", "2pm"),
                          pickerFormat01("#E5EFAB", "fa fa-solid fa-tree", "3pm"),
                          pickerFormat01("#E5EFAB", "fa fa-solid fa-tree", "4pm"),
                          pickerFormat01("#E5EFAB", "fa fa-solid fa-tree", "5pm"),
                          pickerFormat01("#E5EFAB", "fa fa-solid fa-tree", "6pm"),
                          pickerFormat01("#E5EFAB", "fa fa-solid fa-tree", "7pm"),
                          pickerFormat01("#E5EFAB", "fa fa-solid fa-bugs", "8pm"),
                          pickerFormat01("#E5EFAB", "fa fa-solid fa-wind", "9pm"),
                          pickerFormat01("#E5EFAB", "fa fa-solid fa-plant-wilt", "10pm"),
                          pickerFormat01("#E5EFAB", "fa fa-solid fa-worm", "11pm")))
      )
      
                            
    } else {
      updateSelectInput(session, "second_in_mv", "Select an option", choices = NULL)
    }
  })  
  
#Subvariables (uni tab)
  observeEvent(input$var, {
    if (input$var == "site_type") {
      updatePickerInput(session, "second_in", "Choose site type(s):",
                        choices = site_types,
                        choicesOpt = list(content = c(
                          pickerFormat01("#E7BEF9", "fa fa-child-reaching fa-bounce", "Childcare"),
                          pickerFormat01("#E7BEF9", "fa fa-person-cane", "Elderly Care"),
                          pickerFormat01("#E7BEF9", "fa fa-hand-holding-hand fa-sm", "Adult Care/Social Services"),
                          pickerFormat01("#E7BEF9", "fa fa-person-shelter", "24-hr Shelters"),
                          pickerFormat01("#E7BEF9", "fa fa-solid fa-tree-city fa-sm", "Office Buildings"),
                          pickerFormat01("#E7BEF9", "fa fa-bowl-food", "Food Services"),
                          pickerFormat01("#E7BEF9", "fa fa-mosque fa-sm", "Religious Facilities")))
      )
      
    } else if (input$var == "room_type") {
      updatePickerInput(session,"second_in", "Choose room type(s):",
                        choices = room_types2,
                        choicesOpt = list(content = c(
                          pickerFormat01("#F2BEF2", "fa fa-kitchen-set", "Kitchen"),
                          pickerFormat01("#F2BEF2", "fa fa-utensils", "Dining"),
                          pickerFormat01("#F2BEF2", "fa fa-stapler fa-sm", "Office"),
                          pickerFormat01("#F2BEF2", "fa fa-bell-concierge", "Reception"),
                          pickerFormat01("#F2BEF2", "fa fa-chalkboard-user", "Classroom"),
                          pickerFormat01("#F2BEF2", "fa fa-user-group", "Meeting"),
                          pickerFormat01("#F2BEF2", "fa fa-people-group", "Gathering"),
                          pickerFormat01("#F2BEF2", "fa fa-couch", "Break Room"),
                          pickerFormat01("#F2BEF2", "fa fa-door-closed", "Corridor"),
                          pickerFormat01("#F2BEF2", "fa fa-bed fa-sm", "Bedroom"),
                          pickerFormat01("#F2BEF2", "fa fa-box-archive", "Storage"),
                          pickerFormat01("#F2BEF2", "fa fa-stethoscope", "Exam Room"),
                          pickerFormat01("#F2BEF2", "fa fa-flask", "Medical Lab"),
                          pickerFormat01("#F2BEF2", "fa fa-ghost fa-bounce", "Other")))
      ) 
      
    } else if (input$var == "leakiness") {
      updatePickerInput(session,"second_in", "Building Leakiness:",
                        choices = leaky,
                        choicesOpt = list(content = c(
                          pickerFormat01("#FFCEDA", "fa fa-carrot", "Leaky"),
                          pickerFormat01("#FFCEDA", "fa fa-candy-cane", "Moderate"),
                          pickerFormat01("#FFCEDA", "fa fa-pepper-hot", "Tight")))
      ) 
      
    } else if (input$var == "season") {
      updatePickerInput(session,"second_in", "Choose season(s)",
                        choices = seasons,
                        choicesOpt = list(content = c(
                          pickerFormat01("#FFC9EF", "fa fa-solid fa-snowflake", "Winter"),
                          pickerFormat01("#FFC9EF", "fa fa-solid fa-leaf", "Spring"),
                          pickerFormat01("#FFC9EF", "fa fa-solid fa-sun", "Summer"),
                          pickerFormat01("#FFC9EF", "fa fa-crow", "Fall")))
      )
      
    } else if (input$var == "ah_groups") {
      updatePickerInput(session,"second_in", "Number of Airhandlers:",
                        choices = air_hands,
                        choicesOpt = list(content = c(
                          pickerFormat01("#FFCFC5", "fa fa-solid fa-fish", "Less than 10"),
                          pickerFormat01("#FFCFC5", "fa fa-solid fa-frog fa-bounce", "10 to 25"),
                          pickerFormat01("#FFCFC5", "fa fa-solid fa-dragon", "More than 25")))
      ) 
      
    } else {
      updateSelectInput(session, "second_in", "Select an option", choices = NULL)
    }
  })
  
#Waiter
  w_mv <- waiter::Waiter$new(color = transparent(), html = spin_solar())
  w <- waiter::Waiter$new(color = transparent(), html = spin_solar())
  
#MV plot title  
  plot_title <- eventReactive(input$go_mv, {
    req(input$plot_type_mv)
    
    if (input$plot_type_mv == "cfd") {
      paste("Cumulative", "Frequecy", "Distribution")
      
    } else if (input$plot_type_mv == "norm") {
      "Normalized  Scale  Hourly  Time  Series"
      
    } else if (input$plot_type_mv == "scat") {
      "Scatter Plot"
      
    } else {
      ""
  }
    })
  
#Uni plot title  
  plot_titleu <- eventReactive(input$go, {
    req(input$plot_type)
    
    if (input$plot_type == "1hr") {
      "Hourly-Averaged Time Series"
      
    } else if (input$plot_type == "24hr") {
      "Daily-Averaged Time Series"
      
    } else if (input$plot_type == "trend") {
      "Trend Line"
      
    } else if (input$plot_type == "pattern") {
      "Fluctuation Line"
      
    } else {
      ""
    }
  })
  
#MV text  
  plot_pols <- eventReactive(input$plot_type_mv, {
    req(input$plot_type_mv)
    
    if (input$plot_type_mv == "cfd") {
      "Select Indicators to Compare:" 
      
    } else if (input$plot_type_mv == "norm") {
      "Select Indicators to Compare:"
      
    } else if (input$plot_type_mv == "scat") {
      "Select TWO Indicators for the Scatter Plot:"
      
    } else {
      ""
    }
  })
  
#Selected data (mv tab)  
  selected_mv <- reactiveVal(NULL)
  origen <- reactiveVal(NULL)
  
  observeEvent(input$go_mv, {
    req(input$var_mv, input$second_in_mv)
    
    w_mv$show()
    
    mv1 <- 
    if (mv_pt() == "cfd") {
      
      if (input$var_mv %in% c("day_of_week", "tod", "hod")) {
       dataset3 %>%
          filter(pol %in% mv_pol()) %>%
          filter(.data[[input$var_mv]] %in% input$second_in_mv) %>%
          group_by(site_id, id_room, pol) %>%
          mutate(ecdf = ecdf(val)(val))
        
      } else
       dataset3 %>%
          filter(pol %in% mv_pol()) %>%
          filter(.data[[input$var_mv]] %in% input$second_in_mv)
      
    } else if (mv_pt() == "norm") {
      "%ni%" <- Negate("%in%")
        dataset4 %>%
        filter(pol %in% mv_pol()) %>%
        filter(.data[[input$var_mv]] %in% input$second_in_mv)
      
    } else if (mv_pt() == "scat") {
      dataset5 %>%
        filter(.data[[input$var_mv]] %in% input$second_in_mv)
      
    } else {
      dataset3 %>%
        filter(.data[[input$var_mv]] %in% input$second_in_mv)
    }
    origen(mv1)
    selected_mv(mv1)
    
    num_vals <- length(unique(paste(selected_mv()$site_id, selected_mv()$id_room)))
    cmv2 <- 
      switch(input$colors_mv,
             "Default" = colorRampPalette(c("#4FD9D9", "#FF68BD"))(num_vals),
             "Rainbow" = colorRampPalette(c("#F8766D", "#00C0B8","#529EFF","#FF689F"))(num_vals),
             "Colorblind-Friendly" = viridis::plasma(num_vals))
    
    colorsch_mv2(cmv2)
  })
  
#Selected data (uni tab)
  selected <- reactiveVal(NULL)
  origen0 <- reactiveVal(NULL)
  
  observeEvent(input$go, {
    req(input$var, input$second_in)
    
    w$show()
    uni1 <- 
    if (input$plot_type == "1hr") {
      dataset2 %>%
        filter(.data[[input$var]] %in% input$second_in)
      
    } else if (input$plot_type == "24hr") {
      dataset2 %>% d_avg() %>% 
        filter(.data[[input$var]] %in% input$second_in)
      
    } else if (input$plot_type == "trend") {
      dataset2 <-
        readRDS(paste0(rstudioapi::getActiveProject(),"/CDPHE_app_v1/data/cdphe_ssa_trend2.rds")) %>%
        filter(.data[[input$var]] %in% input$second_in)
      
    } else if (input$plot_type == "pattern") {
      dataset2 <-
        readRDS(paste0(rstudioapi::getActiveProject(),"/CDPHE_app_v1/data/cdphe_ssa_pattern2.rds")) %>%
        filter(.data[[input$var]] %in% input$second_in)
      
    } else {
      dataset2 %>%
        filter(.data[[input$var]] %in% input$second_in)
    }
    origen0(uni1)
    selected(uni1)
  })

  
  
#Update selected_mv & selected for siteID
  observe({
    if (input$siteID != "" & is.character(input$siteID)){
      site_ids <- strsplit(input$siteID, ',')[[1]]
      
      mv2 <- origen()
      mv2 <- mv2 %>%
        filter(site_id %in% site_ids)
      selected_mv(mv2)
      
    } else {
      mv2 <- origen()
      selected_mv(mv2)
      
    } 
    num_vals <- length(unique(paste(selected_mv()$site_id, selected_mv()$id_room)))
    cmv2 <- 
      switch(input$colors_mv,
             "Default" = colorRampPalette(c("#4FD9D9", "#FF68BD"))(num_vals),
             "Rainbow" = colorRampPalette(c("#F8766D", "#00C0B8","#529EFF","#FF689F"))(num_vals),
             "Colorblind-Friendly" = viridis::plasma(num_vals))
    
    colorsch_mv2(cmv2)
  })
  
  observe({
    if (input$siteIDu != "" ){
      site_ids <- strsplit(input$siteIDu, ',')[[1]]
      
      uni2 <- origen0()
      uni2 <- uni2 %>%
        filter(site_id %in% site_ids)
      selected(uni2)
      
    } else {
      uni2 <- origen0()
      selected(uni2)
      
    }
    num_vals <- length(unique(paste(selected()$site_id, selected()$id_room)))
    c2 <- 
      switch(input$colors,
             "Default" = colorRampPalette(c("#4FD9D9", "#FF68BD"))(num_vals),
             "Rainbow" = colorRampPalette(c("#F8766D", "#00C0B8","#529EFF","#FF689F"))(num_vals),
             "Colorblind-Friendly" = viridis::plasma(num_vals))
    
    colorsch2(c2)
  }) 
  
#Reset search input when go is clicked   
  observeEvent(input$go_mv, {
    runjs("Shiny.setInputValue( 'siteID', '' );")
  })
  observeEvent(input$go, {
    runjs("Shiny.setInputValue( 'siteIDu', '' );")
  })
  
#Glossary Search
  observeEvent(input$search, {
    shinyjs::runjs(
      sprintf(
        "$items = $('#accordion1 .panel.box.box-solid');
         $items.detach().sort(function(a, b) {
           var aText = $(a).text().toLowerCase();
           var bText = $(b).text().toLowerCase();
           return (aText.includes('%s') ? -1 : aText.localeCompare(bText));
         });
         $('#accordion1').empty().append($items);",
        tolower(input$search)
      ) 
    ) 
  })
  
#Plotly legend
  observeEvent(event_data("plotly_legendclick"), {
    # Get the legend item clicked
    click_data <- event_data("plotly_legendclick")
    
    if (!is.null(click_data)) {
      # Get the name of the clicked legend item
      legend_item <- click_data$label
      
      # Remove the line from the plot by updating the plot
      output$Plot <- renderPlotly({
        p <- event_data("plotly_legendclick")
        
        if (!is.null(p)) {
          plotlyProxy("Plot", session) %>%
            plotly::hide_trace(legend_item)
        }
      })
    }
  })
}