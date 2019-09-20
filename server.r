server <- function(input,output,session){
  
  ############################ By ada status ----
 
  output$myMap <- renderLeaflet({
    req(input$ADA)
    
    leaflet() %>%
      addPolygons(data=bb, stroke = FALSE, fillColor = "#0d0d0d") %>%
      addPolylines(data = sublines2,weight = 3,color = sublines2$color,label = NULL,group = 'Lines',
                   opacity = 0.25, smoothFactor = 3 )%>%
      addCircleMarkers(
        data = allstops1[allstops1$ADA_StatusLayer %in% "Full ADA Access", ],
        color = allstops1[allstops1$ADA_StatusLayer %in% "Full ADA Access",]$adacolors,
        radius = 4,
        popup = councilPopup(
          paste("<h4 class=","header-tiny",">","<u>",allstops1[allstops1$ADA_StatusLayer %in% "Full ADA Access",]$name,"</u></h4><br>", 
                 "<b>","<font size=","0.5","'>","Lines:","</b>", 
                allstops1[allstops1$ADA_StatusLayer %in% "Full ADA Access",]$lines2, "<br><b>","ADA Status:", "</b>","<br>",
                allstops1[allstops1$ADA_StatusLayer %in% "Full ADA Access",]$ADA_Status, "<hr>", "<b>","Elevators:", "</b>",  
                allstops1[allstops1$ADA_StatusLayer %in% "Full ADA Access",]$num_el, "<br>", "Type:",
                allstops1[allstops1$ADA_StatusLayer %in% "Full ADA Access",]$el_stat, "<br>", "Unavailability (24-Hr Average):","<br>",
                allstops1[allstops1$ADA_StatusLayer %in% "Full ADA Access",]$no_service, "<small>", "(Range)", "</small>")
          ),
        group = "sub1_l", 
        fillOpacity = 1,
        weight = 2,
        label = allstops1[allstops1$ADA_StatusLayer %in% "Full ADA Access", 1],
        opacity = 0) %>%
      addLegend(position = "bottomright", colors = c("#228AE6", "#82C91E", "#BE4BDB", "#3B2483","#D05D4E", "#666666"), 
                labels = unique(allstops1$ADA_StatusLayer), opacity = 1 ) %>%
      setView( -73.933560,40.704343,  zoom = 10.5) #%>% 
      #addResetMapButton() 
  })
  
  observeEvent({

    input$ADA
  }, {
    leafletProxy("myMap") %>%
      clearMarkers() %>%
      addCircleMarkers(
        data = allstops1[allstops1$ADA_StatusLayer %in% input$ADA, ],
        color = allstops1[ allstops1$ADA_StatusLayer %in% input$ADA,]$adacolors,
        radius = 4,
        popup = councilPopup(
          paste("<h4 class=","header-tiny",">","<u>",allstops1[allstops1$ADA_StatusLayer %in% input$ADA,]$name,"</u></h4><br>",
                "<b>","<font size=","0.5","'>","Lines:","</b>",
                allstops1[allstops1$ADA_StatusLayer %in% input$ADA,]$lines2, "<br><b>","ADA Status:", "</b>","<br>",
                allstops1[allstops1$ADA_StatusLayer %in% input$ADA,]$ADA_Status, "<hr>", "<b>","Elevators:", "</b>",
                allstops1[allstops1$ADA_StatusLayer %in% input$ADA,]$num_el, "<br>", "Type:",
                allstops1[allstops1$ADA_StatusLayer %in% input$ADA,]$el_stat, "<br>", "Unavailability (24-Hr Average):","<br>",
                allstops1[allstops1$ADA_StatusLayer %in% input$ADA,]$no_service, "<small>", "(Range)", "</small>")
        ),
        fillOpacity = 1,
        weight = 2,
        label = allstops1[allstops1$ADA_StatusLayer %in% input$ADA,]$name,
        opacity = 0
        ) %>%

      setView( -73.933560,40.704343,  zoom = 10.5) #%>% 
      #addResetMapButton() 

    output$stat <- renderText({
     paste((c("There are",nrow(allstops1[!duplicated(allstops1$match1)==TRUE & allstops1$ADA_StatusLayer==input$ADA,]), input$ADA, "stations.")))
    })

  })
  
  
 ################################### By line -----
  
  output$myMap2 <- renderLeaflet({
    req(input$lines)

    leaflet() %>%
      addPolygons(data=bb, stroke = FALSE, fillColor = "#0d0d0d") %>%
      addPolylines(data = sublines2,weight = 3,color = sublines2$color,label = NULL,group = 'Lines',
                   opacity = 0.25, smoothFactor = 3 )%>%
      addCircleMarkers(
        data = allstops1[allstops1$s == "2", ],
        color = allstops1[allstops1$s == "2",]$adacolors,
        radius = 4,
        popup = councilPopup(
          paste("<h4 class=","header-tiny",">","<u>",allstops1[allstops1$s == "2",]$name,"</u></h4><br>",
                 "<b>","<font size=","0.5","'>","Lines:","</b>", 
                allstops1[allstops1$s == "2",]$lines2, "<br><b>","ADA Status:", "</b>","<br>",
                allstops1[allstops1$s == '2',]$ADA_Status, "<hr>", "<b>","Elevators:", "</b>",
                allstops1[allstops1$s == "2",]$num_el, "<br>", "Type:",
                allstops1[allstops1$s == "2",]$el_stat, "<br>", "Unavailability (24-Hr Average):","<br>",
                allstops1[allstops1$s == "2",]$no_service, "<small>", "(Range)", "</small>")),
        group = "a",
        fillOpacity = 1,
        weight = 2,
        label = allstops1[allstops1$s %in% "2",]$name,
        opacity = 0) %>%
      addLegend(position = "bottomright", colors = c("#228AE6", "#82C91E", "#BE4BDB","#3B2483", "#D05D4E", "#666666"), 
                labels = unique(allstops1$ADA_StatusLayer), opacity = 1 ) %>%
      setView( -73.933560,40.704343,  zoom = 10.5) #%>% 
      #addResetMapButton()   
     
  })
  
  observeEvent({
    
    input$lines
  }, {
    leafletProxy("myMap2") %>%
      clearMarkers() %>%
      addCircleMarkers(data = allstops1[allstops1$s == input$lines, ],
                       color = allstops1[allstops1$s == input$lines,]$adacolors,
                       radius = 4,
                       popup = councilPopup(
                         paste("<h4 class=","header-tiny",">","<u>",allstops1[allstops1$s == input$lines,]$name,"</u></h4><br>",
                               "<b>","<font size=","0.5","'>","Lines:","</b>", 
                               allstops1[allstops1$s == input$lines,]$lines2, "<br><b>","ADA Status:", "</b>","<br>",
                               allstops1[allstops1$s == input$lines,]$ADA_Status,"<hr>", "<b>","Elevators:", "</b>",
                               allstops1[allstops1$s == input$lines,]$num_el, "<br>", "Type:",
                               allstops1[allstops1$s == input$lines,]$el_stat, "<br>", "Unavailability (24-Hr Average):","<br>",
                               allstops1[allstops1$s == input$lines,]$no_service, "<small>", "(Range)", "</small>")),
                       group = "a",
                       fillOpacity = 1,
                       weight = 2,
                       label = allstops1[allstops1$s %in% input$lines,]$name,
                       opacity = 0) %>%
      setView( -73.933560,40.704343,  zoom = 10.5) #%>% 
      #addResetMapButton() 
    
    output$stat1 <- renderText({ 
      paste((c("There are",nrow(allstops1[allstops1$s==input$lines,]), "stops on the", input$lines,"line.")))
    })
    output$stat1.1 <- renderText({ 
      paste((c(unique(allstops1$ADA_StatusLayer)[1], ":", round(length(which(allstops1$s==input$lines & allstops1$ADA_StatusLayer==unique(allstops1$ADA_StatusLayer)[1]))/nrow(allstops1[allstops1$s==input$lines,])*100),
               "% or ", length(which(allstops1$s==input$lines & allstops1$ADA_StatusLayer==unique(allstops1$ADA_StatusLayer)[1])),"stations.")))
    })
    output$stat1.2 <- renderText({ 
      paste((c(unique(allstops1$ADA_StatusLayer)[2], ":", round(length(which(allstops1$s==input$lines & allstops1$ADA_StatusLayer==unique(allstops1$ADA_StatusLayer)[2]))/nrow(allstops1[allstops1$s==input$lines,])*100),
               "% or ", length(which(allstops1$s==input$lines & allstops1$ADA_StatusLayer==unique(allstops1$ADA_StatusLayer)[2])),"stations.")))
    })
    output$stat1.3 <- renderText({ 
      paste((c(unique(allstops1$ADA_StatusLayer)[3], ":", round(length(which(allstops1$s==input$lines & allstops1$ADA_StatusLayer==unique(allstops1$ADA_StatusLayer)[3]))/nrow(allstops1[allstops1$s==input$lines,])*100),
               "% or ", length(which(allstops1$s==input$lines & allstops1$ADA_StatusLayer==unique(allstops1$ADA_StatusLayer)[3])),"stations.")))
    })
    output$stat1.4 <- renderText({ 
      paste((c(unique(allstops1$ADA_StatusLayer)[4], ":", round(length(which(allstops1$s==input$lines & allstops1$ADA_StatusLayer==unique(allstops1$ADA_StatusLayer)[4]))/nrow(allstops1[allstops1$s==input$lines,])*100),
               "% or ", length(which(allstops1$s==input$lines & allstops1$ADA_StatusLayer==unique(allstops1$ADA_StatusLayer)[4])),"stations.")))
    })
    output$stat1.5 <- renderText({ 
      paste((c(unique(allstops1$ADA_StatusLayer)[5], ":", round(length(which(allstops1$s==input$lines & allstops1$ADA_StatusLayer==unique(allstops1$ADA_StatusLayer)[5]))/nrow(allstops1[allstops1$s==input$lines,])*100),
               "% or ", length(which(allstops1$s==input$lines & allstops1$ADA_StatusLayer==unique(allstops1$ADA_StatusLayer)[5])),"stations.")))
    })
    
  })
  
  ################################### Combinations: Status & Line-----
  
  output$myMap3 <- renderLeaflet({
    req(input$linesc)
    req(input$ADAc)
    
    leaflet() %>%
      addPolygons(data=bb, stroke = FALSE, fillColor = "#0d0d0d") %>%
      addPolylines(data = sublines2,weight = 3,color = sublines2$color,label = NULL,group = 'Lines',
                   opacity = 0.25, smoothFactor = 3 )%>%
      addCircleMarkers(
        data = allstops1[allstops1$s =="2" & allstops1$ADA_StatusLayer %in% "Full ADA Access", ],
        color = allstops1[allstops1$s == "2" & allstops1$ADA_StatusLayer %in% "Full ADA Access",]$adacolors,
        radius = 4,
        popup = councilPopup(
          paste("<h4 class=","header-tiny",">","<u>",allstops1[allstops1$s == input$lines & allstops1$ADA_StatusLayer %in% input$ADAc,]$name,"</u></h4><br>", 
                 "<b>","<font size=","0.5","'>","Lines:","</b>", 
                allstops1[allstops1$s == "2" & allstops1$ADA_StatusLayer %in% "Full ADA Access",]$lines2, "<br><b>","ADA Status:", "</b>","<br>",
                allstops1[allstops1$s == "2" & allstops1$ADA_StatusLayer %in% "Full ADA Access",]$ADA_Status,"<hr>", "<b>","Elevators:", "</b>",
                allstops1[allstops1$s == "2" & allstops1$ADA_StatusLayer %in% "Full ADA Access",]$num_el, "<br>", "Type:",
                allstops1[allstops1$s == "2" & allstops1$ADA_StatusLayer %in% "Full ADA Access",]$el_stat, "<br>", "Unavailability (24-Hr Average):","<br>",
                allstops1[allstops1$s == "2" & allstops1$ADA_StatusLayer %in% "Full ADA Access",]$no_service, "<small>", "(Range)", "</small>")),
        group = "a",
        fillOpacity = 1,
        weight = 2,
        label = allstops1[allstops1$s %in% "2" & allstops1$ADA_StatusLayer %in% "Full ADA Access",]$name,
        opacity = 0) %>%
      addLegend(position = "bottomright", colors = c("#228AE6", "#82C91E", "#BE4BDB","#3B2483", "#D05D4E", "#666666"), 
                labels = unique(allstops1$ADA_StatusLayer), opacity = 1 ) %>%
      setView( -73.933560,40.704343,  zoom = 10.5) #%>% 
      #addResetMapButton() 
  })
  
  observeEvent({
    input$linesc
    input$ADAc
  }, {
    if (nrow(allstops1[allstops1$s == input$linesc & allstops1$ADA_StatusLayer %in% input$ADAc, ])>0){
    leafletProxy("myMap3") %>%
      clearMarkers() %>%
      addCircleMarkers(
        data = allstops1[allstops1$s == input$linesc & allstops1$ADA_StatusLayer %in% input$ADAc, ],
        color = allstops1[allstops1$s == input$linesc & allstops1$ADA_StatusLayer %in% input$ADAc,]$adacolors,
        radius = 4,
        popup = councilPopup(
          paste("<h4 class=","header-tiny",">","<u>",allstops1[allstops1$s == input$lines & allstops1$ADA_StatusLayer %in% input$ADAc,]$name,"</u></h4><br>", 
                "<b>","<font size=","0.5","'>","Lines:","</b>", 
                allstops1[allstops1$s == input$linesc & allstops1$ADA_StatusLayer %in% input$ADAc,]$lines2, "<br><b>","ADA Status:", "</b>","<br>",
                allstops1[allstops1$s == input$linesc & allstops1$ADA_StatusLayer %in% input$ADAc,]$ADA_Status,"<hr>", "<b>","Elevators:", "</b>",
                allstops1[allstops1$s == input$linesc & allstops1$ADA_StatusLayer %in% input$ADAc,]$num_el, "<br>", "Type:",
                allstops1[allstops1$s == input$linesc & allstops1$ADA_StatusLayer %in% input$ADAc,]$el_stat, "<br>", "Unavailability (24-Hr Average):","<br>",
                allstops1[allstops1$s == input$linesc & allstops1$ADA_StatusLayer %in% input$ADAc,]$no_service, "<small>", "(Range)", "</small>")),
        group = "a",
        fillOpacity = 1,
        weight = 2,
        label = allstops1[allstops1$s %in% input$linesc & allstops1$ADA_StatusLayer %in% input$ADAc,]$name,
        opacity = 0) %>%
      setView( -73.933560,40.704343,  zoom = 10.5) #%>% 
      #addResetMapButton()
      } else{
        leafletProxy("myMap3") %>%
          clearMarkers()
      }
    
    
    output$stat1.6 <- renderText({ 
      paste(c("On the", input$linesc, "line, there are",nrow(allstops1[allstops1$s==input$linesc & allstops1$ADA_StatusLayer %in% input$ADAc,]), input$ADAc, "stations."))
    })
    
  })
  
  ################################### By Stations -----
  
  output$myMap4 <- renderLeaflet({
    req(input$search)
    
    leaflet() %>%
      addPolygons(data=bb, stroke = FALSE, fillColor = "#0d0d0d") %>%
      addPolylines(data = sublines2,weight = 3,color = sublines2$color,label = NULL,group = 'Lines',
                   opacity = 0.25, smoothFactor = 3 )%>%
      addLegend(position = "bottomright", colors = c("#228AE6", "#82C91E", "#BE4BDB","#3B2483", "#D05D4E", "#666666"), 
                labels = unique(allstops1$ADA_StatusLayer), opacity = 1 ) %>%
      addCircleMarkers(
        data = allstops1[allstops1$stationline %in% "110th St: 6", ],
        color = allstops1[allstops1$stationline %in% "110th St: 6",]$adacolors,
        radius = 4,
        popup = councilPopup(
          paste("<h4 class=","header-tiny",">","<u>",allstops1[allstops1$stationline %in% "110th St: 6",]$name,"</u></h4><br>", 
                "<b>","<font size=","0.5","'>","Lines:","</b>", 
                allstops1[allstops1$stationline %in% "110th St: 6",]$lines2, "<br><b>","ADA Status:", "</b>","<br>",
                allstops1[allstops1$stationline %in% "110th St: 6",]$ADA_Status,"<hr>", "<b>","Elevators:", "</b>",
                allstops1[allstops1$stationline %in% "110th St: 6",]$num_el, "<br>", "Type:",
                allstops1[allstops1$stationline %in% "110th St: 6",]$el_stat, "<br>", "Unavailability (24-Hr Average):","<br>",
                allstops1[allstops1$stationline %in% "110th St: 6",]$no_service, "<small>", "(Range)", "</small>")),
        group = "a",
        fillOpacity = 1,
        weight = 2,
        label = as.character(allstops1[allstops1$stationline %in% "110th St: 6",]$name),
        opacity = 0) %>%
      setView( -73.933560,40.704343,  zoom = 10.5) #%>% 
      #addResetMapButton() 
    
  })
  
  output$stat1.7 <- renderText({"Choose the stations needed to complete a trip to see if they are accessible."})
  
  observeEvent({
    input$search
  }, {
    leafletProxy("myMap4") %>%
      clearMarkers() %>%
    addCircleMarkers(
      data = allstops1[allstops1$stationline %in% input$search, ],
      color = allstops1[allstops1$stationline %in% input$search,]$adacolors,
      radius = 4,
      popup = councilPopup(
        paste("<h4 class=","header-tiny",">","<u>",allstops1[allstops1$stationline %in% input$lines,]$name,"</u></h4><br>",
              "<b>","<font size=","0.5","'>","Lines:","</b>",
              allstops1[allstops1$stationline %in% input$search,]$lines2, "<br><b>","ADA Status:", "</b>","<br>",
              allstops1[allstops1$stationline %in% input$search,]$ADA_Status,"<hr>", "<b>","Elevators:", "</b>",
              allstops1[allstops1$stationline %in% input$search,]$num_el, "<br>", "Type:",
              allstops1[allstops1$stationline %in% input$search,]$el_stat, "<br>", "Unavailability (24-Hr Average):","<br>",
              allstops1[allstops1$stationline %in% input$search,]$no_service, "<small>", "(Range)", "</small>")),
      group = "a",
      fillOpacity = 1,
      weight = 2,
      label = allstops1[allstops1$stationline %in% input$search,]$name,
      opacity = 0) %>%
    setView( -73.933560,40.704343,  zoom = 10.5) #%>% 
      #addResetMapButton() 
  })
} 