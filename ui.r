ui <-

    navbarPage("ADA Accessibility of NYC Stations",
               collapsible=TRUE, inverse=FALSE,
             tabPanel("ADA Status & Line",
                      sidebarLayout(
                        sidebarPanel(
                          #h4("Filter By:"),
                          strong(textOutput("stat1.6")),
                          hr(),
                          selectInput(inputId = "linesc",
                                      label = "Choose a Line:",
                                      choices = sort(unique(allstops1$s),decreasing = FALSE),
                                      selected = sort(unique(allstops1$s),decreasing = FALSE)[2],
                                      selectize = TRUE),
                          selectInput(inputId = "ADAc",
                                      label = "Select an ADA Status:",
                                      choices = unique(allstops1$ADA_StatusLayer),
                                      selected = unique(allstops1$ADA_StatusLayer)[1],
                                      selectize = TRUE,
                                      multiple = FALSE),
                           width = 3),
                        mainPanel(width = 9,
                          tags$head(
                            tags$style(HTML(".leaflet-container { background: #fff; }"))
                          ),
                          leafletOutput("myMap3", width = "auto", height = 800)
                        )
                      )
             ),
             tabPanel("Status",
                      sidebarLayout(
                        sidebarPanel(
                            #h4("Filter By:"),
                          h5(textOutput("stat")),
                          strong(hr()) ,
                            selectInput(inputId = "ADA",
                                        label = "Select an ADA Status:",
                                        choices = unique(allstops1$ADA_StatusLayer),
                                        selected = unique(allstops1$ADA_StatusLayer)[1],
                                        selectize = TRUE),
                            width = 3),
                        mainPanel(width = 9,
                          tags$head(
                            tags$style(HTML(".leaflet-container { background: #fff; }"))
                          ),
                          leafletOutput("myMap", width = "auto", height = 800)
                        )
                        )
                      ),
             tabPanel("Lines",
                      sidebarLayout(
                        sidebarPanel(
                          #h4("Filter By:"),
                          h5(textOutput("stat1")),
                          textOutput("stat1.1"),
                          textOutput("stat1.2"),
                          textOutput("stat1.3"),
                          textOutput("stat1.4"),
                          textOutput("stat1.5"),
                          strong(hr()),
                          selectInput(inputId = "lines",
                                      label = "Choose a Line:",
                                      choices = sort(unique(allstops1$s),decreasing = FALSE),
                                      selected = sort(unique(allstops1$s),decreasing = FALSE)[2],
                                      selectize = TRUE),
                          width = 3),
                        mainPanel(width = 9,
                          tags$head(
                            tags$style(HTML(".leaflet-container { background: #fff; }"))
                          ),
                          leafletOutput("myMap2", width = "auto", height = 800)
                        )
                      )
             ),
             tabPanel("Stations",
                      sidebarLayout(
                        sidebarPanel(
                          strong(textOutput("stat1.7")),
                          hr(),
                          selectInput(inputId = "search",
                                      label = "Search Stations:",
                                      choices = sort(unique(allstops1$stationline),decreasing = FALSE),
                                      selected = sort(unique(allstops1$stationline),decreasing = FALSE)[13],
                                      selectize = TRUE,
                                      multiple = TRUE),
                          width = 3),
                        mainPanel(width = 9, tags$head(
                          tags$style(HTML(".leaflet-container { background: #fff; }")),
                          tags$script(HTML(""))
                        ),
                        leafletOutput("myMap4", width = "auto", height = 800))
                      )

             )
          )




