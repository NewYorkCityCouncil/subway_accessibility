library(shiny)
library(dplyr)
source("data.R")
source("tabModule.R")

ui <- fluidPage(
  tags$style(type="text/css", ".recalculating { opacity: 1.0; }"),
  titlePanel("View ADA Accessibility By:"),
  tabsetPanel(id = "type", tags$head(
    tags$style(HTML(".leaflet-container { background: #fff; }"))
  ),
              tabPanel("lines", tabModuleUI("lines")),
              tabPanel("ada", tabModuleUI("ada")),
              tabPanel("ee", tabModuleUI("ee"))
  )
)

server <- function(input, output) {
  callModule(tabModule, "lines")
  callModule(tabModule, "ada")
  callModule(tabModule, "ee")
  
}

# Run the application 
shinyApp(ui = ui, server = server)