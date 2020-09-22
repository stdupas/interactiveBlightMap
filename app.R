#setwd("/Users/josedanielcardenasrincon/Documents/GitHub/interactiveBlightMap/R-space")
#setwd("/home/dupas/map.agromakers/R-space/")
setwd("/srv/shiny-server/interactiveBlightMap/R-space")

source("lateBlightMap.R")
library(shiny)

maps <- loadMaps()

# Define UI for application that draws a map
ui <- fluidPage(
  # App title ----
  titlePanel(paste("Agromakers - Información sobre riesgo del tizón tardio para hoy ", Sys.Date())),
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      numericInput("caption1", "Longitud de la parcela (X)", -73),
      numericInput("caption2", "Latitud de la parcela (Y)", 5),
      selectInput("resistencia", "Seleccionar resistencia varietal",
                  c("Susceptible" = "S",
                    "Resistente" = "R",
                    "Moderada" = "MS")),
      selectInput("dia", "Seleccionar día",
                  c("Hoy" = "1",
                    "Mañana" = "2",
                    "En 2 días" = "3",
                    "En 3 días" = "4",
                    "En 4 días" = "5",
                    "En 5 días" = "6",
                    "En 6 días" = "7")),
      h4("Predicción de severidad de los ataques en la coordenada Blight Units (Grünwald 2002)"),
      verbatimTextOutput("value")
    ),
    # Main panel for displaying outputs ----
    mainPanel(mainPanel(fluid = TRUE, plotOutput('map'))
    )
  )
)

# Define server logic required to draw a histogram
server <- function(session, input, output) {
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['long']])) {
      updateTextInput(session, "caption1", value = query[['long']])
    }
    if (!is.null(query[['lat']])) {
      updateTextInput(session, "caption2", value = query[['lat']])
    }
    if (!is.null(query[['res']])) {
      if(query[['res']]=='S'|query[['res']]=='R'|query[['res']]=='MS'){
      updateSelectInput(session, "resistencia",
                        label ="seleccionar resistencia varietal",
                        choices =  c("Susceptible" = "S",
                                     "Resistente" = "R",
                                     "Moderada" = "MS"),
                        selected = query['res']
      )
      }
    }
  })
  output$map <- renderPlot({
    plotBlightMap(blightMap=maps[[input$resistencia]][[parse_number(input$dia)]], coords=cbind(x=input$caption1, y=input$caption2))
  })
  output$value <- renderPrint( {extract(maps[[input$resistencia]][[parse_number(input$dia)]], cbind(x=input$caption1, y=input$caption2))} )
}

shinyApp(ui = ui, server = server)
