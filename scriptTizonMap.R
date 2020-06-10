setwd("/Users/josedanielcardenasrincon/Desktop/map.agromakers/R-space")
# setwd("/home/dupas/map.agromakers/R-space/")

source("lateBlightMap.R")
library(shiny)

# Define UI for application that draws a map
ui <- fluidPage(
  # App title ----
  titlePanel("Agromakers - Información sobre riesgo del tizón tardio para hoy"),
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      numericInput("caption1", "Longitud de la parcela (X)", -75),
      numericInput("caption2", "Latitud de la parcela (Y)", 5),
      selectInput("resistencia", "Seleccionar resistencia varietal",
                  c("Susceptible" = "S",
                    "Resistente" = "R",
                    "Moderada" = "MS")),
      h4("Predicción de severidad de los ataques en la coordenada Blight Units (Grünwald 2002)"),
      verbatimTextOutput("value")
    ),
    # Main panel for displaying outputs ----
    mainPanel(mainPanel(fluid = TRUE, plotOutput('map'))
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  output$map <- renderPlot({
    plotBlightMap(blightMap=getBlight(resistance=input$resistencia), coords=cbind(x=input$caption1, y=input$caption2))
  })
  output$value <- renderPrint( {extract(getBlight(resistance=input$resistencia), cbind(x=input$caption1, y=input$caption2))} )
}

shinyApp(ui = ui, server = server)