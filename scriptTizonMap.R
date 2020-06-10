setwd("/Users/josedanielcardenasrincon/Documents/GitHub/map.agromakers/R-space")
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
      numericInput("caption1", "Coordenada X", -75),
      numericInput("caption2", "Coordenada Y", 0),
      selectInput("severidad", "Seleccionar resistencia",
                  c("Severa" = "S",
                    "Regular" = "R",
                    "Moderada" = "MS")),
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
    plotBlightMap(blightRMapFromDownloadedDate(resistance=input$severidad))
  })
  output$value <- renderPrint( {extract(blightRMapFromDownloadedDate(resistance=input$severidad), cbind(x=input$caption1, y=input$caption2))} )
}

shinyApp(ui = ui, server = server)
