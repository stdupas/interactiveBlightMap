setwd("/Users/josedanielcardenasrincon/Documents/Github/map.agromakers/R-space")
# setwd("/home/dupas/map.agromakers/R-space/")

source("lateBlightMap.R")

bligthMapS <- blightRMapFromDownloadedDate(resistance="S")
bligthMapR <- blightRMapFromDownloadedDate(resistance="R")
bligthMapMS <- blightRMapFromDownloadedDate(resistance="MS")
#plotBlightMap(blightMap = bligthMapS)

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
      selectInput("severidad", "Seleccionar severidad",
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

blight <- function(resistence){
  if(resistence == "S") bligthMapS
  else if(resistence =="R") bligthMapR
  else if(resistence =="MS") bligthMapMS
}

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$map <- renderPlot({
    plotBlightMap(blight(input$severidad))
  })
  output$value <- renderPrint( {extract(blight(input$severidad), cbind(x=input$caption1, y=input$caption2))} )
}

shinyApp(ui = ui, server = server)