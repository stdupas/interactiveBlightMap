setwd("/Users/josedanielcardenasrincon/Documents/Github/map.agromakers/R-space")
# setwd("/home/dupas/map.agromakers/R-space/")

source("lateBlightMap.R")
# Hay que descomentar la linea de abajo para que se pueda inicializar Shinny como aplicación web
# y comentarla para la vista de script

bligthMapS <- blightRMapFromDownloadedDate(resistance="S")
plotBlightMap(blightMap = bligthMapS)

shinyApp(ui = ui, server = server)

getandplotBligthMap <- (resistance="MS")
getandplotBligthMap <- (resistance="R")
col <- readOGR(dsn = "Data/maps/",layer = "COL_adm0")
plot(getandplotBligthMap)
plot(col,add=TRUE)
list.files()
