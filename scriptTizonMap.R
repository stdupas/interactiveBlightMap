setwd("/Users/josedanielcardenasrincon/Documents/Github/map.agromakers/R-space")
# setwd("/home/dupas/map.agromakers/R-space/")

source("lateBlightMap.R")
#plotBlightMap(blightMap = bligthMapS)

shinyApp(ui = ui, server = server)
