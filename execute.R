#setwd("/Users/josedanielcardenasrincon/Documents/Github/map.agromakers/R-space")
#setwd("/home/dupas/map.agromakers/R-space/")
setwd("/srv/shiny-server/interactiveBlightMap/R-space")

source("lateBlightMap.R")

blightRMapListFOr7daysSinceDate(removeClimateData=TRUE) 
