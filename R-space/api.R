# api.R 
setwd("/srv/shiny-server/interactiveBlightMap/R-space")

source("lateBlightMap.R")

maps <- loadMaps()

#* @get /coordinates
add <- function(long, lat, res){
  return(list(
    dia1 = extract(maps[[res]][[1]], cbind(x=long, y=lat)),
    dia2 = extract(maps[[res]][[2]], cbind(x=long, y=lat)),
    dia3 = extract(maps[[res]][[3]], cbind(x=long, y=lat)),
    dia4 = extract(maps[[res]][[4]], cbind(x=long, y=lat)),
    dia5 = extract(maps[[res]][[5]], cbind(x=long, y=lat)),
    dia6 = extract(maps[[res]][[6]], cbind(x=long, y=lat)),
    dia7 = extract(maps[[res]][[7]], cbind(x=long, y=lat))
    ))
}