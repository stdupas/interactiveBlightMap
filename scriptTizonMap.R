setwd("/Users/josedanielcardenasrincon/Documents/map.agromakers/R-space")
library(raster)
library(sp)

list0 <- list.files(path="./Data/maps/geoTIFFhumedadhorario0104202000Z", full.names=TRUE)
list1 <- list.files(path="./Data/maps/geoTIFFtemphorario0104202000Z",full.names=TRUE)
imageRH <- stack(list0)
imageTemp<-stack(list1)

y0 <- extract(imageRH,c(1:ncell(imageRH)))
coord <- xyFromCell(imageRH,1:ncell(imageRH))
cbd0 <- cbind(coord,y0)
rh_df <- as.data.frame(cbd0)
head(rh_df)

y1 <- extract(imageTemp,c(1:ncell(imageTemp)))
coord <- xyFromCell(imageTemp,1:ncell(imageTemp))
cbd1 <- cbind(coord,y1)
temp_df <- as.data.frame(cbd1)
head(temp_df)

#count(ydf, vars = c("layer.1","layer.2","layer.3","layer.4"))
#brk <- do.call(brick, lapply(list.files(path = "./Data/maps/geoTIFFhumedadhorario0104202000Z", pattern="RH*.*tif"), raster))

