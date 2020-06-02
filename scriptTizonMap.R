setwd("/Users/josedanielcardenasrincon/Documents/Github/map.agromakers/R-space")
# setwd("/home/dupas/map.agromakers/R-space/")

library(raster)
library(sp)
library(chron)

rastlist <- list.files(path = "./Data/maps/", all.files=TRUE, full.names=FALSE, patern='.tif$')
list0 <- list.files(path="./Data/maps", "humedad", full.names=TRUE)
list1 <- list.files(path="./Data/maps", "temp",full.names=TRUE)
imageRH <- stack(list0)
imageTemp<-stack(list1)

#y0 <- extract(imageRH,c(1:ncell(imageRH)))
#coord <- xyFromCell(imageRH,1:ncell(imageRH))
#cbd0 <- cbind(coord,y0)
#rh_df <- as.data.frame(cbd0)

#y1 <- extract(imageTemp,c(1:ncell(imageTemp)))
#coord <- xyFromCell(imageTemp,1:ncell(imageTemp))
#cbd1 <- cbind(coord,y1)
#temp_df <- as.data.frame(cbd1)

#df <- data.frame(matrix(ncol = 6, nrow = 0))
#x <- c("stationID", "x", "y", "time", "temperature", "relativeHumidity")
#colnames(df) <- x

time <- chron(date="04/01/20", time="00:00:00")

#for (i in 1:100) {
#  for(j in (3:ncol(temp_df))){
#  df[nrow(df) + 1,] <- c(i, temp_df[i,1], temp_df[i,2], time, temp_df[i,j], rh_df[i,j] )
#  time=time+(1/24)
#  }
#}