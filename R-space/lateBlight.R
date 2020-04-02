##############################################################################
# title         : 01 - SimcCast_Blight_Units.R;
# purpose       : recreate SimCast model as implemented by:
#                 Grünwald, N. J., Montes, G. R., Saldana, H. L., Covarrubias,
#                 O. A. R., & Fry, W. E. (2002).
#                 Potato Late Blight Management in the Toluca Valley: Field
#                 Validation of SimCast Modified for
#                 Cultivars with High Field Resistance. Plant Disease, 86,
#                 1163–1168.;
# producer      : prepared by A. Sparks and K. Garrett;
# last update   : in Toowoomba, Qld, Australia, Jun 2016;
# inputs        : HUSWO weather data to calculate blight units for each weather
#                  station in the dataset;
# outputs       : blight unit values, averaged hourly weather data to daily for
#                 each weather station;
# remarks 1     : cultivar resistance values are changed on lines 28-30;
# remarks 2     : this model does not attempt to recreate the entire SimCast
#                 model, only the blight unit portion
#                 the fungicide unit portion is not a part of this script and
#                 was not written in R;
# remarks 3     : blight units calculated using this script are used in the
#                 creation of the SimCastMeta model;
# license       : GPL2;
##############################################################################

# Select the resistance level that should be run

resistance = "S"
#resistance = "MS"
#resistance = "R"

# Load libraries ---------------------------------------------------------------
setwd("/Users/josedanielcardenasrincon/Documents/map.agromakers/R-space")
library("readr")
install.packages("readr")
ConsR <- NULL
DayR <- NULL
blightR <- NULL

# Run this function to generate blight unit calculations for the HUSWO data set
DailyBlightUnitFiles <- function() {
  files <- list.files("Data", pattern = ".csv$", full.names =  TRUE)
  for (i in files) {
    #weather_data <- as.data.frame(read_tsv(i))
    weather_data <- read.csv(i, sep=";",header=TRUE)
    #weather_data[, 6:7] <- round(weather_data[, 6:7], 1)
    #colnames(weather_data) <-
    #c("stationID",
    # "date",
    #"year",
    #"month",
    #"day",
    #"hour",
    #"temperature",
    #"relativeHumidity")
    blight_calcs <-
      DayR(weather_data = weather_data,
           min_year = 2019,
           max_year = 2019)
    Date <-
      paste(blight_calcs$oYear,
            blight_calcs$oMonth,
            blight_calcs$oDay,
            sep = "-")
    weather_data <- cbind(Date, blight_calcs)
    weather_data <- subset(weather_data, oYear >= 1)
    if (resistance == "S") {
      resistance <- "susceptible"
    } else if (resistance == "MS") {
      resistance <- "moderate"
    } else
      resistance <- "resistant"
    filename <- paste0(basename(i), resistance, "_dayR.txt")
    write.csv(
      weather_data,
      file = paste0("Cache/Blight Units/", filename),
    )
  }
}

DayR <- function(weather_data, min_year, max_year) {
  ##Take all weather data and output blight units for a selected range of months.
  ##This assumes that functions blightR and ConsR are available
  nYear <- max_year - min_year + 1
  oStation <-
    0 * (1:(366 * nYear)) ##longer than 365 days to account for leap years
  oYear <- oStation
  oMonth <- oStation
  oDay <- oStation
  oBlight <- oStation
  oC <- oStation
  oRH <- oStation
  uStation <- unique(weather_data$stationID)
  tC <- 0 * (1:24)
  tRH <- 0 * (1:24)
  globalIndex  <-  1
  for (iStation in (uStation)) {
    tStation = subset(weather_data, stationID == iStation)
    for (iYear in (min_year:max_year)) {
      tYear = subset(tStation, year == iYear)
      umonth  =  unique(tYear$month)
      for (i_month in (umonth)) {
        t_month <- subset(tYear, month == i_month)
        rh_test <- sum(t_month$relativeHumidity == 999)
        t_test <- sum(t_month$temperature == 999.9)
        
        if (rh_test + t_test == 0) {
          if (i_month  ==  4 |
              i_month  ==  6 | i_month  ==  9 | i_month == 11)
          {
            maxDay = 30
          } else
            if (i_month  ==  2) {
              if (iYear / 4 == round(iYear / 4)) {
                maxDay = 29
              } else {
                maxDay = 28
              }
            } else {
              maxDay = 31
            }
          for (iDay in (1:maxDay)) {
            if (iYear < max_year | i_month < 12 | iDay < 31) {
              # Hours are 13:00 - 23:00, 0:00 - 12:00
              # Note that the last day of the last year is excluded
              tDay <- subset(t_month, day == iDay)
              tDay2 <- NULL
              tC[1:12] <- tDay$temperature[13:24]
              tRH[1:12] <- tDay$relativeHumidity[13:24]
              if (iDay < maxDay) {
                tDay2 <- subset(t_month, day == iDay + 1)
              }else if (i_month < 12) {
                t_month2 <- subset(tYear, month == i_month + 1)
                tDay2 <- subset(t_month2, day == 1)
              }else{
                
              }
              
              tC[13:24] <- tDay2$temperature[1:12]
              tRH[13:24] <- tDay2$relativeHumidity[1:12]
              if(sum(is.na(tDay2$temperature[1:12])) == 0 & sum(is.na(tDay2$relativeHumidity[1:12])) == 0 & sum(is.na(tDay$temperature[13:24])) == 0 & sum(is.na(tDay$relativeHumidity[13:24])) == 0)
              {
              out1 <- ConsR(tC  =  tC, tRH  =  tRH)
              out2 <-
                blightR(consmc = out1$consmc,
                        tcons = out1$tcons,
                        resistance = resistance)
              blight <- sum(out2)
              oStation[globalIndex] <- iStation
              oYear[globalIndex] <- iYear
              oMonth[globalIndex] <- i_month
              oDay[globalIndex] <- iDay
              oBlight[globalIndex] <- blight
              oC[globalIndex] <- mean(tC)
              oRH[globalIndex] <- mean(tRH)
              globalIndex <- globalIndex + 1
              }
            }
          }
        }
      }
    }
  }
  outDaily <-
    data.frame(oStation, oYear, oMonth, oDay, oC, oRH, oBlight)
  return(outDaily)
}


ConsR <- function(tRH, tC) {
  # This outputs tcons and consmc as a list.
  # To get each assign out1 = ConsR(tRH, tC) then out1$tcons and out1$consmc
  # tRH  =  Temporary Relative Humidity
  # tC  =  Temporary *C
  consmc <- 0 * (1:12) - 99
  first <- TRUE
  tcons <- 0 * (1:12)
  cons_index <- 1
  tttemp <- (-99)
  
  for (j in (1:24)) {
    if (tRH[j] >= 90) {
      tcons[cons_index] <- tcons[cons_index] + 1
      if (first) {
        tttemp <- tC[j]
      }
      
      else{
        tttemp <- c(tttemp, tC[j])
      }
      first <- FALSE
      
      if ((tRH[j + 1] < 90 & j < 24) | j  ==  24) {
        consmc[cons_index] <- mean(tttemp)
        cons_index <- cons_index + 1
        tttemp <- (-99)
        first <- TRUE
      }
    }
  }
  cons_out <- data.frame(tcons, consmc)
  cons_out
}

## Blight Unit Calculation
blightR <- function(consmc, tcons, resistance) {
  blight_unit = 0 * (1:12)
  if (resistance == "S") {
    for (k in (1:12)) {
      if (consmc[k] <= 27 & consmc[k] >= 3) {
        if (consmc[k] >= 23 & consmc[k] <= 27) {
          if (tcons[k] >= 7 & tcons[k] <= 9) {
            blight_unit[k] = 1
          } else
            if (tcons[k] >= 10 &
                tcons[k] <= 12) {
              blight_unit[k] = 2
            } else
              if (tcons[k] >= 13 &
                  tcons[k] <= 15) {
                blight_unit[k] = 3
              } else
                if (tcons[k] >= 19 &
                    tcons[k] <= 24) {
                  blight_unit[k] = 5
                }
        }
        if (consmc[k] >= 13 & consmc[k] <= 22) {
          if (tcons[k] >= 7 & tcons[k] <= 9) {
            blight_unit[k] = 5
          } else
            if (tcons[k] >= 10 &
                tcons[k] <= 12) {
              blight_unit[k] = 6
            } else
              if (tcons[k] >= 13 &
                  tcons[k] <= 24) {
                blight_unit[k] = 7
              }
        }
        
        if (consmc[k] >= 8 & consmc[k] <= 12) {
          if (tcons[k] == 7) {
            blight_unit[k] = 1
          } else
            if (tcons[k] >= 8 &
                tcons[k] <= 9) {
              blight_unit[k] = 2
            } else
              if (tcons[k] == 10) {
                blight_unit[k] = 3
              } else
                if (tcons[k] >= 11 &
                    tcons[k] <= 12) {
                  blight_unit[k] = 4
                } else
                  if (tcons[k] >= 13 &
                      tcons[k] <= 15) {
                    blight_unit[k] = 5
                  } else
                    if (tcons[k] >= 16 &
                        tcons[k] <= 24) {
                      blight_unit[k] = 6
                    }
        }
        
        if (consmc[k] >= 3 & consmc[k] <= 7) {
          if (tcons[k] >= 10 & tcons[k] <= 12) {
            blight_unit[k] = 1
          } else
            if (tcons[k] >= 13 &
                tcons[k] <= 15) {
              blight_unit[k] = 2
            } else
              if (tcons[k] >= 16 &
                  tcons[k] <= 18) {
                blight_unit[k] = 3
              } else
                if (tcons[k] >= 19 &
                    tcons[k] <= 24) {
                  blight_unit[k] = 4
                }
        }
      }
    }
    
  }
  
  if (resistance  ==  "MS") {
    for (k in (1:12)) {
      if (consmc[k] <= 27 & consmc[k] >= 3) {
        if (consmc[k] >= 23 & consmc[k] <= 27) {
          if (tcons[k] >= 10 & tcons[k] <= 18) {
            blight_unit[k] = 1
          } else
            if (tcons[k] >= 19 &
                tcons[k] <= 24) {
              blight_unit[k] = 2
            }
        }
        
        if (consmc[k] >= 13 & consmc[k] <= 22) {
          if (tcons[k] == 7) {
            blight_unit[k] = 1
          } else
            if (tcons[k] == 8) {
              blight_unit[k] = 2
            } else
              if (tcons[k] == 9) {
                blight_unit[k] = 3
              } else
                if (tcons[k] == 10) {
                  blight_unit[k] = 4
                } else
                  if (tcons[k] >= 11 &
                      tcons[k] <= 12) {
                    blight_unit[k] = 5
                  } else
                    if (tcons[k] >= 13 &
                        tcons[k] <= 24) {
                      blight_unit[k] = 6
                    }
        }
        
        if (consmc[k] >= 8 & consmc[k] <= 12) {
          if (tcons[k] >= 7 & tcons[k] <= 9) {
            blight_unit[k] = 1
          } else
            if (tcons[k] >= 10 &
                tcons[k] <= 12) {
              blight_unit[k] = 2
            } else
              if (tcons[k] == 13 &
                  tcons[k] <= 15) {
                blight_unit[k] = 3
              } else
                if (tcons[k] >= 16 &
                    tcons[k] <= 18) {
                  blight_unit[k] = 4
                } else
                  if (tcons[k] >= 19 &
                      tcons[k] <= 24) {
                    blight_unit[k] = 5
                  }
        }
        
        if (consmc[k] >= 3 & consmc[k] <= 7) {
          if (tcons[k] >= 13 & tcons[k] <= 24) {
            blight_unit[k] = 1
          }
        }
      }
    }
  } else if (resistance  ==  "R") {
    for (k in (1:12)) {
      if (consmc[k] <= 27 & consmc[k] >= 3) {
        if (consmc[k] >= 23 & consmc[k] <= 27) {
          if (tcons[k] >= 14 & tcons[k] <= 16) {
            blight_unit[k] = 1
          }
        }
        
        if (consmc[k] >= 13 & consmc[k] <= 22) {
          if (tcons[k] == 7) {
            blight_unit[k] = 1
          } else
            if (tcons[k] == 8) {
              blight_unit[k] = 2
            } else
              if (tcons[k] == 9) {
                blight_unit[k] = 3
              } else
                if (tcons[k] >= 10 &
                    tcons[k] <= 12) {
                  blight_unit[k] = 4
                } else
                  if (tcons[k] >= 13 &
                      tcons[k] <= 24) {
                    blight_unit[k] = 5
                  }
        }
        
        if (consmc[k] >= 8 & consmc[k] <= 12) {
          if (tcons[k] >= 10 & tcons[k] <= 12) {
            blight_unit[k] = 1
          } else
            if (tcons[k] >= 13 &
                tcons[k] <= 15) {
              blight_unit[k] = 2
            } else
              if (tcons[k] >= 16 &
                  tcons[k] <= 24) {
                blight_unit[k] = 3
              }
        }
        
        if (consmc[k] >= 3 & consmc[k] <= 7) {
          if (tcons[k] >= 19 & tcons[k] <= 24) {
            blight_unit[k] = 1
          }
        }
      }
    }
  }
  return(blight_unit)
}


DailyBlightUnitFiles()

# eos