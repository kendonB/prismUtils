# # PRISMtoPoint - this code provides functions to download
# # PRISM weather data and pull the data for a set of given
# # points with lat-long coordinates or a
# # SpatialPointsDataFrame.  Output can be an .RData, .dta, or
# # a .csv file. Be sure to set the working directory to the
# # folder that contains PRISMDownloadFunction.R.  
# #
# # Copyright (C) 2014 Kendon Bell
# # 
# # This program is free software: you can redistribute it
# # and/or modify it under the terms of the GNU General Public
# # License as published by the Free Software Foundation,
# # either version 3 of the License, or (at your option) any
# # later version.  
# # 
# # This program is distributed in the hope
# # that it will be useful, but WITHOUT ANY WARRANTY; without
# # even the implied warranty of MERCHANTABILITY or FITNESS FOR
# # A PARTICULAR PURPOSE. See the GNU General Public License
# # for more details.  
# # 
# # You should have received a copy of the
# # GNU General Public License along with this program.  If
# # not, see http://www.gnu.org/licenses/.  
# #
# # The data these scripts use are subject to the 
# # PRISM Gridded Climate Data -
# # Terms of Use. As of 07/06/2014, these are available at
# # http://www.prism.oregonstate.edu/documents/PRISM_terms_of_use.pdf
# # NOTE in particular the 'Data' section: 'All data (gridded,
# # polygon, tabular, graphical) retrieved from the website or
# # otherwise provided on the website may be freely reproduced
# # and distributed for non-commercial purposes.  
# # 
# # When referring to the data, the source must be clearly and
# # prominently stated and include, at a minimum, our name,
# # URL, and the date of data creation. For example: PRISM
# # Climate Group, Oregon State University,
# # http://prism.oregonstate.edu, created 4 Feb 2004.
# # 
# # Sale or other commercial use of the data must be arranged by
# # contacting the PRISM Climate Group.'
# # 
# # Kendon Bell
# # kmb56@berkeley.edu
# # 207 Giannini Hall, Berkeley CA 94720
# 
# getWeatherAtPoints <- function(type, last, first, dirOut, fileNameOut, 
#   weatherDir, daily = TRUE, download = FALSE, spatialPts = NULL, 
#   latLong = NULL, fileOut = "RData") {
#   # DESCRIPTION: The following function takes a contiguous set
#   # of dates (last and first) and a set of coordinates (either
#   # from a spatialPts or a dataframe) and uses a
#   # point-in-polygon algorithm to get the PRISM weather for
#   # that set of points. WORKING DIRECTORY MUST CONTAIN
#   # PRISMDownloadFunction.R REQUIRES: rgdal, sp VARIABLES:
#   # type=a character vector denoting the datatype(s) to be
#   # downloaded.  'tmax', 'tmin', 'ppt' last=a string giving the
#   # last date/month to be collected in a 8/6 character format.
#   # eg. '20121126' or '201211'.  first=a string giving the
#   # first date/month to be collected in a 8/6 character format.
#   # eg. '20121126' or '201211'.  dirOut=directory for the
#   # output file fileNameOut=filename (excluding extension) for
#   # the output file weatherDir= location for downloaded or
#   # predownloaded PRISM files daily=logical for daily vs
#   # monthly download=logical for new downloads vs predownloaded
#   # files.  spatialPts=optional spatialPointsDataFrame with
#   # locations to be collected.  One of spatialPts or latLong
#   # must be specified.  latLong=optional dataframe with id,
#   # lat, long as columns. One of spatialPts or latLong must be
#   # specified.  fileOut= 'RData', 'dta', or 'csv'.  RETURN
#   # VALUE: None. The function saves a dataframe containing the
#   # id, lat, long, and weather variables.  EXAMPLES: latLong <-
#   # data.frame(id= c(1,2), lat=c(37.838429, 37.872459),
#   # long=c(-122.261496, -122.275580))
#   # setwd('C:/Users/Kenny/Dropbox/PhD/PRISM R files')
#   # getWeatherAtPoints(type = c('tmax','ppt'), last =
#   # '20070109', first = '20070111', fileNameOut = 'fileName',
#   # dirOut = 'E:/PRISM Data/weatherforpoints', weatherDir =
#   # 'E:/PRISM Data', daily = TRUE, download = TRUE, latLong =
#   # latLong, fileOut = 'dta')
#   require(sp)
#   require(rgdal)
#   require(foreach)
#   
#   source("PRISMDownloadFunction.R")
#   origWD <- getwd()
#   # A few input checks
#   if ((is.null(spatialPts) & is.null(latLong)) | (!is.null(spatialPts) & 
#     !is.null(latLong))) {
#     stop("Provide exactly one of spatialPts or latLong")
#   }
#   if (!(fileOut %in% c("RData", "dta", "csv"))) {
#     stop("fileOut not correctly specified. Choose one of \"RData\", \"dta\", or \"csv\".")
#   }
#   
#   # Download the files if needed
#   if (download) {
#     if (daily) {
#       for (typeName in type) {
#         downloadAllPrismDaily(directory = weatherDir, 
#           lastDay = last, firstDay = first, type = typeName)
#       }
#     } else {
#       for (typeName in type) {
#         downloadAllPrismMonthly(directory = weatherDir, 
#           lastMonth = last, firstMonth = first, type = typeName)
#       }
#     }
#   }
#   setwd(weatherDir)
#   # Will loop through day by day, discarding the grid each day
#   # to economize on memory
#   
#   if (daily) {
#     # Generate a vector of dates to collect
#     last <- as.Date(x = last, format = "%Y%m%d")
#     
#     first <- as.Date(x = first, format = "%Y%m%d")
#     
#     datesToCollect <- sapply(0:(last - first), FUN = function(x) {
#       format(last - x, format = "%Y%m%d")
#     })
#     
#     weatherDf <- foreach(date = datesToCollect, .combine = rbind) %do% 
#       {
#         # Will build up a SpatialPointsDataFrame
#         if (!is.null(latLong)) {
#           spatialPts <- SpatialPointsDataFrame(coords = latLong[, 
#           c("long", "lat")], data = latLong)
#         }
#         
#         spatialPts$date <- date
#         
#         for (typeName in type) {
#           fileName <- paste(weatherDir, "/", typeName, 
#           "/", "PRISM_", typeName, "_stable_4kmD1_", 
#           date, "_bil.bil", sep = "")
#           
#           # Read the grid in
#           grid <- readGDAL(fname = fileName, silent = TRUE)
#           
#           proj4string(spatialPts) <- proj4string(grid)
#           
#           spatialPts@data[, typeName] <- over(x = spatialPts, 
#           y = grid)
#         }
#         spatialPts@data
#       }
#   } else {
#     # Generate a vector of dates to collect
#     last <- as.Date(x = paste(last, "01", sep = ""), format = "%Y%m%d")
#     
#     first <- as.Date(x = paste(first, "01", sep = ""), format = "%Y%m%d")
#     
#     monthsToCollect <- sapply(seq(from = first, to = last, 
#       by = "1 months"), FUN = function(x) {
#       format(x, format = "%Y%m")
#     })
#     
#     weatherDf <- foreach(month = monthsToCollect, .combine = rbind) %do% 
#       {
#         # Will build up a SpatialPointsDataFrame
#         if (!is.null(latLong)) {
#           spatialPts <- SpatialPointsDataFrame(coords = latLong[, 
#           c("long", "lat")], data = latLong)
#         }
#         
#         spatialPts$month <- month
#         
#         for (typeName in type) {
#           fileName <- paste(weatherDir, "/", typeName, 
#           "/", "PRISM_", typeName, "_stable_4kmM2_", 
#           month, "_bil.bil", sep = "")
#           
#           # Read the grid in
#           grid <- readGDAL(fname = fileName, silent = TRUE)
#           
#           proj4string(spatialPts) <- proj4string(grid)
#           
#           spatialPts@data[, typeName] <- over(x = spatialPts, 
#           y = grid)
#         }
#         spatialPts@data
#       }
#   }
#   # Now have everything downloaded and in memory in the
#   # weatherDf object Need to save
#   dir.create(file.path(dirOut), showWarnings = FALSE)
#   setwd(dirOut)
#   if (fileOut == "RData") {
#     save(weatherDf, file = paste(fileNameOut, ".RData", sep = ""))
#   } else if (fileOut == "dta") {
#     require(foreign)
#     write.dta(dataframe = weatherDf, file = paste(fileNameOut, 
#       ".dta", sep = ""))
#   } else if (fileOut == "csv") {
#     write.csv(x = weatherDf, file = paste(fileNameOut, ".csv", 
#       sep = ""))
#   }
#   setwd(origWD)
# } 
