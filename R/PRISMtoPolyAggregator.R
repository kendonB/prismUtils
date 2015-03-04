# # PRISMtoPolyAggregator - this code provides functions to download PRISM weather
# # data and aggregate the data to a set of given polygons, e.g. ZIP codes.
# # Copyright (C) 2014 Kendon Bell
# # 
# # This program is free software: you can redistribute it and/or modify
# # it under the terms of the GNU General Public License as published by
# # the Free Software Foundation, either version 3 of the License, or
# # (at your option) any later version.
# # 
# # This program is distributed in the hope that it will be useful,
# # but WITHOUT ANY WARRANTY; without even the implied warranty of
# # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# # GNU General Public License for more details.
# # 
# # You should have received a copy of the GNU General Public License
# # along with this program.  If not, see http://www.gnu.org/licenses/.
# # 
# # Kendon Bell 
# # kmb56@berkeley.edu
# # 207 Giannini Hall, Berkeley CA 94720
# # 
# # Requires internet access and the R-file 
# 
# # SOME of these are needed - don't remember which...
# ################################
# # Libraries and functions
# ################################
# library(raster)
# library(parallel)
# library(foreach)
# library(doParallel)
# library(Grid2Polygons)
# library(PBSmapping)
# library(rgdal)
# library(sp)
# library(maptools)
# library(maps)
# library(plyr)
# library(lfe)
# library(chron)
# library(reshape)
# library(zipcode)
# library(foreign)
# library(RCurl)
# library(gdata)
# library(maps)
# library(doBy)
# library(xtable)
# 
# 
# getGridRowsWeights <- function(geoValue, brickGrid, geoPoly, 
#                                geoPolyVar, gridArcMin){
#   ################################################################
#   # This function gets weights of the rows of a SpatialGridDataFrame
#   # (converted to a raster brick before passing to this function)
#   # corresponding to a particular individual polygon.
#   # geoValue is a numeric scalar that identifies the polygon within
#   # The SpatialPolygons dataframe geoPoly. The variable for geoValue
#   # (within geoPoly) is geoPolyVar.
#   # This returns a list of: weights corresponding to rows in the, 
#   # SpatialGridDataFrame that is converted to brickGrid.
#   # gridArcMin is the cell size in brickGrid.
#   # These rows and weights can then be used to pull weather from
#   # other PRISM grids.
#   ################################################################
#   if (is.na(geoValue)) {
#     return(NA)
#   } else {
#     # subset the given SpatialPointsPolygon to the zip
#     thisPoly <- tryCatch(geoPoly[geoPolyVar == geoValue,], error = function(e) NA)    
#     
#     if (suppressWarnings(is.na(thisPoly))) {
#       return(NA)
#     } else {
#       grid1 <- getSmallGrid(thisPoly=thisPoly, brick=brickGrid, gridArcMin=gridArcMin)
#       
#       # Convert the poly to a PolySet from PBSMapping
#       tmp1<- SpatialPolygons2PolySet(thisPoly)
#       
#       # Convert the grid to a poly then to a PolySet
#       tmp2 <- Grid2Polygons(grid1, zcol="id")
#       tmp3 <- SpatialPolygons2PolySet(tmp2)
#       
#       # Fix up the projection
#       attr(tmp1, "projection") <- "LL"
#       
#       # Get the intersections
#       tmp4 <- joinPolys(tmp1, tmp3, operation="INT")
#       
#       # rollup = 2 subtracts holes.
#       suppressMessages(suppressWarnings(areasJoin <- calcArea(tmp4, rollup=2)))
#       suppressMessages(suppressWarnings(areasGrid <- calcArea(tmp3, rollup=1)))
#       
#       # Non zero grid elements
#       nonZeroGridElements <- areasJoin$PID
#       
#       # Now pull out the areas proportions and the large grid rows
#       weights <- areasJoin$area/areasGrid$area[nonZeroGridElements]
#       rowsInBigGrid <- grid1$id[nonZeroGridElements]
#       
#       # Some grid cells have more than one polygon joins so this aggregates them
#       # and actually turns them into weights.
#       denom <- sum(weights)
#       weights <- aggregate(x=weights, by=list(rowsInBigGrid), FUN=sum)$x/denom
#       
#       return(data.frame(unique(rowsInBigGrid), weights))
#     }
#   }
# }
# 
# 
# ###############
# # Example calling:
# ##############
# 
# # # PRISM Grid object
# # spGridDaily
# # 
# # # Geographical polygon
# # zipsPoly
# #
# # # Id of polygons in the polygon file
# # ZCTA5CE10
# #
# # # list of zips to aggregate get weather for of class numeric
# # zipVar <- c(94609, 94720)
# # 
# # cl<-makeCluster(8)
# # registerDoParallel(cl)
# # ptm <- proc.time()
# # file.remove("log.txt")
# # brickGrid <- brick(spGridDaily)
# # zipsRowsWeights <- foreach(X=1:length(zipVar), .packages=c('sp', 'raster',
# #                                                                      'PBSmapping', 'maptools', 
# #                                                                      'Grid2Polygons')) %dopar% {
# #                                                                        sink("log.txt", append=TRUE)
# #                                                                        cat(paste("Starting iteration",X,"\n"))
# #                                                                        sink()
# #                                                                        tmp <- getGridRowsWeights(geoValue=zipVar[X], brickGrid=brickGrid, 
# #                                                                                                  geoPoly=zipsPoly, geoPolyVar=zipsPoly$ZCTA5CE10, gridArcMin=2.5)
# #                                                                        return(tmp)
# #                                                                      }
# 
# ### THEN ###
# 
# getWeather <- function(weatherGrid, rowsWeights){
#   newWeights <- rowsWeights[,2][which(!is.na(weatherGrid$band1[rowsWeights[,1]]))]
#   newWeights <- newWeights / sum(newWeights)
#   tmp <- as.numeric(weatherGrid$band1[rowsWeights[,1][which(!is.na(weatherGrid$band1[rowsWeights[,1]]))]] %*% newWeights)
#   return(tmp)
# }
# 
# # # Example calling:
# # 
# # # Vector of chron objects for the days to be collected 
# # datesToCollect
# # 
# # T <- length(datesToCollect)
# # 
# # cl <- makeCluster(detectCores())
# # clusterExport(cl,ls()) # Don't do this if you have a big workspace.
# # clusterEvalQ(cl, library(sp))
# # clusterEvalQ(cl, library(rgdal))
# # clusterEvalQ(cl, library(chron))
# # ptm <- proc.time()
# # file.remove("log.txt")
# # tempMaxDaily <- parSapply(cl=cl, X=1:T, FUN=function(X){
# #   options(chron.year.abb = FALSE)
# #   sink("log.txt", append=TRUE)
# #   cat(paste("Starting iteration",X,"\n"))
# #   sink()
# #   fileName <- paste("PRISM_tmax_stable_4kmD1_", datesToCollect[X], "_bil.bil", sep="")
# #   grid <- readGDAL(fname=fileName)
# #   
# #   if(is.na(grid)){
# #     return(NA)
# #   } else {
# #     weather <- sapply(1:N, FUN=function(x) {
# #       if (!is.na(zipsRowsWeights[[x]])){
# #         tmp <- getWeather(weatherGrid=grid, rowsWeights=zipsRowsWeights[[x]])
# #         return(tmp)
# #       } else if (!is.na(fipsRowsWeights[[x]])) {
# #         tmp <- getWeather(weatherGrid=grid, rowsWeights=fipsRowsWeights[[x]])
# #         return(tmp)
# #       } else if (!is.na(cbsaRowsWeights[[x]])) {
# #         tmp <- getWeather(weatherGrid=grid, rowsWeights=cbsaRowsWeights[[x]])
# #         return(tmp)
# #       } else {
# #         return(NA)
# #       }
# #     })
# #     return(weather)
# #   }
# # })
# 
