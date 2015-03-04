#' @export
#' @title PRISM Grid Download Function 
#' @description
#' This function downloads a PRISM weather data
#' grid to disk or the R-environment. A \code{.csv} 
#' file containing download info is also provided. 
#' If downloading
#' to disk, .bil files are downloaded; if downloading
#' to the R-environment, a \code{SpatialGridDataFrame}
#' is downloaded.
#'
#' @details The data these scripts use are subject to the 
#' PRISM Gridded Climate Data -
#' Terms of Use. As of 07/06/2014, these are available at
#' http://www.prism.oregonstate.edu/documents/PRISM_terms_of_use.pdf
#' NOTE in particular the Data section: 'All data (gridded,
#' polygon, tabular, graphical) retrieved from the website or
#' otherwise provided on the website may be freely reproduced
#' and distributed for non-commercial purposes.  
#' 
#' When referring to the data, the source must be clearly and
#' prominently stated and include, at a minimum, our name,
#' URL, and the date of data creation. For example: PRISM
#' Climate Group, Oregon State University,
#' http://prism.oregonstate.edu, created 4 Feb 2004.  
#' 
#' Sale or other commercial use of the data must be arranged by
#' contacting the PRISM Climate Group.'  
#' 
#' @param date An 8 character date string 'yyyymmdd' 
#'         for daily or a 6 character date string 'yyyymm' 
#'         for monthly.
#' @param type A string denoting the datatype to be downloaded. 
#'         "tmax", "tmin", "ppt".
#' @param range String "daily" or "monthly".
#' @param spGrid \code{logical} for \code{SpatialGridDataFrame} return value 
#'           vs storing in working directory. Default is \code{TRUE}.
#' @return Either a SpatialGridDataFrame from the sp package 
#' (spGrid==TRUE) or
#' the bil and associated files are stored in the working
#' directory and the function returns TRUE/FALSE depending on
#' the success of the unzipping.  The current naming
#' convention for the bil files (as of 12/13/2013) are for
#' daily: "PRISM_type_stable_4kmD1_yyyymmdd_bil.bil" e.g.
#' "PRISM_tmax_stable_4kmD1_19990101_bil.bil"
#' and monthly: "PRISM_type_stable_4kmM2_yyyymm_bil.bil"
#' 
#' @examples
#' \dontrun{
#' grid1 <- getPRISMGrid(date="19990101", type="tmax", range="daily",
#'   spGrid=TRUE)
#' grid2 <- getPRISMGrid(date="199901",
#'   type="tmax", range="monthly", spGrid=TRUE)
#' getPRISMGrid(date="19990101",
#' type="tmax", range="daily", spGrid=FALSE)
#' }
getPRISMGrid <- function(date, type, range = "daily", spGrid = TRUE) {
  if (!is.logical(spGrid)){
    stop("spGrid must be of class logical")
  } else if (!(grepl("[0-9]", date) & nchar(date) %in% c(6,8))) {
    stop("date must be of form yyyymm for monthly or yyyymmdd for daily")
  } else if (!(range %in% c("daily", "monthly"))) {
    stop("range must be \"daily\" or \"monthly\"")
  } else if (!(type %in% c("tmax", "tmin", "ppt"))){
    stop("type must be \"tmax\", \"tmin\", \"ppt\"")
  }
  
  # get a temporary .zip file name
  fileName <- paste(tempfile(pattern = paste(date, type, sep = "")), 
                    ".zip", sep = "")
  
  # The url with type, date, and range
  url <- paste("http://www.prism.oregonstate.edu/fetchData.php?type", 
               "=bil&kind=recent&elem=", type, "&range=", range, "&temporal=", 
               date, sep = "")
  
  # Download the file to fileName
  download.file(url = url, mode = "wb", destfile = fileName)
  
  # if a SpatialGridDataFrame is to be returned:
  if (spGrid) {
    # unzip the file to the working directory.
    tmpFile <- unzip(zipfile = fileName, overwrite = TRUE)
    
    # read into R
    spGrid <- readGDAL(fname = tmpFile[1])
    
    # delete the file
    file.remove(tmpFile)
    
    return(spGrid)
  } else {
    # unzip the file to the working directory
    itWorked <- tryCatch(unzip(zipfile = fileName, overwrite = TRUE), 
                         error = function(e) {
                           FALSE
                         }, warning = function(w) {
                           FALSE
                         })
    
    # delete the zip file
    file.remove(fileName)
    
    success <- is.character(itWorked)
    
    # Create the download log file if doesn't yet exist.
    if (!file.exists("downloadLog.csv")){
      logDf <- data.table(time=Sys.time(), id=paste0(date, type, range), downloadSuccess=success)
      write.csv(x = logDf, file = "downloadLog.csv")
    } else {
      logDf <- data.table(read.csv("downloadLog.csv", 
                                   colClasses=c("character", "POSIXct", 
                                                "character", "logical")))
      index <- which(logDf$id == paste0(date, type, range))
      if (length(index) > 0){
        # Replace the row if it's there already
        logDf[index, ] <- data.table(X = as.character(index), time=Sys.time(), id=paste0(date, type, range), downloadSuccess=success)
      } else {
        # new row
        logDf <- rbindlist(list(logDf,  
                                data.table(X = as.character(NROW(logDf) + 1), 
                                           time=Sys.time(), 
                                           id=paste0(date, type, range), 
                                           downloadSuccess=success)))
      }
    }
    return(itWorked)
  }
}
