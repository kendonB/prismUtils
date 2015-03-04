#' @export
#' @title PRISM Normal Download Function 
#' @description
#' This function downloads a PRISM weather data
#' normal grid to disk or the R-environment. If downloading
#' to disk, .bil files are downloaded; if downloading
#' to the R-environment, a \code{SpatialGridDataFrame}
#' is downloaded. A \code{.RData} file containing download
#' info is also provided.
#'
#' @details The data these scripts access are subject to the 
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
#' @param type A string denoting the datatype to be downloaded. 
#'         "tmax", "tmin", "ppt".
#' @param month A 2 character string for the month 
#' e.g. Jan -> "01". \code{NA}
#'   indicates all months.
#' @param spGrid \code{logical} for \code{SpatialGridDataFrame} return value 
#'           vs storing in working directory. Default is \code{TRUE}.
#' @return Either a SpatialGridDataFrame from the sp package 
#' (spGrid==TRUE) or
#' the .bil and associated files are stored in the working
#' directory and the function returns TRUE/FALSE depending on
#' the success of the unzipping.  The current naming
#' convention for the bil files (as of 10/19/2014) are
#' "PRISM_tmax_30yr_normal_4kmM2_mm_bil.bil"
getPRISMNormal <- function(type, month = NA, spGrid = TRUE) {
  # get a temporary .zip file name
  fileName <- paste(tempfile(pattern = paste(month, type, sep = "")), 
                    ".zip", sep = "")
  
  # The url with type, date, and range
  url <- paste("http://www.prism.oregonstate.edu/fetchData.php?type", 
               "=bil&kind=normals&elem=", type, "&spatial=4km&temporal=", 
               month, sep = "")
  
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
    return(itWorked)
  }
}
