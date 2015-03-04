#' @export
#' @title Download many PRISM grid files
#' @description This function downloads PRISM data 
#' to a given directory. Creates folders and
#' subdirectories for different types if they don't
#' exist.
#' @param directory The directory to download to
#' @param last The last day/month to download
#' data for as a 6/8 character string - "yyyymm"
#' or "yyyymmdd".
#' @param first The first day/month to download
#' data for as a 6/8 character string - "yyyymm"
#' or "yyyymmdd"
#' @param type Data series to download. "tmax",
#' "tmin", "ppt", or "all"
#' 
#' @return None. Data are downloaded to disk.
#' 
#' @examples
#' \dontrun{
#' downloadAllPrismDaily(directory='E:/PRISM Data',
#' last='20130630', first='19810101', type='tmax')
#' downloadAllPrismDaily(directory='E:/PRISM Data',
#' last='20130630', first='19810101', type='tmin')
#' downloadAllPrismDaily(directory='E:/PRISM Data',
#' last='20130630', first='19810101', type='ppt')
#' }
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
#' contacting the PRISM Climate Group.
downloadManyPrism <- function(directory, last, 
                                  first, type) {
  
  dir.create(file.path(directory, type), showWarnings = FALSE)
  setwd(file.path(directory, type))
  
  # Generate a vector of dates to collect
  lastDay <- as.Date(x = lastDay, format = "%Y%m%d")
  
  firstDay <- as.Date(x = firstDay, format = "%Y%m%d")
  
  datesToCollect <- sapply(0:(lastDay - firstDay), FUN = function(x) {
    format(lastDay - x, format = "%Y%m%d")
  })
  
  # This makes sure that the script doesn't get files already
  # downloaded.
  listDownloaded <- list.files(path = file.path(directory, 
                                                type))
  
  # This gets the last 8 digits from the file names - as of
  # 01/26/2014, this is the 8 digit date.
  datesDownloaded <- substrRight(gsub("[^0-9]", "", unlist(listDownloaded)), 
                                 n = 8)
  
  # Remove those already downloaded
  datesToCollect <- setdiff(as.character(datesToCollect), datesDownloaded)
  
  if (length(datesToCollect) > 0) {
    file.remove("log.txt")
    for (i in 1:length(datesToCollect)) {
      gridDownloaded <- FALSE
      tryNumber <- 1
      while ((!gridDownloaded) & tryNumber < 11) {
        gridDownloaded = TRUE
        sink("log.txt", append = TRUE)
        cat(paste("Downloading", datesToCollect[i], "\n"))
        sink()
        tryCatch(getPRISMGrid(date = datesToCollect[i], 
                              type = type, range = "daily", spGrid = FALSE), 
                 error = function(e) {
                   sink("log.txt", append = TRUE)
                   cat(paste(as.character(e), " Try Number = ", 
                             tryNumber))
                   sink()
                   gridDownloaded <- FALSE
                 })
        tryNumber <- tryNumber + 1
        if (tryNumber == 11) {
          sink("log.txt", append = TRUE)
          cat(paste("Downloading failed for: ", as.character(datesToCollect[i])))
          sink()
        }
      }
    }
  }
  # This goes back and retries missing downloads - If something
  # is wrong, (like no internet connection) this can get stuck
  # in an infinite loop. So keep an eye on the log file to make
  # sure.
  listDownloaded <- list.files(path = directory)
  datesDownloaded <- substrRight(gsub("[^0-9]", "", unlist(listDownloaded)), 
                                 n = 8)
  missingDates <- setdiff(as.character(datesToCollect), datesDownloaded)
  if (length(missingDates) > 0) {
    for (i in 1:length(missingDates)) {
      downloadAllPrismDaily(directory = directory, lastDay = missingDates[i], 
                            firstDay = missingDates[i], type = type)
    }
  }
}
