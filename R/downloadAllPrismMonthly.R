# downloadAllPrismMonthly <- function(directory, lastMonth, firstMonth, 
#                                     type) {
#   # DESCRIPTION: The function downloads monthly PRISM data to
#   # working directory REQUIRES: none VARIABLES: directory
#   # (class=character) is the directory lastMonth
#   # (class=character) is the 6 digit date for the last month
#   # required in format 'yyyymm' firstDay (class=character) is
#   # the 6 digit date for the first month required in format
#   # 'yyyymm' type (class=character) is the type 'tmax', 'tmin',
#   # 'ppt' RETURN VALUE: None. Procedural EXAMPLES:
#   # downloadAllPrismMonthly(directory='E:/PRISM Data/tmax',
#   # lastMonth='201306', firstMonth='198101', type='tmax')
#   
#   # String utility function
#   substrRight <- function(x, n) {
#     substr(x, nchar(x) - n + 1, nchar(x))
#   }
#   
#   dir.create(file.path(directory, type), showWarnings = FALSE)
#   setwd(file.path(directory, type))
#   
#   # Generate a vector of dates to collect
#   lastMonth <- as.Date(x = paste(lastMonth, "01", sep = ""), 
#                        format = "%Y%m%d")
#   
#   firstMonth <- as.Date(x = paste(firstMonth, "01", sep = ""), 
#                         format = "%Y%m%d")
#   
#   monthsToCollect <- sapply(seq(from = firstMonth, to = lastMonth, 
#                                 by = "1 months"), FUN = function(x) {
#                                   format(x, format = "%Y%m")
#                                 })
#   
#   # This makes sure that the script doesn't get files already
#   # downloaded.
#   listDownloaded <- list.files(path = file.path(directory, 
#                                                 type))
#   
#   # This gets the last 6 digits from the file names - as of
#   # 01/26/2014, this is the 6 digit month
#   monthsDownloaded <- substrRight(gsub("[^0-9]", "", unlist(listDownloaded)), 
#                                   n = 6)
#   
#   # Remove those already downloaded
#   monthsToCollect <- setdiff(as.character(monthsToCollect), 
#                              monthsDownloaded)
#   
#   if (length(monthsToCollect) > 0) {
#     file.remove("log.txt")
#     for (i in 1:length(monthsToCollect)) {
#       gridDownloaded <- FALSE
#       tryNumber <- 1
#       while ((!gridDownloaded) & tryNumber < 11) {
#         gridDownloaded = TRUE
#         sink("log.txt", append = TRUE)
#         cat(paste("Downloading", monthsToCollect[i], 
#                   "\n"))
#         sink()
#         tryCatch(getPRISMGrid(date = monthsToCollect[i], 
#                               type = type, range = "monthly", spGrid = FALSE), 
#                  error = function(e) {
#                    sink("log.txt", append = TRUE)
#                    cat(paste(as.character(e), " Try Number = ", 
#                              tryNumber))
#                    sink()
#                    gridDownloaded <- FALSE
#                  })
#         tryNumber <- tryNumber + 1
#         if (tryNumber == 11) {
#           sink("log.txt", append = TRUE)
#           cat(paste("Downloading failed for: ", as.character(monthsToCollect[i])))
#           sink()
#         }
#       }
#     }
#   }
#   # This goes back and retries missing downloads - If something
#   # is wrong, (like no internet connection) this can get stuck
#   # in an infinite loop. So keep an eye on the log file to make
#   # sure.
#   listDownloaded <- list.files(path = directory)
#   monthsDownloaded <- substrRight(gsub("[^0-9]", "", unlist(listDownloaded)), 
#                                   n = 8)
#   missingMonths <- setdiff(as.character(monthsToCollect), monthsDownloaded)
#   if (length(missingMonths) > 0) {
#     for (i in 1:length(missingMonths)) {
#       downloadAllPrismMonthly(directory = directory, lastMonth = missingMonths[i], 
#                               firstMonth = missingMonths[i], type = type)
#     }
#   }
# } 
