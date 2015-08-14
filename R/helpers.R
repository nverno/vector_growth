### helpers.R --- 
## Filename: helpers.R
## Description: 
## Author: Noah Peart
## Created: Thu Aug 13 19:59:28 2015 (-0400)
## Last-Updated: Thu Aug 13 20:09:31 2015 (-0400)
##           By: Noah Peart
######################################################################

## Find data
findData <- function(locs, files) {
    found <- c()
    for (loc in locs) {
        if (any((present <- file.exists(paste0(loc, files))))) {
            found <- c(found, paste0(loc, files[present]))
            files <- files[!present]
        }
        if (length(files) == 0)
            break
    }
    return( list(found=found, missed=files) )
}

