### helpers.R --- 
## Filename: helpers.R
## Description: 
## Author: Noah Peart
## Created: Thu Aug 13 19:59:28 2015 (-0400)
## Last-Updated: Fri Aug 14 12:56:20 2015 (-0400)
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

## grep indices ordered by year
grepInOrder <- function(coln, yrs, dat) {
    unlist(sapply(paste0(coln, yrs), function(x) grep(x, names(dat))))
}

## Polar to cartesian
## deg: if input theta is in degrees
pol2cart <- function(r, theta, deg = FALSE, recycle = FALSE) {
    if (deg) theta <- theta * pi/180
    if (length(r) > 1 && length(r) != length(theta) && !recycle)
        stop("'r' vector different length than theta, if recycling 'r' values is desired 'recycle' must be TRUE")
    xx <- r * cos(theta)
    yy <- r * sin(theta)
    ## Account for machine error in trig functions
    xx[abs(xx) < 2e-15] <- 0
    yy[abs(yy) < 2e-15] <- 0
    out <- cbind(xx, yy)
    colnames(out) <- c("x", "y")
    return( out )
}
