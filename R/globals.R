### globals.R --- 
## Filename: globals.R
## Description: 
## Author: Noah Peart
## Created: Thu Aug 13 19:57:04 2015 (-0400)
## Last-Updated: Wed Aug 19 16:42:28 2015 (-0400)
##           By: Noah Peart
######################################################################

dataloc <- c("../../treedata/")           # locations to look for data
datafiles <- c("pp.csv", "transect.csv")  # data files
temploc <- "../temp"                      # where to store temporary data

################################################################################
##
##                                 Packages
##
################################################################################
require(shiny)
require(plyr)
require(dplyr)
require(ggplot2)
require(grid)  # arrows

################################################################################
##
##                              Plotting stuff
##
################################################################################
defaults <- theme_bw()
