### globals.R --- 
## Filename: globals.R
## Description: 
## Author: Noah Peart
## Created: Thu Aug 13 19:57:04 2015 (-0400)
## Last-Updated: Wed Aug 26 15:25:32 2015 (-0400)
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
require(grid)      # arrows
require(shinyjs)   # toggle/show/hid
require(devtools)  # install_github

################################################################################
##
##                              Plotting stuff
##
################################################################################
defaults <- theme_bw()
