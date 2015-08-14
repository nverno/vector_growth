### globals.R --- 
## Filename: globals.R
## Description: 
## Author: Noah Peart
## Created: Thu Aug 13 19:57:04 2015 (-0400)
## Last-Updated: Thu Aug 13 20:07:23 2015 (-0400)
##           By: Noah Peart
######################################################################

dataloc <- c("../../treedata/")           # locations to look for data
datafiles <- c("pp.csv", "transect.csv")  # data files

################################################################################
##
##                                 Packages
##
################################################################################
require(shiny)
require(plyr)
require(dplyr)
