### setup.R --- 
## Filename: setup.R
## Description: setup for vector growth interactive app
## Author: Noah Peart
## Created: Thu Aug 13 20:10:33 2015 (-0400)
## Last-Updated: Thu Aug 13 20:11:43 2015 (-0400)
##           By: Noah Peart
######################################################################
source("globals.R")
source("helpers.R", chdir = T)

if (!file.exists("temp"))
    dir.create("temp")  # store any temporary data

## Read transect/permanent plot data
if (!file.exists("temp/pp.rds") |
    !file.exists("temp/tp.rds")) {
    source("remake.R")
} else {
    pp <- readRDS("temp/pp.rds")
    tp <- readRDS("temp/tp.rds")
}
