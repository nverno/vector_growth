### canopy.R --- 
## Filename: canopy.R
## Description: 
## Author: Noah Peart
## Created: Wed Sep  2 14:22:37 2015 (-0400)
## Last-Updated: Wed Sep  2 15:43:33 2015 (-0400)
##           By: Noah Peart
######################################################################

################################################################################
##
##                                  Testing
##
################################################################################
if (interactive()) {
    require(shiny)
    input <- list(vgPlot = 4, vgX="DBH", vgY="HT", vgShowDied=TRUE)
    inds <- with(pp, PPLOT %in% 4 & !is.na(pp$DBH) & !is.na(pp$HT))
    vgInds <- function() inds
}

################################################################################
##
##                      Create canopy height variables
##
################################################################################
choices <- c("plot", "subplot", "quadrat")
nmaxTree <- 1  # number of trees to use for canopy height

pp %>% group_by(PPLOT, SPLOT) %>%
  top_n(1, HT) %>%
  select(HT) %>%
  arrange(PPLOT)

pp$can <- NA
nmax <- 1
pp <- within(pp, {
    if (choice=="plot") {
        split(can, PPLOT) <- sapply(split(HT, PPLOT), function(x)
            max, na.rm=T)
    } else if (choice=="subplot") {
        split(can, interaction(SPLOT, PPLOT)) <-
            sapply(split(HT, interaction(SPLOT, PPLOT)), max, na.rm=T)
    }
})


