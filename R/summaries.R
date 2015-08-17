### summaries.R --- 
## Filename: summaries.R
## Description: 
## Author: Noah Peart
## Created: Fri Aug 14 13:11:02 2015 (-0400)
## Last-Updated: Fri Aug 14 13:17:21 2015 (-0400)
##           By: Noah Peart
######################################################################

## sourced locally in vectors.Rmd

################################################################################
##
##                             Observed heights
##
################################################################################
## Sample sizes by year/plot
pp %>%
  group_by(YEAR, PPLOT) %>%
  filter(STAT == "ALIVE",
         !is.na(HTOBS)) -> htobs

htobsSpecTable <- table(htobs$YEAR, htobs$SPEC)
htobsPlotTable <- table(htobs$YEAR, htobs$PPLOT)
