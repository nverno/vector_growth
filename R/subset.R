### subset.R --- 
## Filename: subset.R
## Description: Interface to subset the data
## Author: Noah Peart
## Created: Mon Aug 24 18:28:13 2015 (-0400)
## Last-Updated: Tue Aug 25 13:05:42 2015 (-0400)
##           By: Noah Peart
######################################################################

################################################################################
##
##                                  Testing
##
################################################################################
if (interactive()) {
    require(shiny)
    input <- list(vgSpec = "ABBA", vgX="DBH", vgY="HT", vgShowDied=TRUE)
    inds <- with(pp, {
        sum(PPLOT %in% 4 & !is.na(pp$DBH) & !is.na(pp$HT))
    })  # 414
    vgInds <- function() inds
}

################################################################################
##
##                                   Main
##
################################################################################
## Subsets:
## SPEC
chooser <- renderUI({
    inputPanel(
        checkboxGroupInput("vgSpec", "Species:", choices=levels(pp$SPEC), selected="ABBA")
    )
})

dat <- reactive({
    droplevels(with(pp, pp[SPEC %in% input$vgSpec, ]))
})
