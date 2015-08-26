### subset.R --- 
## Filename: subset.R
## Description: Interface to subset the data
## Author: Noah Peart
## Created: Mon Aug 24 18:28:13 2015 (-0400)
## Last-Updated: Wed Aug 26 13:23:23 2015 (-0400)
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
## Selections
output$chooser <- renderUI({
    fluidRow(
        column(width=2,
               helpText("Species Selectors:"), 
               actionButton("vgAllSpec", "Select All"),
               hr(),
               actionButton("vgNoneSpec", "Select None"),
               hr(),
               actionButton("vgMainThree", "Main Three")),
        column(width=2,
               checkboxGroupInput("vgSpec", "Species:", choices=levels(pp$SPEC), selected="ABBA")),
        column(width=2,
               checkboxGroupInput("vgAspect", "Aspect:",
                                  choices=levels(pp$ASPCL), selected=levels(pp$ASPCL))),
        column(width=2, 
               checkboxGroupInput("vgElev", "Elevation:",
                                  choices=levels(pp$ELEVCL), selected=levels(pp$ELEVCL)))
    )
})

## Species Observers
observeEvent(input$vgAllSpec,
             updateCheckboxGroupInput(session, "vgSpec", "Species:",
                                      choices=levels(pp$SPEC),
                                      selected=levels(pp$SPEC))
             )

observeEvent(input$vgNoneSpec,
             updateCheckboxGroupInput(session, "vgSpec", "Species:",
                                      choices=levels(pp$SPEC),
                                      selected=NULL)
             )
observeEvent(input$vgMainThree,
             updateCheckboxGroupInput(session, "vgSpec", "Species:",
                                      choices=levels(pp$SPEC),
                                      selected=c("ABBA", "PIRU", "BECO"))
             )

## Create subset
dat <- reactive({
    droplevels(with(pp, pp[SPEC %in% input$vgSpec &
                             ASPCL %in% input$vgAspect &
                           ELEVCL %in% input$vgElev, ]))
})
