### subset.R --- 
## Filename: subset.R
## Description: Interface to subset the data
## Author: Noah Peart
## Created: Mon Aug 24 18:28:13 2015 (-0400)
## Last-Updated: Wed Aug 26 15:30:00 2015 (-0400)
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
    div(id = "chooser",
        fluidPage(
            fluidRow(class="chooserRow1",
                     column(width=2, class="chooserRow1 colEven",
                            helpText("Species Selectors:", style="font-weight:bold; color: grey;"), 
                            actionButton("vgAllSpec", "All", style="width:100px;"),
                            hr(style="margin-top: 0.2em; margin-bottom: 0.2em;"),
                            actionButton("vgNoneSpec", "None", style="width:100px"),
                            hr(style="margin-top: 0.2em; margin-bottom: 0.2em;"),
                            actionButton("vgMainThree", "Main Three", style="width:100px")),
                     column(width=2, class="chooserRow1 colOdd",
                            checkboxGroupInput("vgSpec", "Species:", choices=levels(pp$SPEC), selected="ABBA")),
                     column(width=2, class="chooserRow1 colEven",
                            checkboxGroupInput("vgAspect", "Aspect:",
                                               choices=levels(pp$ASPCL), selected=levels(pp$ASPCL))),
                     column(width=2, class="chooserRow1 colOdd",
                            checkboxGroupInput("vgElev", "Elevation:",
                                               choices=levels(pp$ELEVCL), selected=levels(pp$ELEVCL)))
                     ),
            hr(),
            fluidRow(actionButton("vgSubset", "Make Subset") ),
            tags$head(tags$style("
.chooserRow1{height:400px;}
.colEven{background-color: rgba(0,20,0,0.1);}
.colOdd{background-color: rgba(0,0,0,0);}"
                                 ))
        )
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
    if (input$vgSubset > 0) {
        isolate(droplevels(with(pp, pp[SPEC %in% input$vgSpec &
                                 ASPCL %in% input$vgAspect &
                                 ELEVCL %in% input$vgElev, ])))
    } else pp
})

## Text with information about subset
output$vgSubText <- renderPrint({
    if (input$vgSubset > 0) {
        isolate(
            HTML(sprintf("Species: %s\nElevatation: %s\nAspect: %s",
                    paste(input$vgSpec, collapse=","),
                    paste(input$vgElev, collapse=","),
                    paste(input$vgAspect, collapse=",")))
        )
    } else "Not Subsetted"
})
