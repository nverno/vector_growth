### growth_vectors.R --- 
## Filename: growth_vectors.R
## Description: 
## Author: Noah Peart
## Created: Wed Aug 19 14:42:19 2015 (-0400)
## Last-Updated: Wed Aug 26 19:07:29 2015 (-0400)
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
    inds <- with(pp, {
        sum(PPLOT %in% 4 & !is.na(pp$DBH) & !is.na(pp$HT))
    })  # 414
    vgInds <- function() inds
}

################################################################################
##
##                                  Layout
##
################################################################################
## Create layout options
vgVals <- reactiveValues()

vgOptions <- list(
    selectOptions=c("PPLOT", "SPEC", "CLASS", "YEAR", "ELEVCL", "ASPCL", "DIED", "rDIED", "CPOS"),
    checkOptions=c("PPLOT", "SPLOT", "SPEC", "CLASS", "YEAR", "YRMORT", "ELEVCL", "ASPCL", "DIED",
        "STAT", "CPOS", "rDIED"),
    colorOptions=c("PPLOT", "SPLOT", "SPEC", "CLASS", "YEAR", "YRMORT", "ELEVCL", "ASPCL", "DIED",
        "STAT", "CPOS", "rDIED"),
    shapeOptions=c("PPLOT", "SPLOT", "SPEC", "CLASS", "YEAR", "YRMORT", "ELEVCL", "ASPCL", "DIED",
        "STAT", "CPOS", "rDIED"),
    splitOptions=c("PPLOT", "SPLOT", "SPEC", "CLASS", "YEAR", "YRMORT", "ELEVCL", "ASPCL", "DIED",
        "STAT", "CPOS", "rDIED")
)

output$vgLayout <- renderUI({
    fluidPage(
        fluidRow(class="vgLayoutRow1",
                 column(width=2, class="vgLayoutRow1 colEven",
                        checkboxGroupInput("vgSelect", "Selectors:", choices=vgOptions$selectOptions,
                                           selected=c("PPLOT"))),
                 column(width=2, class="vgLayoutRow1 colOdd",
                        checkboxGroupInput("vgCheck", "Group Input:", choices=vgOptions$checkOptions,
                                           selected=c("CLASS")))
                 ),
        hr(),
        fluidRow(actionButton("vgMakeInterface", "Make Interface") ),
        tags$head(tags$style("
.vgLayoutRow1{height:400px;}
.colEven{background-color: rgba(0,20,0,0.1);}
.colOdd{background-color: rgba(0,0,0,0);}"
                             ))
    )
})

## Dynamic user interface
output$vgInterface <- renderUI({
    if (input$vgMakeInterface > 0) {
        isolate({
            ## Create input widgets
            selectors <- lapply(input$vgSelect, function(x) {
                cs <- if (is.factor(dat()[,x])) levels(dat()[,x]) else unique(dat()[,x])
                sel <- names(which.max(table(dat()[,x])))
                selectInput(sprintf("vgSelect%s", x), x, choices=cs, selected=sel)
            })
            checks <- lapply(input$vgCheck, function(x) {
                cs <- if (is.factor(dat()[,x])) levels(dat()[,x]) else unique(dat()[,x])
                checkboxGroupInput(sprintf("vgCheck%s", x), x, choices=cs, selected=cs, inline=T)
            })
            color <- selectInput("vgColor", "Color:", choices=vgOptions$colorOptions, selected="YEAR")
            shape <- selectInput("vgShape", "Shape:", choices=vgOptions$shapeOptions, selected="SPEC")
            split <- selectInput("vgSplitBy", "Split by:", choices=vgOptions$shapeOptions, selected=NULL)
            
            sidebarLayout(
                sidebarPanel(
                    selectInput("vgY", "Y", choices=c("HT", "HTOBS", "DBH", "BV", "BA")),
                    selectInput("vgX", "X", choices=c("DBH", "BV", "BA", "HT", "HTOBS")),
                    selectors,
                    checks,
                    color,
                    shape,
                    checkboxInput("vgSplit", "Split Graphs"),
                    conditionalPanel(
                        condition = "input.vgSplit == true",
                        split,
                        checkboxInput("vgWrap", "Wrap Output")
                    ),
                    checkboxInput("vgPoints", "Points"),
                    conditionalPanel(
                        condition = 'input.vgPoints == true',
                        checkboxInput('vgStat', 'Show status when measured')
                    ),
                    conditionalPanel(
                        condition = "input.vgY == 'HT' && input.vgX == 'DBH'",
                        checkboxInput("vgShowFit", "Show fitted (not working)")
                    ),
                    checkboxInput("vgSmooth", "Add smoothed spline"),
                    conditionalPanel(
                        condition = "input.vgSmooth == true",
                        selectInput("vgSmoothMethod", "Method", list("loess", "lm"))
                    ),
                    hr(),
                    helpText("Mortality", style="color: #48ca3b;"),
                      checkboxInput("vgShowDied", "Show died"),

                    hr(),
                    helpText("Aesthetics", style="color: #48ca3b;"),
                      checkboxInput("vgArrowEnds", "Open Arrows")
                ),

                mainPanel(
                    vecGrowth
                )
            )
        })
    }
})


################################################################################
##
##                                   Main
##
################################################################################
vgInds <- reactive({
    Reduce("&", c(
        lapply(input$vgSelect, function(x)
            dat()[,x] %in% input[[sprintf("vgSelect%s", x)]]),
        lapply(input$vgCheck, function(x)
            dat()[,x] %in% input[[sprintf("vgCheck%s", x)]])
    )) & !is.na(dat()[,input$vgX]) & !is.na(dat()[,input$vgY])
})

vgArrow <- reactive({
    arrow(angle=20, length=unit(0.4, "cm"), type=ifelse(input$vgArrowEnds, "open", "closed"))
})

vgSamp <- reactive({
    samp <- dat()[vgInds(),]
    samp$cols <- if (input$vgShowDied) {
        with(dat()[vgInds(),], {
            died <- sapply(split(DIED, id), function(x) any(x==1, na.rm=T))
            factor(1L+died[match(dat()[vgInds(), "id"], names(died))], levels=2:1)
        })
    } else factor(dat()[vgInds(), input$vgColor])
    samp
})

vecGrowth <- renderPlot({
    p1 <- ggplot(vgSamp(), aes_string(input$vgX, input$vgY, color="cols")) +
      geom_path(alpha=0.5, aes(group=id), arrow=vgArrow(), na.rm=T) +
      defaults

    if (input$vgShowDied) {
        p1 <- p1 + scale_color_discrete("Died", breaks=2:1, labels=c("died", "lived"))
    } else
        p1 <- p1 + scale_color_discrete(input$vgColor)
    
    if (input$vgPoints) {
        if (input$vgStat) {
            p1 <- p1 + geom_point(alpha=0.5, aes(shape=STAT)) +
              scale_shape_manual(values=c(16,4))
        } else
            p1 <- p1 + geom_point(alpha=0.5, aes_string(shape=input$vgShape))
    }
    
    if (input$vgSplit)
        if (input$vgWrap) p1 <- p1 + facet_wrap(as.formula(paste("~", input$vgSplitBy)))
        else p1 <- p1 + facet_grid(as.formula(paste("~", input$vgSplitBy)))
    
    if (input$vgSmooth) {
        p1 <- p1 + geom_smooth(method=input$vgSmoothMethod, alpha=0.05, na.rm=T)
    }
    p1
})

## vecUI <- renderUI({
##     sidebarLayout(
##         sidebarPanel(
##             selectInput("vgPlot", "Choose Plot:", choices=levels(dat()$PPLOT)),
##             selectInput("vgY", "Y", choices=c("HT", "HTOBS", "DBH", "BV", "BA")),
##             selectInput("vgX", "X", choices=c("DBH", "BV", "BA", "HT", "HTOBS")),
##             checkboxInput("vgSplit", "Split by Species"),
##             conditionalPanel(
##                 condition = "input.vgSplit == true",
##                 checkboxInput("vgWrap", "Wrap Output")
##             ),
##             checkboxInput("vgPoints", "Points"),
##             conditionalPanel(
##                 condition = "input.vgY == 'HT' && input.vgX == 'DBH'",
##                 checkboxInput("vgShowFit", "Show fitted (not working)")
##             ),
##             checkboxInput("vgSmooth", "Add smoothed spline"),
##             conditionalPanel(
##                 condition = "input.vgSmooth == true",
##                 selectInput("vgSmoothMethod", "Method", list("loess", "lm"))
##             ),
##             hr(),
##             helpText("Mortality", style="color: #48ca3b;"),
##               checkboxInput("vgShowDied", "Show died"),

##             hr(),
##             helpText("Aesthetics", style="color: #48ca3b;"),
##               checkboxInput("vgArrowEnds", "Open Arrows")
##         ),

##         mainPanel(
##             vecGrowth
##         )
##     )
## })

################################################################################
##
##                                   Info
##
################################################################################
vgPointInfo <-
    'Points marked with \'X\' were dead when measured (should all be from 1980s).'

## tags$head(tags$style("#vgInfo{
## background-color: green;
## }"))

output$vgInfo <- renderText({
    if (input$vgPoints) vgPointInfo
})



