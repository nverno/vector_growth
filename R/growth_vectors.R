### growth_vectors.R --- 
## Filename: growth_vectors.R
## Description: 
## Author: Noah Peart
## Created: Wed Aug 19 14:42:19 2015 (-0400)
## Last-Updated: Wed Aug 26 13:24:09 2015 (-0400)
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


################################################################################
##
##                                   Main
##
################################################################################
vgInds <- reactive({
    with(dat(), PPLOT %in% input$vgPlot & !is.na(dat()[,input$vgX]) & !is.na(dat()[,input$vgY]))
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
    } else dat()[vgInds(), "YEAR"]
    samp
})

vecGrowth <- renderPlot({
    p1 <- ggplot(vgSamp(), aes_string(input$vgX, input$vgY, color="cols")) +
      geom_path(alpha=0.5, aes(group=id), arrow=vgArrow(), na.rm=T) +
      defaults

    if (input$vgShowDied) {
        p1 <- p1 + scale_color_discrete("Died", breaks=2:1, labels=c("died", "lived"))
    } else
        p1 <- p1 + scale_color_discrete("Year")

    if (input$vgPoints)
        p1 <- p1 + geom_point(alpha=0.5, aes(shape=STAT)) +
          scale_shape_manual(values=c(16,4))
    
    if (input$vgSplit)
        if (input$vgWrap) p1 <- p1 + facet_wrap(~ SPEC)
        else p1 <- p1 + facet_grid(~ SPEC)

    if (input$vgSmooth) {
        p1 <- p1 + geom_smooth(method=input$vgSmoothMethod, alpha=0.05)
    }
    p1
})

vecUI <- renderUI({
    
    sidebarLayout(
        sidebarPanel(
            selectInput("vgPlot", "Choose Plot:", choices=levels(dat()$PPLOT)),
            selectInput("vgY", "Y", choices=c("HT", "HTOBS", "DBH", "BV", "BA")),
            selectInput("vgX", "X", choices=c("DBH", "BV", "BA", "HT", "HTOBS")),
            checkboxInput("vgSplit", "Split by Species"),
            conditionalPanel(
                condition = "input.vgSplit == true",
                checkboxInput("vgWrap", "Wrap Output")
            ),
            checkboxInput("vgPoints", "Points"),
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



