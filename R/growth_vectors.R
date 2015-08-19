### growth_vectors.R --- 
## Filename: growth_vectors.R
## Description: 
## Author: Noah Peart
## Created: Wed Aug 19 14:42:19 2015 (-0400)
## Last-Updated: Wed Aug 19 17:36:49 2015 (-0400)
##           By: Noah Peart
######################################################################

vgInds <- reactive({
    with(pp, PPLOT %in% input$vgPlot & !is.na(pp[,input$vgX]) & !is.na(pp[,input$vgY]))
})

vgArrow <- reactive({
    arrow(angle=20, length=unit(0.4, "cm"), type=ifelse(input$vgArrowEnds, "open", "closed"))
})
vecGrowth <- renderPlot({
    p1 <- ggplot(pp[vgInds(),], aes_string(input$vgX, input$vgY, color="YEAR")) +
      geom_path(alpha=0.5, aes(group=id), arrow=vgArrow(), na.rm=T) +
      defaults
    
    if (input$vgPoints)
        p1 <- p1 + geom_point(alpha=0.5)
    
    if (input$vgSplit)
        if (input$vgWrap) p1 <- p1 + facet_wrap(~ SPEC)
        else p1 <- p1 + facet_grid(~ SPEC)
    p1
})

vecUI <- renderUI({
    
    sidebarLayout(
        sidebarPanel(
            selectInput("vgPlot", "Choose Plot:", choices=levels(pp$PPLOT)),
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
                checkboxInput("vgShowFit", "Show fitted")
            ),
            checkboxInput("vgSmooth", "Add smoothed spline"),
            conditionalPanel(
                condition = "input.vgSmooth == true",
                selectInput("vgSmoothMethod", "Method", list("loess", "lm"))
            ),

            hr(),
            helpText("Aesthetics", style="color: #48ca3b;"),
            checkboxInput("vgArrowEnds", "Open Arrows")
        ),

        mainPanel(
            vecGrowth
        )
    )
})
