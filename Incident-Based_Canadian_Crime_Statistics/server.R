#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("cleaning_data_function.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    x <- reactive({
        #Use those radioButtons to select the data to use
        #Note that the input was called "dataSource" in the ui.R file
        #Here it is an element from a list.
        #I extract it using input$nameOfThing
        if (input$dataSource == "DataFile2000") {
            cleaning_data_function(read.csv("3510017701_crimestats2000-2009.csv"))
        } else {
            if(input$dataSource == "DataFile2010"){
                cleaning_data_function(read.csv("3510017701_crimestats2010-2019.csv"))
            }else{
                #This means do not use any data.  This will break things.
                NULL
            }
        }
        
        #I didn't need to use if{}else{if{}else{}}
        # but I do so to show how to use multiple if else statements.
    })
    
    
    output$distPlot <- renderPlot({
        
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
        
    })
    
})
