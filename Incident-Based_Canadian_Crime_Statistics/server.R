#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    x <- reactive({
        #Use those radioButtons to select the data to use
        #Note that the input was called "dataSource" in the ui.R file
        #Here it is an element from a list.
        #I extract it using input$nameOfThing
        if (input$dataSource == "DataFile2000") {
            read.csv("3510017701_crimestats2000-2009.csv")
        } else {
            if(input$dataSource == "DataFile2010"){
                read.csv("3510017701_crimestats2010-2019.csv")
            }else{
                #This means do not use any data.  This will break things.
                NULL
            }
        }
        
        #I didn't need to use if{}else{if{}else{}}
        # but I do so to show how to use multiple if else statements.
    })
    
    ############################################################################
    #### TAB 1 - TOTALS ########################################################
    ############################################################################
    
    output$TotalsPlot <- renderPlot({
        
        dataToPlot = as_tibble(matrix(NA, ncol = dim(x())[2])) %>%
            rename(Date = V1, Violations = V2, Statistics = V3, Value = V4)
        
        
        if (input$violation == 'All Violations'){
            for (i in 1:dim(x())[1]){
                if (x()[i, 2] == 'Total, all violations [0]'){
                    dataToPlot = dataToPlot %>% add_row(x()[i,])
                    plot(x = dataToPlot[,c(1,4)])
                }
            }
            if (input$statistic == 'Number of Incidents'){
                
            }
        }
        
    })
        
})
