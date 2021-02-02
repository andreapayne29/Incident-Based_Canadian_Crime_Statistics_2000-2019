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
            } 
            else {
                if(input$dataSource == "DataFileTotal"){
                read.csv("3510017701_crimestats2000-2019.csv")
                }
                else{
                #This means do not use any data.  This will break things.
                NULL
                }
            }
        }
        
        #I didn't need to use if{}else{if{}else{}}
        # but I do so to show how to use multiple if else statements.
    })
    
    ############################################################################
    #### TAB 1 - TOTALS ########################################################
    ############################################################################
    
    output$TotalsPlot <- renderPlot({
        
        
        #creating temp dataframe to aid in plotting
        dataToPlot = as_tibble(matrix(NA, ncol = dim(x())[2])) %>%
            rename(Date = V1, Violations = V2, Statistics = V3, Value = V4)
        
        #sorting via user input
        
        #beginning with All Violations
        if (input$violation == 'All Violations'){
            
            #total number of incidents
            if (input$statistic == 'Number of Incidents'){
                #populating new dataframe
                for (i in 1:dim(x())[1]){
                    if (x()[i, 2] == 'Total, all violations [0]'){
                        if(x()[i, 3] == 'Actual incidents'){
                            dataToPlot = dataToPlot %>% add_row(x()[i,])
                        }
                    }
                }
            }
            #total number of persons cleared
            else if (input$statistic == 'Persons Cleared'){
                #populating temp dataframe
                for (i in 1:dim(x())[1]){
                    if (x()[i, 2] == 'Total, all violations [0]'){
                        if(x()[i, 3] == 'Total cleared'){
                            dataToPlot = dataToPlot %>% add_row(x()[i,])
                        }
                    }
                }
            }
            #total adults charged
            else if (input$statistic == 'Adults (18+ yrs) Charged'){
                #populating temp dataframe
                for (i in 1:dim(x())[1]){
                    if (x()[i, 2] == 'Total, all violations [0]'){
                        if(x()[i, 3] == 'Total, adult charged'){
                            dataToPlot = dataToPlot %>% add_row(x()[i,])
                        }
                    }
                }
            }
            #total youths charged
            else if (input$statistic == 'Youths (12-17 yrs) Charged'){
                #populating temp dataframe
                for (i in 1:dim(x())[1]){
                    if (x()[i, 2] == 'Total, all violations [0]'){
                        if(x()[i, 3] == 'Total, youth charged'){
                            dataToPlot = dataToPlot %>% add_row(x()[i,])
                        }
                    }
                }
            }
            #total youths not charged
            else if (input$statistic == 'Youths (12-17 yrs) Not Charged'){
                #populating temp dataframe
                for (i in 1:dim(x())[1]){
                    if (x()[i, 2] == 'Total, all violations [0]'){
                        if(x()[i, 3] == 'Total, youth not charged'){
                            dataToPlot = dataToPlot %>% add_row(x()[i,])
                        }
                    }
                }
            }
        }
        
        
        
        dataToPlot = dataToPlot %>% remove_missing()               
        
        
        #Plotting
        
        #getting labels
        if(input$statistic == 'Number of Incidents'){
            LAB = "Number of Incidents"
        }
        else if (input$statistic == 'Persons Cleared'){
            LAB = "Persons Cleared"
        }
        else if (input$statistic == 'Adults (18+ yrs) Charged'){
            LAB = "Adults (18+ yrs) Charged"
        }
        else if (input$statistic == 'Youths (12-17 yrs) Charged'){
            LAB = "Youths (12-17 yrs) Charged"
        }
        else if (input$statistic == 'Youths (12-17 yrs) Not Charged'){
            LAB = "Youths (12-17 yrs) Not Charged"
        }
        
        
        #finding plot type
        if(input$plotType == 'Scatterplot'){
            plot(x = dataToPlot[,c(1,4)], xlab = "Year", ylab = LAB,
                 ylim = c(input$ymin, input$ymax))
            if(input$scatplotType){
                
                
                #in order to run regression, we need the total population
                #however, we don't have that. so we make it instead with the 
                #equation
                #pop = (total incidents*100,000)/incident rate per 100,000
                
                #collecting data for equation
                if(input$dataSource == "DataFileTotal"){
                    rows = 20
                }
                else{
                    rows = 10
                }
                populationMatrix = matrix(0, nrow = rows, ncol = 4)
                colnames(populationMatrix) <- c("Date", "Total Incidents", "Total Rate", "Total Population")
                
                
                addedRows = 0
                #getting totals incidents + years
                for (i in 1:dim(x())[1]){
                    if (x()[i,2] == 'Total, all violations [0]'){
                        if(x()[i,3] == 'Actual incidents'){
                            present = FALSE
                            for (j in 1:dim(populationMatrix)[1]){
                                if(x()[i,1]== populationMatrix[j,1]){
                                    present = TRUE
                                }
                            }
                            if (present == FALSE){
                                addedRows = addedRows+1
                                populationMatrix[addedRows, 1] = x()[i, 1] #year
                                populationMatrix[addedRows, 2] = x()[i, 4] #total inc
                            }
                                
                        }
                    }
                }
                #getting rate
                for (i in 1:dim(x())[1]){
                    if (x()[i,2] == 'Total, all violations [0]'){
                        if(x()[i,3] == 'Rate per 100,000 population'){
                            for(j in 1:dim(populationMatrix)[1]){
                                if(populationMatrix[j, 1] == x()[i,1]){
                                    populationMatrix[j, 3] = x()[i, 4] #rate
                                }
                            }
                            
                        }
                    }
                }
                #calc total population
                for(j in 1:dim(populationMatrix)[1]){
                    populationMatrix[j,4] = (populationMatrix[j,2]*100000)/populationMatrix[j,3]
                }
                
                
                #running regression
                linearRegression<- lm(dataToPlot$Value ~ populationMatrix[,4])
                #getting Conf Bands
                newx = seq(min(dataToPlot$Date),max(dataToPlot$Date), length.out = rows)
                conf_interval <- predict(linearRegression, newdata=data.frame(x=newx), interval="confidence",
                                         level = 0.95)
                #replotting with regression
                plot(x = dataToPlot[,c(1,4)], xlab = "Year", ylab = LAB,
                     ylim = c(input$ymin, input$ymax))
                matlines(newx, conf_interval[,1:3], col = "blue", lty=2)
                
            }
        }
        else if (input$plotType == 'Bar Plot'){
            barplot(dataToPlot$Value,
                    xlab = LAB, horiz = TRUE, 
                    xlim = c(input$xmin, input$xmax), 
                    legend.text = dataToPlot$Date, 
                    col = dataToPlot$Date)
        }
        
    })
        
})
