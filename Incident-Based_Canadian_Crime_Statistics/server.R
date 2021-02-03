
# checking and installing libraries
if(!require("shiny"))
    install.packages("shiny")
library(shiny)

if(!require("tidyverse"))
    install.packages("tidyverse")
library(tidyverse)



shinyServer(function(input, output) {
    
    
    #reactive variable for data source using radio buttons
    x <- reactive({
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
                NULL
                }
            }
        }
    })
    
################################################################################
######## TAB 1 - TOTALS ########################################################
################################################################################
    
    output$TotalsPlot <- renderPlot({
        
        
        #creating temp dataframe to aid in plotting
        dataToPlot = as_tibble(matrix(NA, ncol = dim(x())[2])) %>%
            rename(Date = V1, Violations = V2, Statistics = V3, Value = V4)
        
        #sorting via user input
        
#########beginning with All Violations
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
        
######## Violent Crime
        if (input$violation == 'Violent Criminal Code Violations'){
            
            #total number of incidents
            if (input$statistic == 'Number of Incidents'){
                #populating new dataframe
                for (i in 1:dim(x())[1]){
                    if (x()[i, 2] == 'Total violent Criminal Code violations [100]'){
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
                    if (x()[i, 2] == 'Total violent Criminal Code violations [100]'){
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
                    if (x()[i, 2] == 'Total violent Criminal Code violations [100]'){
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
                    if (x()[i, 2] == 'Total violent Criminal Code violations [100]'){
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
                    if (x()[i, 2] == 'Total violent Criminal Code violations [100]'){
                        if(x()[i, 3] == 'Total, youth not charged'){
                            dataToPlot = dataToPlot %>% add_row(x()[i,])
                        }
                    }
                }
            }
        }
        
######## Property Crime
        if (input$violation == 'Property Crime Violations'){
            
            #total number of incidents
            if (input$statistic == 'Number of Incidents'){
                #populating new dataframe
                for (i in 1:dim(x())[1]){
                    if (x()[i, 2] == 'Total property crime violations [200]'){
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
                    if (x()[i, 2] == 'Total property crime violations [200]'){
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
                    if (x()[i, 2] == 'Total property crime violations [200]'){
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
                    if (x()[i, 2] == 'Total property crime violations [200]'){
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
                    if (x()[i, 2] == 'Total property crime violations [200]'){
                        if(x()[i, 3] == 'Total, youth not charged'){
                            dataToPlot = dataToPlot %>% add_row(x()[i,])
                        }
                    }
                }
            }
        }
        
######## Other Criminal Code Violations
        if (input$violation == 'Other Criminal Code Violations'){
            
            #total number of incidents
            if (input$statistic == 'Number of Incidents'){
                #populating new dataframe
                for (i in 1:dim(x())[1]){
                    if (x()[i, 2] == 'Total other Criminal Code violations [300]'){
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
                    if (x()[i, 2] == 'Total other Criminal Code violations [300]'){
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
                    if (x()[i, 2] == 'Total other Criminal Code violations [300]'){
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
                    if (x()[i, 2] == 'Total other Criminal Code violations [300]'){
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
                    if (x()[i, 2] == 'Total other Criminal Code violations [300]'){
                        if(x()[i, 3] == 'Total, youth not charged'){
                            dataToPlot = dataToPlot %>% add_row(x()[i,])
                        }
                    }
                }
            }
        }
        
######## Impaired Driving
        if (input$violation == 'Impaired Driving'){
            
            #total number of incidents
            if (input$statistic == 'Number of Incidents'){
                #populating new dataframe
                for (i in 1:dim(x())[1]){
                    if (x()[i, 2] == 'Total impaired driving [910]'){
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
                    if (x()[i, 2] == 'Total impaired driving [910]'){
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
                    if (x()[i, 2] == 'Total impaired driving [910]'){
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
                    if (x()[i, 2] == 'Total impaired driving [910]'){
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
                    if (x()[i, 2] == 'Total impaired driving [910]'){
                        if(x()[i, 3] == 'Total, youth not charged'){
                            dataToPlot = dataToPlot %>% add_row(x()[i,])
                        }
                    }
                }
            }
        }
        
######## Other Criminal Code Traffic Violations
        if (input$violation == 'Other Criminal Code Traffic Violations'){
            
            #total number of incidents
            if (input$statistic == 'Number of Incidents'){
                #populating new dataframe
                for (i in 1:dim(x())[1]){
                    if (x()[i, 2] == 'Total other Criminal Code traffic violations [920]'){
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
                    if (x()[i, 2] == 'Total other Criminal Code traffic violations [920]'){
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
                    if (x()[i, 2] == 'Total other Criminal Code traffic violations [920]'){
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
                    if (x()[i, 2] == 'Total other Criminal Code traffic violations [920]'){
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
                    if (x()[i, 2] == 'Total other Criminal Code traffic violations [920]'){
                        if(x()[i, 3] == 'Total, youth not charged'){
                            dataToPlot = dataToPlot %>% add_row(x()[i,])
                        }
                    }
                }
            }
        }
        
######## Drug Violations
        if (input$violation == 'Drug Violations'){
            
            #total number of incidents
            if (input$statistic == 'Number of Incidents'){
                #populating new dataframe
                for (i in 1:dim(x())[1]){
                    if (x()[i, 2] == 'Total drug violations [401]'){
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
                    if (x()[i, 2] == 'Total drug violations [401]'){
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
                    if (x()[i, 2] == 'Total drug violations [401]'){
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
                    if (x()[i, 2] == 'Total drug violations [401]'){
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
                    if (x()[i, 2] == 'Total drug violations [401]'){
                        if(x()[i, 3] == 'Total, youth not charged'){
                            dataToPlot = dataToPlot %>% add_row(x()[i,])
                        }
                    }
                }
            }
        }
        
######## Youth Criminal Justice Act
        if (input$violation == 'Youth Criminal Justice Act'){
            
            #total number of incidents
            if (input$statistic == 'Number of Incidents'){
                #populating new dataframe
                for (i in 1:dim(x())[1]){
                    if (x()[i, 2] == 'Youth Criminal Justice Act [6450]'){
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
                    if (x()[i, 2] == 'Youth Criminal Justice Act [6450]'){
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
                    if (x()[i, 2] == 'Youth Criminal Justice Act [6450]'){
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
                    if (x()[i, 2] == 'Youth Criminal Justice Act [6450]'){
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
                    if (x()[i, 2] == 'Youth Criminal Justice Act [6450]'){
                        if(x()[i, 3] == 'Total, youth not charged'){
                            dataToPlot = dataToPlot %>% add_row(x()[i,])
                        }
                    }
                }
            }
        }
        
######## Other Federal Statutes
        if (input$violation == 'Other Federal Statutes'){
            
            #total number of incidents
            if (input$statistic == 'Number of Incidents'){
                #populating new dataframe
                for (i in 1:dim(x())[1]){
                    if (x()[i, 2] == 'Total other Federal Statutes [610]'){
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
                    if (x()[i, 2] == 'Total other Federal Statutes [610]'){
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
                    if (x()[i, 2] == 'Total other Federal Statutes [610]'){
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
                    if (x()[i, 2] == 'Total other Federal Statutes [610]'){
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
                    if (x()[i, 2] == 'Total other Federal Statutes [610]'){
                        if(x()[i, 3] == 'Total, youth not charged'){
                            dataToPlot = dataToPlot %>% add_row(x()[i,])
                        }
                    }
                }
            }
        }
        
        
        #removing the first row in the data (NA) leftover from initialization
        dataToPlot = dataToPlot %>% remove_missing()               
        
    
######### Plotting
        
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
                colnames(populationMatrix) <- c("Date", "Total Incidents", 
                                                "Total Rate", "Total Population")
                
                
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
    
################################################################################
######## TAB 2 - RATES #########################################################
################################################################################
    
    output$RatePlot <- renderPlot({
        
        
        #creating temp dataframe to aid in plotting
        dataToPlot = as_tibble(matrix(NA, ncol = dim(x())[2])) %>%
            rename(Date = V1, Violations = V2, Statistics = V3, Value = V4)
        
        #sorting via user input
        
#########beginning with All Violations
        if (input$violation == 'All Violations'){
            
            #total number of incidents
            if (input$statistic == 'Number of Incidents'){
                #populating new dataframe
                for (i in 1:dim(x())[1]){
                    if (x()[i, 2] == 'Total, all violations [0]'){
                        if(x()[i, 3] == 'Rate per 100,000 population'){
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
                        if(x()[i, 3] == 'Rate, adult charged per 100,000 population aged 18 years and over'){
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
                        if(x()[i, 3] == 'Rate, youth charged per 100,000 population aged 12 to 17 years'){
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
                        if(x()[i, 3] == 'Rate, youth not charged per 100,000 population aged 12 to 17 years'){
                            dataToPlot = dataToPlot %>% add_row(x()[i,])
                        }
                    }
                }
            }
        }
        
######## Violent Crime
        if (input$violation == 'Violent Criminal Code Violations'){
            
            #total number of incidents
            if (input$statistic == 'Number of Incidents'){
                #populating new dataframe
                for (i in 1:dim(x())[1]){
                    if (x()[i, 2] == 'Total violent Criminal Code violations [100]'){
                        if(x()[i, 3] == 'Rate per 100,000 population'){
                            dataToPlot = dataToPlot %>% add_row(x()[i,])
                        }
                    }
                }
            }
            #total number of persons cleared
            else if (input$statistic == 'Persons Cleared'){
                #populating temp dataframe
                for (i in 1:dim(x())[1]){
                    if (x()[i, 2] == 'Total violent Criminal Code violations [100]'){
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
                    if (x()[i, 2] == 'Total violent Criminal Code violations [100]'){
                        if(x()[i, 3] == 'Rate, adult charged per 100,000 population aged 18 years and over'){
                            dataToPlot = dataToPlot %>% add_row(x()[i,])
                        }
                    }
                }
            }
            #total youths charged
            else if (input$statistic == 'Youths (12-17 yrs) Charged'){
                #populating temp dataframe
                for (i in 1:dim(x())[1]){
                    if (x()[i, 2] == 'Total violent Criminal Code violations [100]'){
                        if(x()[i, 3] == 'Rate, youth charged per 100,000 population aged 12 to 17 years'){
                            dataToPlot = dataToPlot %>% add_row(x()[i,])
                        }
                    }
                }
            }
            #total youths not charged
            else if (input$statistic == 'Youths (12-17 yrs) Not Charged'){
                #populating temp dataframe
                for (i in 1:dim(x())[1]){
                    if (x()[i, 2] == 'Total violent Criminal Code violations [100]'){
                        if(x()[i, 3] == 'Rate, youth not charged per 100,000 population aged 12 to 17 years'){
                            dataToPlot = dataToPlot %>% add_row(x()[i,])
                        }
                    }
                }
            }
        }
        
######## Property Crime
        if (input$violation == 'Property Crime Violations'){
            
            #total number of incidents
            if (input$statistic == 'Number of Incidents'){
                #populating new dataframe
                for (i in 1:dim(x())[1]){
                    if (x()[i, 2] == 'Total property crime violations [200]'){
                        if(x()[i, 3] == 'Rate per 100,000 population'){
                            dataToPlot = dataToPlot %>% add_row(x()[i,])
                        }
                    }
                }
            }
            #total number of persons cleared
            else if (input$statistic == 'Persons Cleared'){
                #populating temp dataframe
                for (i in 1:dim(x())[1]){
                    if (x()[i, 2] == 'Total property crime violations [200]'){
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
                    if (x()[i, 2] == 'Total property crime violations [200]'){
                        if(x()[i, 3] == 'Rate, adult charged per 100,000 population aged 18 years and over'){
                            dataToPlot = dataToPlot %>% add_row(x()[i,])
                        }
                    }
                }
            }
            #total youths charged
            else if (input$statistic == 'Youths (12-17 yrs) Charged'){
                #populating temp dataframe
                for (i in 1:dim(x())[1]){
                    if (x()[i, 2] == 'Total property crime violations [200]'){
                        if(x()[i, 3] == 'Rate, youth charged per 100,000 population aged 12 to 17 years'){
                            dataToPlot = dataToPlot %>% add_row(x()[i,])
                        }
                    }
                }
            }
            #total youths not charged
            else if (input$statistic == 'Youths (12-17 yrs) Not Charged'){
                #populating temp dataframe
                for (i in 1:dim(x())[1]){
                    if (x()[i, 2] == 'Total property crime violations [200]'){
                        if(x()[i, 3] == 'Rate, youth not charged per 100,000 population aged 12 to 17 years'){
                            dataToPlot = dataToPlot %>% add_row(x()[i,])
                        }
                    }
                }
            }
        }
        
######## Other Criminal Code Violations
        if (input$violation == 'Other Criminal Code Violations'){
            
            #total number of incidents
            if (input$statistic == 'Number of Incidents'){
                #populating new dataframe
                for (i in 1:dim(x())[1]){
                    if (x()[i, 2] == 'Total other Criminal Code violations [300]'){
                        if(x()[i, 3] == 'Rate per 100,000 population'){
                            dataToPlot = dataToPlot %>% add_row(x()[i,])
                        }
                    }
                }
            }
            #total number of persons cleared
            else if (input$statistic == 'Persons Cleared'){
                #populating temp dataframe
                for (i in 1:dim(x())[1]){
                    if (x()[i, 2] == 'Total other Criminal Code violations [300]'){
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
                    if (x()[i, 2] == 'Total other Criminal Code violations [300]'){
                        if(x()[i, 3] == 'Rate, adult charged per 100,000 population aged 18 years and over'){
                            dataToPlot = dataToPlot %>% add_row(x()[i,])
                        }
                    }
                }
            }
            #total youths charged
            else if (input$statistic == 'Youths (12-17 yrs) Charged'){
                #populating temp dataframe
                for (i in 1:dim(x())[1]){
                    if (x()[i, 2] == 'Total other Criminal Code violations [300]'){
                        if(x()[i, 3] == 'Rate, youth charged per 100,000 population aged 12 to 17 years'){
                            dataToPlot = dataToPlot %>% add_row(x()[i,])
                        }
                    }
                }
            }
            #total youths not charged
            else if (input$statistic == 'Youths (12-17 yrs) Not Charged'){
                #populating temp dataframe
                for (i in 1:dim(x())[1]){
                    if (x()[i, 2] == 'Total other Criminal Code violations [300]'){
                        if(x()[i, 3] == 'Rate, youth not charged per 100,000 population aged 12 to 17 years'){
                            dataToPlot = dataToPlot %>% add_row(x()[i,])
                        }
                    }
                }
            }
        }
        
######## Impaired Driving
        if (input$violation == 'Impaired Driving'){
            
            #total number of incidents
            if (input$statistic == 'Number of Incidents'){
                #populating new dataframe
                for (i in 1:dim(x())[1]){
                    if (x()[i, 2] == 'Total impaired driving [910]'){
                        if(x()[i, 3] == 'Rate per 100,000 population'){
                            dataToPlot = dataToPlot %>% add_row(x()[i,])
                        }
                    }
                }
            }
            #total number of persons cleared
            else if (input$statistic == 'Persons Cleared'){
                #populating temp dataframe
                for (i in 1:dim(x())[1]){
                    if (x()[i, 2] == 'Total impaired driving [910]'){
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
                    if (x()[i, 2] == 'Total impaired driving [910]'){
                        if(x()[i, 3] == 'Rate, adult charged per 100,000 population aged 18 years and over'){
                            dataToPlot = dataToPlot %>% add_row(x()[i,])
                        }
                    }
                }
            }
            #total youths charged
            else if (input$statistic == 'Youths (12-17 yrs) Charged'){
                #populating temp dataframe
                for (i in 1:dim(x())[1]){
                    if (x()[i, 2] == 'Total impaired driving [910]'){
                        if(x()[i, 3] == 'Rate, youth charged per 100,000 population aged 12 to 17 years'){
                            dataToPlot = dataToPlot %>% add_row(x()[i,])
                        }
                    }
                }
            }
            #total youths not charged
            else if (input$statistic == 'Youths (12-17 yrs) Not Charged'){
                #populating temp dataframe
                for (i in 1:dim(x())[1]){
                    if (x()[i, 2] == 'Total impaired driving [910]'){
                        if(x()[i, 3] == 'Rate, youth not charged per 100,000 population aged 12 to 17 years'){
                            dataToPlot = dataToPlot %>% add_row(x()[i,])
                        }
                    }
                }
            }
        }
        
######## Other Criminal Code Traffic Violations
        if (input$violation == 'Other Criminal Code Traffic Violations'){
            
            #total number of incidents
            if (input$statistic == 'Number of Incidents'){
                #populating new dataframe
                for (i in 1:dim(x())[1]){
                    if (x()[i, 2] == 'Total other Criminal Code traffic violations [920]'){
                        if(x()[i, 3] == 'Rate per 100,000 population'){
                            dataToPlot = dataToPlot %>% add_row(x()[i,])
                        }
                    }
                }
            }
            #total number of persons cleared
            else if (input$statistic == 'Persons Cleared'){
                #populating temp dataframe
                for (i in 1:dim(x())[1]){
                    if (x()[i, 2] == 'Total other Criminal Code traffic violations [920]'){
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
                    if (x()[i, 2] == 'Total other Criminal Code traffic violations [920]'){
                        if(x()[i, 3] == 'Rate, adult charged per 100,000 population aged 18 years and over'){
                            dataToPlot = dataToPlot %>% add_row(x()[i,])
                        }
                    }
                }
            }
            #total youths charged
            else if (input$statistic == 'Youths (12-17 yrs) Charged'){
                #populating temp dataframe
                for (i in 1:dim(x())[1]){
                    if (x()[i, 2] == 'Total other Criminal Code traffic violations [920]'){
                        if(x()[i, 3] == 'Rate, youth charged per 100,000 population aged 12 to 17 years'){
                            dataToPlot = dataToPlot %>% add_row(x()[i,])
                        }
                    }
                }
            }
            #total youths not charged
            else if (input$statistic == 'Youths (12-17 yrs) Not Charged'){
                #populating temp dataframe
                for (i in 1:dim(x())[1]){
                    if (x()[i, 2] == 'Total other Criminal Code traffic violations [920]'){
                        if(x()[i, 3] == 'Rate, youth not charged per 100,000 population aged 12 to 17 years'){
                            dataToPlot = dataToPlot %>% add_row(x()[i,])
                        }
                    }
                }
            }
        }
        
######## Drug Violations
        if (input$violation == 'Drug Violations'){
            
            #total number of incidents
            if (input$statistic == 'Number of Incidents'){
                #populating new dataframe
                for (i in 1:dim(x())[1]){
                    if (x()[i, 2] == 'Total drug violations [401]'){
                        if(x()[i, 3] == 'Rate per 100,000 population'){
                            dataToPlot = dataToPlot %>% add_row(x()[i,])
                        }
                    }
                }
            }
            #total number of persons cleared
            else if (input$statistic == 'Persons Cleared'){
                #populating temp dataframe
                for (i in 1:dim(x())[1]){
                    if (x()[i, 2] == 'Total drug violations [401]'){
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
                    if (x()[i, 2] == 'Total drug violations [401]'){
                        if(x()[i, 3] == 'Rate, adult charged per 100,000 population aged 18 years and over'){
                            dataToPlot = dataToPlot %>% add_row(x()[i,])
                        }
                    }
                }
            }
            #total youths charged
            else if (input$statistic == 'Youths (12-17 yrs) Charged'){
                #populating temp dataframe
                for (i in 1:dim(x())[1]){
                    if (x()[i, 2] == 'Total drug violations [401]'){
                        if(x()[i, 3] == 'Rate, youth charged per 100,000 population aged 12 to 17 years'){
                            dataToPlot = dataToPlot %>% add_row(x()[i,])
                        }
                    }
                }
            }
            #total youths not charged
            else if (input$statistic == 'Youths (12-17 yrs) Not Charged'){
                #populating temp dataframe
                for (i in 1:dim(x())[1]){
                    if (x()[i, 2] == 'Total drug violations [401]'){
                        if(x()[i, 3] == 'Rate, youth not charged per 100,000 population aged 12 to 17 years'){
                            dataToPlot = dataToPlot %>% add_row(x()[i,])
                        }
                    }
                }
            }
        }
        
######## Youth Criminal Justice Act
        if (input$violation == 'Youth Criminal Justice Act'){
            
            #total number of incidents
            if (input$statistic == 'Number of Incidents'){
                #populating new dataframe
                for (i in 1:dim(x())[1]){
                    if (x()[i, 2] == 'Youth Criminal Justice Act [6450]'){
                        if(x()[i, 3] == 'Rate per 100,000 population'){
                            dataToPlot = dataToPlot %>% add_row(x()[i,])
                        }
                    }
                }
            }
            #total number of persons cleared
            else if (input$statistic == 'Persons Cleared'){
                #populating temp dataframe
                for (i in 1:dim(x())[1]){
                    if (x()[i, 2] == 'Youth Criminal Justice Act [6450]'){
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
                    if (x()[i, 2] == 'Youth Criminal Justice Act [6450]'){
                        if(x()[i, 3] == 'Rate, adult charged per 100,000 population aged 18 years and over'){
                            dataToPlot = dataToPlot %>% add_row(x()[i,])
                        }
                    }
                }
            }
            #total youths charged
            else if (input$statistic == 'Youths (12-17 yrs) Charged'){
                #populating temp dataframe
                for (i in 1:dim(x())[1]){
                    if (x()[i, 2] == 'Youth Criminal Justice Act [6450]'){
                        if(x()[i, 3] == 'Rate, youth charged per 100,000 population aged 12 to 17 years'){
                            dataToPlot = dataToPlot %>% add_row(x()[i,])
                        }
                    }
                }
            }
            #total youths not charged
            else if (input$statistic == 'Youths (12-17 yrs) Not Charged'){
                #populating temp dataframe
                for (i in 1:dim(x())[1]){
                    if (x()[i, 2] == 'Youth Criminal Justice Act [6450]'){
                        if(x()[i, 3] == 'Rate, youth not charged per 100,000 population aged 12 to 17 years'){
                            dataToPlot = dataToPlot %>% add_row(x()[i,])
                        }
                    }
                }
            }
        }
        
######## Other Federal Statutes
        if (input$violation == 'Other Federal Statutes'){
            
            #total number of incidents
            if (input$statistic == 'Number of Incidents'){
                #populating new dataframe
                for (i in 1:dim(x())[1]){
                    if (x()[i, 2] == 'Total other Federal Statutes [610]'){
                        if(x()[i, 3] == 'Rate per 100,000 population'){
                            dataToPlot = dataToPlot %>% add_row(x()[i,])
                        }
                    }
                }
            }
            #total number of persons cleared
            else if (input$statistic == 'Persons Cleared'){
                #populating temp dataframe
                for (i in 1:dim(x())[1]){
                    if (x()[i, 2] == 'Total other Federal Statutes [610]'){
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
                    if (x()[i, 2] == 'Total other Federal Statutes [610]'){
                        if(x()[i, 3] == 'Rate, adult charged per 100,000 population aged 18 years and over'){
                            dataToPlot = dataToPlot %>% add_row(x()[i,])
                        }
                    }
                }
            }
            #total youths charged
            else if (input$statistic == 'Youths (12-17 yrs) Charged'){
                #populating temp dataframe
                for (i in 1:dim(x())[1]){
                    if (x()[i, 2] == 'Total other Federal Statutes [610]'){
                        if(x()[i, 3] == 'Rate, youth charged per 100,000 population aged 12 to 17 years'){
                            dataToPlot = dataToPlot %>% add_row(x()[i,])
                        }
                    }
                }
            }
            #total youths not charged
            else if (input$statistic == 'Youths (12-17 yrs) Not Charged'){
                #populating temp dataframe
                for (i in 1:dim(x())[1]){
                    if (x()[i, 2] == 'Total other Federal Statutes [610]'){
                        if(x()[i, 3] == 'Rate, youth not charged per 100,000 population aged 12 to 17 years'){
                            dataToPlot = dataToPlot %>% add_row(x()[i,])
                        }
                    }
                }
            }
        }        
        
        #removing first row from the data leftover from initialization (all NAs)
        dataToPlot = dataToPlot %>% remove_missing()               
        
#########        
######### Plotting
#########        
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
