#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Incident-Based Canadian Crime Statistics, from 2000 to 2019"),
    
    #adding radio buttons for decade choice
    radioButtons("dataSource", "Data",
                 c("2000-2009" = "DataFile2000",
                   "2010-2019" = "DataFile2010",
                   "All Years (2000-2019)" = "DataFileTotal")),
    
    # Sidebar allowing for user input for what data and what plot
    sidebarLayout(
        sidebarPanel(
            #dropdown bar for type of violation
            selectInput("violation",  "Violation", c("All Violations", 
                                                     "Violent Criminal Code Violations", 
                                                     "Property Crime Violations", 
                                                     "Other Criminal Code Violations", 
                                                     "Impaired Driving", 
                                                     "Other Criminal Code Traffic Violations", 
                                                     "Drug Violations", 
                                                     "Youth Criminal Justice Act", 
                                                     "Other Federal Statutes")),
           
            #dropdown bar for what type of statistic
            selectInput("statistic", "Statistic", c("Number of Incidents", "Persons Cleared", "Adults (18+ yrs) Charged", "Youths (12-17 yrs) Charged", "Youths (12-17 yrs) Not Charged")),
            
            #plot type radio buttons
            radioButtons("plotType", "Type of Plot", c("Scatterplot", "Bar Plot")),
            #conditional analyses for plot type
            conditionalPanel(
                condition = "input.plotType == 'Scatterplot'",   
                numericInput("ymin", "y-axis minimum:", 0),
                numericInput("ymax", "y-axis maximum value:", 2800000),
                checkboxInput("scatplotType", "Linear Regression [Covariate: Population]", FALSE)
            ),
            conditionalPanel(
                condition = "input.plotType == 'Bar Plot'",   
                numericInput("xmin", "x-axis minimum:", 0),
                numericInput("xmax", "x-axis maximum value:", 4000000)
            ),
        
            
        ),
        
        # where the plots will be displayed
        mainPanel(
            #building multiple tabs
            navlistPanel(
                #totals tab
                tabPanel("Totals", plotOutput("TotalsPlot")),
                
                #rate per 100,000 tab
                tabPanel("Rate per 100,000 Population", plotOutput("RatePlot")),
                
                #about me tab
                tabPanel("About",source("about.R")$value())
            )
        )
    )
))
