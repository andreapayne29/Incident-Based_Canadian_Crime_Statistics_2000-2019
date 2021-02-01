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
                   "2010-2019" = "DataFile2010")),
    
    # Sidebar allowing for user input for what data and what plot
    sidebarLayout(
        sidebarPanel(
            #dropdown bar for type of violation
            selectInput("violation",  "Violation", c("All Violations", "Violent Criminal Code Violations", "Property Crime Violations", "Other Criminal Code Violations", "Impaired Driving", "Other Criminal Code Traffic Violations", "Drug Violations", "Youth Criminal Justice Act", "Other Federal Statutes")),
           
            #dropdown bar for what type of statistic
            selectInput("statistic", "Statistic", c("Number of Incidents", "Persons Cleared", "Adults (18+ yrs) Charged", "Youths (12-17 yrs) Charged", "Youths (12-17 yrs) Not Charged")),
            
            #plot type radio buttons
            radioButtons("plotType", "Type of Plot", c("Scatterplot", "Bar Plot")),
            #conditional analyses for plot type
            conditionalPanel(
                condition = "input.plotType == 'Scatterplot'",   
                numericInput("xmin", "x-axis minimum:", 0),
                numericInput("xmax", "x-axis maximum value:", 1000000),
                checkboxInput("scatplotType", "Linear Regression", FALSE)
            ),
            conditionalPanel(
                condition = "input.plotType == 'Bar Plot'",   
                checkboxInput("barPlotType", "Density", FALSE),
                sliderInput("bins",
                            "Number of bins:",
                            min = 1,
                            max = 50,
                            value = 30)
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
                tabPanel("About", 
                         HTML("<h1> Andrea Payne </h1>
        <p >Say something sensible about the project you are doing.  Explain the project in a paragraph.   To use this as part of a job portfolio, include a picture of yourself.  I already have a job so I chose a picture of me working at an ice desk that I made when I visited University of Manitoba.  For job hunting purposes, you should use a picture that clearly identifies you.</p>
		<p>Make a new paragraph to say something about yourself. Use html code to make links to things like your LinkedIn, Twitter, Github, etc page.  Keep your code very basic to avoid breaking things.  Feel free to modify this template. Note that linking to social media isn't important for class, but it is very useful for building a portfolio of projects and skills. Although this paragraph can be very basic for our assignment, adding those elements in now will save you time when you are job hunting for a career or co-op job. </p>"
                         )) #This is the new tab with some info
            )
        )
    )
))
