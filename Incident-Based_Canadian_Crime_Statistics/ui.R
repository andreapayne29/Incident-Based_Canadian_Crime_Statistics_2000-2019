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
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("violation",  "Violation", c("All Violations", "Violent Criminal Code Violations", "Property Crime Violations", "Other Criminal Code Violations", "Impaired Driving", "Other Criminal Code Traffic Violations", "Drug Violations", "Youth Criminal Justice Act", "Other Federal Statutes")),
            radioButtons("typeOfStat", "Statistic Type", c("Totals", "Rate per 100,000 population")),
            conditionalPanel(
                condition = "input.typeOfStat == 'Totals'",   
                selectInput("statistic", "Statistic", c("Total Incidents", "Total Cleared", "Total Persons Charged", "Total Adults Charged", "Total Youths Charged", "Total Youths Not Charged"))
                ),
            conditionalPanel(
                condition = "input.typeOfStat == 'Rate per 100,000 population'",   
                selectInput("statistic", "Statistic", c("Incidents/100,000 population", "Persons Charged/100,000 population, 12+ years old", "Persons Charged/100,000 population, 18+ years old", "Youth Charged/100,000 population, 12-17 years old", "Youth Not Charged/100,000 population, 12-17 years old"))
            ),
            radioButtons("plotType", "Type of Plot", c("Scatterplot", "Bar Plot")),
        
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            #
            #
            # Now I want different Tabs
            # Each tab will give me different options#
            # In this case I will have 2 tabs
            # Those 2 tabs will use different variables
            # But they could do totally different things
            #
            #
            # The About tab is where you should include your name
            # and some other info about yourself.
            # It's probably easiest if you make incremental updates to the teamplate I provide
            #
            #
            navlistPanel(
                tabPanel("Population Data", 
                         
                         HTML("<h1> Your name goes here</h1>
        <p >Say something sensible about the project you are doing.  Explain the project in a paragraph.   To use this as part of a job portfolio, include a picture of yourself.  I already have a job so I chose a picture of me working at an ice desk that I made when I visited University of Manitoba.  For job hunting purposes, you should use a picture that clearly identifies you.</p>
		<p>Make a new paragraph to say something about yourself. Use html code to make links to things like your LinkedIn, Twitter, Github, etc page.  Keep your code very basic to avoid breaking things.  Feel free to modify this template. Note that linking to social media isn't important for class, but it is very useful for building a portfolio of projects and skills. Although this paragraph can be very basic for our assignment, adding those elements in now will save you time when you are job hunting for a career or co-op job. </p>"
                         )),
                tabPanel("Private Dwellings"),
                tabPanel("About") #This is the new tab with some info
            )
        )
    )
))
