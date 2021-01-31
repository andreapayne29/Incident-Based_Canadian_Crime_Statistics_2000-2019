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
    titlePanel("Incident-Based Canadian Crime Statistics From 2000 to 2019"),
    
    #adding radio buttons for decade choice
    radioButtons("dataSource", "Data",
                 c("2000-2009" = "DataFile2000",
                   "2010-2019" = "DataFile2010")),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("violation",  "Violation", c("All Violations", "Violent Criminal Code Violations", "Property Crime Violations", "Other Criminal Code Violations", "Impaired Driving", "Other Criminal Code Traffic Violations", "Drug Violations", "Youth Criminal Justice Act", "Other Federal Statutes")),
            radioButtons("typeOfStat", "Statistic Type", c("Totals", "Rate per 100,000 population", "Percentage")),
            conditionalPanel(
                condition = "input.typeOfStat == 'Totals'",   
                selectInput("statistic", "Statistic", c("Total Incidents", "Total Cleared", "Total Persons Charged", "Total Adults Charged", "Total Youths Charged", "Total Youths Not Charged"))
                ),
            conditionalPanel(
                condition = "input.typeOfStat == 'Rate per 100,000 population'",   
                selectInput("statistic", "Statistic", c("Incidents/100,000 population", "Persons Charged/100,000 population, 12+ years old", "Persons Charged/100,000 population, 18+ years old", "Youth Charged/100,000 population, 12-17 years old", "Youth Not Charged/100,000 population, 12-17 years old"))
            ),
            conditionalPanel(
                condition = "input.typeOfStat == 'Percentage'", 
                selectInput("statistic", "Statistic", c("Percentage Change in Rate"))
            ),
            
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
))
