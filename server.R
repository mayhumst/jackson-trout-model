#
# This module defines the shiny server function that will be called to run the app.
#
## Load libraries
library(shiny)
library(bslib)
library(dataRetrieval)
library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(zoo)
library(forecast)
library(readr)

## Import helper functions from other files
source("./helpers/graph.R")
source("./helpers/load_data.R")
source("./helpers/model.R")
source("./helpers/server_functions.R")

### Define app server 
server <- function(input, output) {
  
  ## Check for data updates
  load_data()
  
  ## Get current year
  curr_water_year <- calculate_water_year(as.Date(Sys.time(), tz = "UTC"))
  
  ## Render plot for old years
  output$PastYearsPlot <- renderPlot({
    
    ## Get variables from inputs
    Species <- input$Species_OldYears
    SpringYear <- as.integer(input$Year)
    
    ## Display plot - old years
    show_graph(Species, 
               get_year_data(SpringYear),
               get_year_dates(SpringYear, Species))
  })
  
  ## Render plot title for old years
  output$PastYearsPlotTitle <- renderText({
    
    ## Get variables from inputs
    Species <- input$Species_OldYears
    SpringYear <- as.integer(input$Year)
    
    ## Get year range to add to the plot title
    YearRange <- paste(toString(SpringYear-1), "-", toString(SpringYear), sep="")
    
    ## Concatenate the full title
    paste("<h4>", YearRange, Species, "Trout Spawn, Hatch, and Emergence</h4>")
    
  })
  
  ## Render key dates for single (old) year
  output$PastYearDateTable <- renderTable({
    
    ## Get variables from inputs
    Species <- input$Species_OldYears
    SpringYear <- input$Year
    
    ## Get the single row of critical dates
    format_single_year_data_table(Species, SpringYear)
  })
  
  ## Render plot for the current year
  output$CurrentYearPlot <- renderPlot({
    
    ## Get variables from inputs
    Species <- input$Species_ThisYear
    SpringYear <- curr_water_year
    
    ## Display plot - old years
    show_graph(Species, 
               get_year_data(SpringYear),
               get_year_dates(SpringYear, Species))
  })
  
  ## Render plot title for current year
  output$CurrentYearPlotTitle <- renderText({
    
    ## Get variables from inputs
    Species <- input$Species_ThisYear
    SpringYear <- curr_water_year
    
    ## Get year range to add to the plot title
    YearRange <- paste(toString(SpringYear-1), "-", toString(SpringYear), sep="")
    
    ## Concatenate the full title
    paste("<h4>", YearRange, Species, "Trout Spawn, Hatch, and Emergence</h4>")
    
  })
  
  ## Render plot summary for current year
  output$CurrentYearSummary <- renderText({
    
    ## Get variables from inputs
    Species <- input$Species_ThisYear
    SpringYear <- curr_water_year
    
    ## Get graph summary
    show_summary(TRUE, Species, 
               get_year_data(SpringYear),
               get_year_dates(SpringYear, Species))
    
  })
  
  ## Render key dates for this year only
  output$CurrentYearDateTable <- renderTable({
    
    ## Get variables from inputs
    Species <- input$Species_ThisYear
    SpringYear <- curr_water_year
    
    ## Get the single row of critical dates
    format_single_year_data_table(Species, SpringYear)
  })
  
  ## Render table for all key dates tab
  output$AllKeyDatesTable <- renderTable({
    
    ## Get inputs
    Species <- input$Species_KeyDates
    
    ## Get table
    format_all_years_data_table(Species)
    
  })
}
