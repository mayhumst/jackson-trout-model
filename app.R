

#
# This module builds and runs the shiny application.
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
#library(shinydashboard)
#library(shinydashboardPlus)

## Import helper functions from other files
source("./helpers/get_data.R")
source("./helpers/graph.R")
source("./server.R")
source("./ui.R")

## Set variables for start and end years
StYear <- 2024
CurrDate <- Sys.Date() - 1
EndYear <- as.integer(format(CurrDate, "%Y"))
EndDay <- toString(format(CurrDate, "-%m-%d"))

## Download/process data
All_Temps <- get_BT_data(StYear, EndYear)
Graph_Crit_Dates <- model_crit_dates(All_Temps, StYear, EndYear)

## Run the Shiny app
shinyApp(ui = ui, server = server)

