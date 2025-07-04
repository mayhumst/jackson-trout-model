

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

## Import helper functions from other files
source("./helpers/load_data.R")
source("./server.R")
source("./ui.R")

## Download/process/update data
load_data()

## Run the Shiny app
shinyApp(ui = ui, server = server)

