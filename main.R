
#
# This module loads the libraries necessary for the project and calls the runApp() 
#   function to start the app in the /app directory. This function automatically 
#   invokes app.R. 
#


# Load libraries

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

# Set/confirm working directory. This should be the same directory this file is in.

setwd("C:/Users/mayah/OneDrive/Desktop/shiny")

# Invoke the shiny command to start the app.

runApp("app")

