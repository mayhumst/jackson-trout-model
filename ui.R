#
# This module defines the shiny UI that will be called to run the app.
#

## Determine current year so our selectInput is up-to-date
get_year_range <- function() {
  curr_water_year <- calculate_water_year(as.Date(Sys.time(), tz = "UTC"))
  year_range <- seq(from = curr_water_year - 1, to = 2008)
  year_list <- as.list(year_range)
  for(i in 1:length(year_range)) {
    year_range[i] <- paste(
      toString(as.integer(year_range[i]) - 1), 
      toString(as.integer(year_range[i])), 
      sep = "-" 
    )
  }
  names(year_list) <- year_range
  return(year_list)
}

year_list <- get_year_range()


## UI sidebar component
sidebar <- sidebar(
  
  ## Sidebar input when on "This Year" tab
  conditionalPanel(condition = "input.tabs == 'This Year'", 
    # Input: Select species
    selectInput(
      "Species_ThisYear",
      "Select species:",
      choices = list("Brown Trout" = "Brown",
                    "Rainbow Trout" = "Rainbow"
      )
    ),
  ), 
  
  
  ## Sidebar input when on "Old Years" tab
  conditionalPanel(condition = "input.tabs == 'Old Years'", 
                   
    # Input: Select species
    selectInput(
      "Species_OldYears",
      "Select species:",
      choices = list("Brown Trout" = "Brown",
                  "Rainbow Trout" = "Rainbow"
      )
    ),
                     
    # Input: Select year
    selectInput(
      "Year",
      "Select year:",
      choices = year_list
    )
  ), 
  
  
  ## Sidebar input when on "Key Dates" tab
  conditionalPanel(condition = "input.tabs == 'Key Dates'",
    # Input: Select species
    selectInput(
      "Species_KeyDates",
      "Select species:",
      choices = list("Brown Trout" = "Brown",
                     "Rainbow Trout" = "Rainbow"
      )
    )
  )
)


## UI main body
body <- navset_pill(id = "tabs", 
    nav_panel("This Year", 
      card(
        htmlOutput(outputId = "ThisYearPlotTitle"),
        plotOutput(outputId = "ThisYearPlot")
      ),
      card(
        tableOutput(outputId = "ThisYearDateTable")
      )
    ), 
    nav_panel("Old Years", 
      card(
        htmlOutput(outputId = "OldYearsPlotTitle"),
        plotOutput(outputId = "OldYearsPlot")
      ), 
      card(
        tableOutput(outputId = "OldYearDateTable")
      )
    ),
    nav_panel("Key Dates",
      card(
        tableOutput(outputId = "AllKeyDatesTable")
      )
    )
  
)



## Define user interface
ui <- page_sidebar(
  
  # App header
  title = "Jackson River Trout Spawning and Emergence Predictor",
  
  # Sidebar panel for inputs
  sidebar = sidebar,
  
  # Main body
  body
)