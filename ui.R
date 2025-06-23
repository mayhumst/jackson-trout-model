#
# This module defines the shiny UI that will be called to run the app.
#

## Determine current year so our selectInput is up-to-date
curr_water_year <- calculate_water_year(as.Date(Sys.time(), tz = "UTC"))
year_range <- seq(from = 2008, to = curr_water_year - 1)
year_list <- as.list(year_range)
for(i in 1:length(year_range)) {
  year_range[i] <- paste(
    toString(as.integer(year_range[i]) - 1), 
    toString(as.integer(year_range[i])), 
    sep = "-" 
  )
}
names(year_list) <- year_range


## UI sidebar component
sidebar <- sidebar(
  
  ## Sidebar input when on "This Year" tab
  conditionalPanel(condition = "input.tabs == 'This Year'", 
    # Input: Select species
    selectInput(
      "Species",
      "Select species:",
      choices = list("Brown Trout" = "Brown",
                    "Rainbow Trout" = "Rainbow"
      ),
      selected = "Brown"
    ),
  ), 
  
  
  ## Sidebar input when on "Old Years" tab
  conditionalPanel(condition = "input.tabs == 'Old Years'", 
                   
    # Input: Select species
    selectInput(
      "Species",
      "Select species:",
      choices = list("Brown Trout" = "Brown",
                  "Rainbow Trout" = "Rainbow"
      ),
      selected = "Brown"
    ),
                     
    # Input: Select year
    selectInput(
      "Year",
      "Select year:",
      choices = year_list,
      selected = 2009
    )
  ), 
  
  
  ## Sidebar input when on "Key Dates" tab
  conditionalPanel(condition = "input.tabs == 'Key Dates'",
    # Input: Select species
    selectInput(
      "Species",
      "Select species:",
      choices = list("Brown Trout" = "Brown",
                     "Rainbow Trout" = "Rainbow"
      ),
      selected = "Brown"
    )
  )
)


## UI main body
body <- navset_pill(id = "tabs", 
    nav_panel("This Year", 
      card(
        plotOutput(outputId = "ThisYearPlot")
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
  title = "My Title",
  
  # Sidebar panel for inputs
  sidebar = sidebar,
  
  # Main body
  body
)