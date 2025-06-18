## UI sidebar
sidebar <- sidebar(
  
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
      choices = list("2007-2008" = 2008,
                     "2008-2009" = 2009,
                     "2009-2010" = 2010,
                     "2010-2011" = 2011,
                     "2011-2012" = 2012,
                     "2012-2013" = 2013,
                     "2013-2014" = 2014,
                     "2014-2015" = 2015,
                     "2015-2016" = 2016,
                     "2016-2017" = 2017,
                     "2017-2018" = 2018,
                     "2018-2019" = 2019,
                     "2019-2020" = 2020,
                     "2020-2021" = 2021,
                     "2021-2022" = 2022,
                     "2022-2023" = 2023,
                     "2023-2024" = 2024
      ),
      selected = 2009
    )
  ), 
  
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
  )
)


## UI main body
body <- navset_pill(id = "tabs", 
    nav_panel("Old Years", 
      card(
        htmlOutput(outputId = "OldYearsPlotTitle"),
        plotOutput(outputId = "OldYearsPlot")
      ), 
      card(
        tableOutput(outputId = "OldYearDateTable")
      )
    ),
    nav_panel("This Year", 
      card(
        plotOutput(outputId = "ThisYearPlot")
      )
    ), 
    nav_panel("Key Dates",
      card(
        tableOutput(outputId = "AllKeyDatesTable")
      )
    )
  
)



# Define user interface
ui <- page_sidebar(
  
  # App header
  title = "My Title",
  
  # Sidebar panel for inputs
  sidebar = sidebar,
  
  # Main body
  body
)