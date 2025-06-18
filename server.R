#
# This module defines the server function that will be called to run the app.
#



## Define server function
server <- function(input, output) {
  
  ## Render plot for old years
  output$OldYearsPlot <- renderPlot({
    
    ## Get variables from inputs
    Species <- input$Species
    SpringYear <- as.integer(input$Year)
    
    ## Calculate graph inputs
    FallYear <- SpringYear-1
    StartDate <- paste(toString(FallYear), "-10-1", sep="")
    EndDate <- paste(toString(SpringYear), "-7-1", sep="")
    
    ## Display plot - old years
    show_graph(All_Temps,
               Graph_Crit_Dates,
               as.Date(StartDate),
               as.Date(EndDate),
               FallYear,
               SpringYear)
  })
  
  ## Render plot title for old years
  output$OldYearsPlotTitle <- renderText({
    
    ## Get year range to add to the plot title
    SpringYear <- as.integer(input$Year)
    YearRange <- paste(toString(SpringYear-1), "-", toString(SpringYear), sep="")
    
    ## Concatenate the full title
    paste("<h4>", YearRange, "Brown Trout Spawn, Hatch, and Emergence</h4>")
    
  })
  
  ## Render key dates for single (old) year
  output$OldYearDateTable <- renderTable({
    
    ## Get the single row of critical dates
    Single_Year <- Graph_Crit_Dates %>%
      filter(Year == input$Year)
    
    ## Drop the "Year" column
    Single_Year <- Single_Year[c("SpawnStart", "SpawnPeak", "SpawnEnd", 
                                 "HaStart", "HaPeak", "EmStart", "EmPeak")]
    
    ## Confirm dates are formatted correctly. Can change formatting to something
    ##  else if desired, e.g. '%Y-%m-%d', but must specify a formatting.
    Single_Year$SpawnStart <- format(Single_Year$SpawnStart, "%B %d %Y")
    Single_Year$SpawnPeak <- format(Single_Year$SpawnPeak, "%B %d %Y")
    Single_Year$SpawnEnd <- format(Single_Year$SpawnEnd, "%B %d %Y")
    Single_Year$HaStart <- format(Single_Year$HaStart, "%B %d %Y")
    Single_Year$HaPeak <- format(Single_Year$HaPeak, "%B %d %Y")
    Single_Year$EmStart <- format(Single_Year$EmStart, "%B %d %Y")
    Single_Year$EmPeak <- format(Single_Year$EmPeak, "%B %d %Y")
    
    ## Change column names to reader-friendly
    colnames(Single_Year) <- c("Spawn Start", "Spawn Peak", "Spawn End", 
                             "Hatch Start", "Hatch Peak", "Emergence Start", 
                             "Emergence Peak")
    
    
    Single_Year
  })
  
  ## Render plot for the current year
  output$ThisYearPlot <- renderPlot({
    
    ## Get variables from inputs
    Species <- input$Species
    SpringYear <- 2025
    
    ## Calculate graph inputs
    FallYear <- SpringYear-1
    StartDate <- paste(toString(FallYear), "-10-1", sep="")
    EndDate <- paste(toString(SpringYear), "-7-1", sep="")
    
    ## Display plot - old years
    show_graph(All_Temps,
               Graph_Crit_Dates,
               as.Date(StartDate),
               as.Date(EndDate),
               FallYear,
               SpringYear)
  })
  
  ## Render table for all key dates tab
  output$AllKeyDatesTable <- renderTable({
    
    ## Copy Table of critical dates
    Key_Dates <- Graph_Crit_Dates
    
    ## Change integer year (2008) to string range (2007-2008)
    Key_Dates$Year <- paste(Key_Dates$Year - 1, Key_Dates$Year, sep = "-")
    #Key_Dates$Year <- Map(paste, Key_Dates$Year - 1, Key_Dates$Year, sep="-")
    
    ## Confirm dates are formatted correctly. Can change formatting to something
    ##  else if desired, e.g. '%Y-%m-%d', but must specify a formatting.
    Key_Dates$SpawnStart <- format(Key_Dates$SpawnStart, "%B %d %Y")
    Key_Dates$SpawnPeak <- format(Key_Dates$SpawnPeak, "%B %d %Y")
    Key_Dates$SpawnEnd <- format(Key_Dates$SpawnEnd, "%B %d %Y")
    Key_Dates$HaStart <- format(Key_Dates$HaStart, "%B %d %Y")
    Key_Dates$HaPeak <- format(Key_Dates$HaPeak, "%B %d %Y")
    Key_Dates$EmStart <- format(Key_Dates$EmStart, "%B %d %Y")
    Key_Dates$EmPeak <- format(Key_Dates$EmPeak, "%B %d %Y")
      
    ## Change column names to reader-friendly
    colnames(Key_Dates) <- c("Year", "Spawn Start", "Spawn Peak", "Spawn End", 
                             "Hatch Start", "Hatch Peak", "Emergence Start", 
                             "Emergence Peak")
    
    ## Return final table
    Key_Dates
  })
}
