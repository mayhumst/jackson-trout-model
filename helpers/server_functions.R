#
# This module contains helper functions to be used by the server to query for necessary data
#

### Get a single year's worth of temp/discharge data from a CSV file (used by the reactive plots)
get_year_data <- function(year) {
  
  ## Read the correct CSV file and return the output
  
  filename = paste("data/", 
                   toString(year-1), 
                   "-", 
                   toString(year), 
                   "_Temp_Discharge.csv", sep="")
  data <- read.csv(filename)
  data$Date <- as.Date(data$Date, "%Y-%m-%d")
  data$Year <- as.character(data$Year)
  data$meanT.x <- as.numeric(data$meanT.x)
  data$meanT.y <- as.numeric(data$meanT.y)
  data$maxD <- as.integer(data$maxD)
  data$MeanT <- as.numeric(data$MeanT)
  
  return(data)
  
} 


### Get a single year's worth of critical dates
get_year_dates <- function(year, species) {
  
  ## Read the correct CSV file of critical dates and return one year's dates
  
  if(species == "Brown") {
    
    ## Return Brown Trout critical dates
    
    filename <- "data/BT_Critical_Dates.csv"
    dates <- read.csv(filename, 
                         colClasses = c("integer", "Date", "Date", "Date", "Date", 
                                        "Date", "Date", "Date"))
    correct_row <- dates %>%
      filter(Year == year)
    return(correct_row)
    
  } else if(species == "Rainbow") {
    
    ## Return Rainbow Trout critical dates
    
    filename <- "data/RT_Critical_Dates.csv"
    dates <- read.csv(filename, 
                      colClasses = c("integer", "integer", "Date", "Date", "Date", "Date", "Date", "Date", "Date","integer",
                                     "Date", "Date", "Date", "Date", "Date", "Date", "Date"))
    correct_row <- dates %>%
      filter(Year == year)
    return(correct_row)
    
  } else {
    return(NULL)
  }
  
}


### Get all years' worth of critical dates
get_all_dates <- function(species) {
  
  ## Read the correct CSV file of critical dates and return all dates
  
  if(species == "Brown") {
    ## Return Brown Trout critical dates
    filename <- "data/BT_Critical_Dates.csv"
    dates <- read.csv(filename, 
                      colClasses = c("integer", "Date", "Date", "Date", "Date", 
                                     "Date", "Date", "Date"))
    return(dates)
    
  } else if(species == "Rainbow") {
    ## Return Rainbow Trout critical dates
    filename <- "data/RT_Critical_Dates.csv"
    dates <- read.csv(filename, 
                      colClasses = c("integer", "integer", "Date", "Date", "Date", "Date", "Date", "Date", "Date","integer",
                                     "Date", "Date", "Date", "Date", "Date", "Date", "Date"))
    return(dates)
    
  } else {
    return(NULL)
  }
}


### Determine the current water year
calculate_water_year <- function(this_date) {
  
  ## Determine water year of current date
  if(as.integer(format(this_date, "%m")) < 10) { # before october
    this_water_year <- as.integer(format(this_date, "%Y"))
  } else if(as.integer(format(this_date, "%m")) == 10) { # in october
    if(as.integer(format(this_date, "%d")) == 1) {
      this_water_year <- as.integer(format(this_date, "%Y"))
    } else {
      this_water_year <- as.integer(format(this_date, "%Y")) + 1
    }
  } else { # past october 1st 
    this_water_year <- as.integer(format(this_date, "%Y")) + 1
  }
  
  ## Return integer water year
  return(this_water_year)
}


### Format the visual data table displaying a single year's key dates
format_single_year_data_table <- function(Species, Year) {
  
  ## Brown Trout
  if(Species == "Brown") {
    
    ## Get the single row of critical dates
    Single_Year <- get_year_dates(as.integer(Year), Species)
    ## Drop the "Year" column
    Single_Year <- Single_Year[c("SpawnStart", "SpawnPeak",  
                                 "HaStart", "HaPeak", "EmStart", "EmPeak")]
    
    ## Confirm dates are formatted correctly. Can change formatting to something
    ##  else if desired, e.g. '%Y-%m-%d', but must specify a formatting.
    Single_Year$SpawnStart <- format(Single_Year$SpawnStart, "%b %d")
    Single_Year$SpawnPeak <- format(Single_Year$SpawnPeak, "%b %d")
    Single_Year$HaStart <- format(Single_Year$HaStart, "%b %d")
    Single_Year$HaPeak <- format(Single_Year$HaPeak, "%b %d")
    Single_Year$EmStart <- format(Single_Year$EmStart, "%b %d")
    Single_Year$EmPeak <- format(Single_Year$EmPeak, "%b %d")
    
    ## Change column names to reader-friendly
    colnames(Single_Year) <- c("Spawn Start", "Spawn Peak", 
                               "Hatch Start", "Hatch Peak", "Emergence Start", 
                               "Emergence Peak")
    return(Single_Year)
    
  }
  
  ## Rainbow Trout
  if(Species == "Rainbow") {
    
    ## Get the single row of critical dates
    Single_Year <- get_year_dates(as.integer(Year), Species)
    ## Drop the "Year" column
    Single_Year <- Single_Year[c("WinSpawnLen", "WinSpawnStart", 
                                 "WinSpawnPeak", "WinSpawnEnd", "WinHatchStart", 
                                 "WinHatchPeak", "WinEmergStart", "WinEmergPeak", 
                                 "SprSpawnLen", "SprSpawnStart", "SprSpawnPeak", 
                                 "SprSpawnEnd", "SprHatchStart", "SprHatchPeak",
                                 "SprEmergStart", "SprEmergPeak")]
    
    ## Confirm dates are formatted correctly. Can change formatting to something
    ##  else if desired, e.g. '%Y-%m-%d', but must specify a formatting.
    Single_Year$WinSpawnStart <- format(Single_Year$WinSpawnStart, "%b %d")
    Single_Year$WinSpawnPeak <- format(Single_Year$WinSpawnPeak, "%b %d")
    Single_Year$WinSpawnEnd <- format(Single_Year$WinSpawnEnd, "%b %d")
    Single_Year$WinHatchStart <- format(Single_Year$WinHatchStart, "%b %d")
    Single_Year$WinHatchPeak <- format(Single_Year$WinHatchPeak, "%b %d")
    Single_Year$WinEmergStart <- format(Single_Year$WinEmergStart, "%b %d")
    Single_Year$WinEmergPeak <- format(Single_Year$WinEmergPeak, "%b %d")
    Single_Year$SprSpawnStart <- format(Single_Year$SprSpawnStart, "%b %d")
    Single_Year$SprSpawnPeak <- format(Single_Year$SprSpawnPeak, "%b %d")
    Single_Year$SprSpawnEnd <- format(Single_Year$SprSpawnEnd, "%b %d")
    Single_Year$SprHatchStart <- format(Single_Year$SprHatchStart, "%b %d")
    Single_Year$SprHatchPeak <- format(Single_Year$SprHatchPeak, "%b %d")
    Single_Year$SprEmergStart <- format(Single_Year$SprEmergStart, "%b %d")
    Single_Year$SprEmergPeak <- format(Single_Year$SprEmergPeak, "%b %d")
    
    ## Change column names to reader-friendly
    colnames(Single_Year) <- c("Winter Spawn Length", "Winter Spawn Start", 
                               "Winter Spawn Peak", "Winter Spawn End", 
                               "Winter Hatch Start", "Winter Hatch Peak", 
                               "Winter Emergence Start", "Winter Emergence Peak", 
                               "Spring Spawn Length", "Spring Spawn Start", 
                               "Spring Spawn Peak", "Spring Spawn End", 
                               "Spring Hatch Start", "Spring Hatch Peak", 
                               "Spring Emergence Start", "Spring Emergence Peak")
    return(Single_Year)
    
  }
  else {
    return(NULL)
  }
  
}


### Format the visual data table displaying all years' key dates
format_all_years_data_table <- function(Species) {
  
  ## Brown Trout 
  if(Species == "Brown") {
    
    ## Copy Table of critical dates
    Key_Dates <- get_all_dates(Species)
    
    ## Change integer year (2008) to string range (2007-2008)
    Key_Dates$Year <- paste(Key_Dates$Year - 1, Key_Dates$Year, sep = "-")

    ## Confirm dates are formatted correctly. Can change formatting to something
    ##  else if desired, e.g. '%Y-%m-%d', but must specify a formatting.
    Key_Dates$SpawnStart <- format(Key_Dates$SpawnStart, "%b %d")
    Key_Dates$SpawnPeak <- format(Key_Dates$SpawnPeak, "%b %d")
    Key_Dates$HaStart <- format(Key_Dates$HaStart, "%b %d")
    Key_Dates$HaPeak <- format(Key_Dates$HaPeak, "%b %d")
    Key_Dates$EmStart <- format(Key_Dates$EmStart, "%b %d")
    Key_Dates$EmPeak <- format(Key_Dates$EmPeak, "%b %d")
    
    # Remove "spawn end" (not necessary to show)
    Key_Dates$SpawnEnd <- NULL

    ## Change column names to reader-friendly
    colnames(Key_Dates) <- c("Year", "Spawn Start", "Spawn Peak", 
                             "Hatch Start", "Hatch Peak", "Emergence Start", 
                             "Emergence Peak")
    
    ## Return final table
    return(Key_Dates)
  }
  
  ## Rainbow Trout
  else if(Species == "Rainbow") {
    
    ## Copy Table of critical dates
    Key_Dates <- get_all_dates(Species)
    
    ## Change integer year (2008) to string range (2007-2008)
    Key_Dates$Year <- paste(Key_Dates$Year - 1, Key_Dates$Year, sep = "-")
    
    ## Confirm dates are formatted correctly. Can change formatting to something
    ##  else if desired, e.g. '%Y-%m-%d', but must specify a formatting.
    Key_Dates$WinSpawnStart <- format(Key_Dates$WinSpawnStart, "%b %d")
    Key_Dates$WinSpawnPeak <- format(Key_Dates$WinSpawnPeak, "%b %d")
    Key_Dates$WinSpawnEnd <- format(Key_Dates$WinSpawnEnd, "%b %d")
    Key_Dates$WinHatchStart <- format(Key_Dates$WinHatchStart, "%b %d")
    Key_Dates$WinHatchPeak <- format(Key_Dates$WinHatchPeak, "%b %d")
    Key_Dates$WinEmergStart <- format(Key_Dates$WinEmergStart, "%b %d")
    Key_Dates$WinEmergPeak <- format(Key_Dates$WinEmergPeak, "%b %d")
    Key_Dates$SprSpawnStart <- format(Key_Dates$SprSpawnStart, "%b %d")
    Key_Dates$SprSpawnPeak <- format(Key_Dates$SprSpawnPeak, "%b %d")
    Key_Dates$SprSpawnEnd <- format(Key_Dates$SprSpawnEnd, "%b %d")
    Key_Dates$SprHatchStart <- format(Key_Dates$SprHatchStart, "%b %d")
    Key_Dates$SprHatchPeak <- format(Key_Dates$SprHatchPeak, "%b %d")
    Key_Dates$SprEmergStart <- format(Key_Dates$SprEmergStart, "%b %d")
    Key_Dates$SprEmergPeak <- format(Key_Dates$SprEmergPeak, "%b %d")
    
    ## Change column names to reader-friendly
    colnames(Key_Dates) <- c("Year", "Winter Spawn Length", "Winter Spawn Start", 
                             "Winter Spawn Peak", "Winter Spawn End", 
                             "Winter Hatch Start", "Winter Hatch Peak", 
                             "Winter Emergence Start", "Winter Emergence Peak", 
                             "Spring Spawn Length", "Spring Spawn Start", 
                             "Spring Spawn Peak", "Spring Spawn End", 
                             "Spring Hatch Start", "Spring Hatch Peak", 
                             "Spring Emergence Start", "Spring Emergence Peak")
    
    ## Return final table
    return(Key_Dates)
  }
  
}