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
                      colClasses = c("integer", "integer", "Date", "Date", "Date", "integer", 
                                     "Date", "Date", "Date"))
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
                      colClasses = c("integer", "integer", "Date", "Date", "Date", "integer", 
                                     "Date", "Date", "Date"))
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
    return(Single_Year)
    
  }
  
  ## Rainbow Trout
  if(Species == "Rainbow") {
    
    ## Get the single row of critical dates
    Single_Year <- get_year_dates(as.integer(Year), Species)
    ## Drop the "Year" column
    Single_Year <- Single_Year[c("WinSpawnLen", "WinSpawn", "WinHatch", 
                                 "WinEmerg", "SprSpawnLen", "SprSpawn", "SprHatch", "SprEmerg")]
    
    ## Confirm dates are formatted correctly. Can change formatting to something
    ##  else if desired, e.g. '%Y-%m-%d', but must specify a formatting.
    Single_Year$WinSpawn <- format(Single_Year$WinSpawn, "%B %d %Y")
    Single_Year$WinHatch <- format(Single_Year$WinHatch, "%B %d %Y")
    Single_Year$WinEmerg <- format(Single_Year$WinEmerg, "%B %d %Y")
    Single_Year$SprSpawn <- format(Single_Year$SprSpawn, "%B %d %Y")
    Single_Year$SprHatch <- format(Single_Year$SprHatch, "%B %d %Y")
    Single_Year$SprEmerg <- format(Single_Year$SprEmerg, "%B %d %Y")
    
    ## Change column names to reader-friendly
    colnames(Single_Year) <- c("Winter Spawn Length", "Winter Spawn Average", "Winter Hatch Average", 
                               "Winter Emergence Average", "Spring Spawn Length", "Spring Spawn Average", "Spring Hatch Average", 
                               "Spring Emergence Average")
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
    Key_Dates$WinSpawn <- format(Key_Dates$WinSpawn, "%B %d %Y")
    Key_Dates$WinHatch <- format(Key_Dates$WinHatch, "%B %d %Y")
    Key_Dates$WinEmerg <- format(Key_Dates$WinEmerg, "%B %d %Y")
    Key_Dates$SprSpawn <- format(Key_Dates$SprSpawn, "%B %d %Y")
    Key_Dates$SprHatch <- format(Key_Dates$SprHatch, "%B %d %Y")
    Key_Dates$SprEmerg <- format(Key_Dates$SprEmerg, "%B %d %Y")
    
    ## Change column names to reader-friendly
    colnames(Key_Dates) <- c("Year", "Winter Spawn Length", "Winter Spawn Average", "Winter Hatch Average", 
                               "Winter Emergence Average", "Spring Spawn Length", "Spring Spawn Average", "Spring Hatch Average", 
                               "Spring Emergence Average")
    
    ## Return final table
    return(Key_Dates)
  }
  
}