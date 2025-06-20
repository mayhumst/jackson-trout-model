
### Get a years worth of temp/discharge data from a CSV file (used by the reactive plots)
get_year_data <- function(year) {
  
  ## Read the correct CSV file and return the output
  
  filename = paste("save_2/", 
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
    filename <- "save_2/BT_Critical_Dates.csv"
    dates <- read.csv(filename, 
                         colClasses = c("integer", "Date", "Date", "Date", "Date", 
                                        "Date", "Date", "Date"))
    correct_row <- dates %>%
      filter(Year == year)
    return(correct_row)
    
  } else if(species == "Rainbow") {
    
  } else {
    return(NULL)
  }
  
}


### Get all years' worth of critical dates
get_all_dates <- function(species) {
  
  ## Read the correct CSV file of critical dates and return all dates
  
  if(species == "Brown") {
    filename <- "save_2/BT_Critical_Dates.csv"
    dates <- read.csv(filename, 
                      colClasses = c("integer", "Date", "Date", "Date", "Date", 
                                     "Date", "Date", "Date"))
    return(dates)
    
  } else if(species == "Rainbow") {
    
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
  
  ## Return
  return(this_water_year)
}
