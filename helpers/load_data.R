

### Query USGS for necessary data; interpolate and clean data, then return. 
get_USGS <- function(WTP, GD, TD, TC, Temp, Dis, FallStart, SpringEnd, StDate, EndDate) {
  
  ## Download continuous water data from USGS servers from two water stations
  
  GD_Data <- readNWISuv(GD, TD, startDate = FallStart, 
                        endDate = SpringEnd, tz="America/New_York")
  WTP_Data <- readNWISuv(WTP, TC, startDate = FallStart, 
                         endDate = SpringEnd, tz="America/New_York")
  
  ## Reformat data for water station 1; 
  ##      calculate daily mean temp from quarter-hourly temp; 
  ##      get max discharge from full day's record; 
  ##      add entries for missing dates
  
  GD_Data$Date <- as.Date(GD_Data$dateTime)
  GD_mean <- ddply(GD_Data,c("Date"), summarise,
                   meanT = mean(X_00010_00000, na.rm=TRUE),
                   maxD = max(X_00060_00000, na.rm=TRUE))
  GD_mean <- complete(GD_mean, Date=seq.Date(StDate, EndDate, by="1 day"))
  
  ## Reformat data for water station 2; 
  ##      calculate daily mean temp from quarter-hourly temp; 
  ##      max discharge from full day's record;
  ##      add entries for missing dates
  
  WTP_Data$Date <- as.Date(WTP_Data$dateTime)
  WTP_mean <- ddply(WTP_Data,c("Date"), summarise,
                    meanT = mean(X_00010_00000, na.rm=TRUE))
  
  WTP_mean <- complete(WTP_mean, Date=seq.Date(StDate, EndDate, by="1 day"))
  
  ## Interpolate to fill missing values in daily temperature data for station 1
  
  x <- zoo(GD_mean$meanT,GD_mean$Date)
  x <- as.ts(x)
  x <- na.interp(x)
  GD_mean$meanT <- x
  
  ## Interpolate to fill missing values in daily temperature data for station 2
  
  x <- zoo(WTP_mean$meanT,WTP_mean$Date)
  x <- as.ts(x)
  x <- na.interp(x)
  WTP_mean$meanT <- x
  
  ## Merge data from gages into one matrix
  
  Temps <- merge(GD_mean, WTP_mean, by="Date")
  Temps <- complete(Temps, Date=seq.Date(StDate, EndDate, by="1 day"))
  
  Temps$Year <- format(Temps$Date,'%Y')
  
  Temps$MeanT <- rowMeans(cbind(Temps$meanT.x, Temps$meanT.y), na.rm = TRUE)
  
  ## DELETE all interpolated temp values after today's date IF this year's data is the current water year
  
  today_date <- as.Date(Sys.time(), tz = "America/New_York")
  for(i in 1:nrow(Temps)) {
    if (Temps$Date[[i]] > today_date) {
      Temps$meanT.y[[i]] <- NA
      Temps$meanT.x[[i]] <- NA
      Temps$MeanT[[i]] <- NA
    }
  }
  
  
  return(Temps)
  
}


### Given the current date and the date of last update, download any missing data
update_data <- function(today, last_update) {
  
  ## Set ID numbers for gage locations and parameters to download data from USGS
  
  WTP <-"02012800"
  GD <- "02011800"
  
  TD <- c("00010", "00060")
  TC <- "00010"
  Temp <- "X_00010_00000"
  Dis <- "X_00060_00000"
  
  ## Determine water year of current date
  
  if(as.integer(format(today, "%m")) < 10) { # before october
    this_water_year <- as.integer(format(today, "%Y"))
  } else if(as.integer(format(today, "%m")) == 10) { # in october
    if(as.integer(format(today, "%d")) == 1) {
      this_water_year <- as.integer(format(today, "%Y"))
    } else {
      this_water_year <- as.integer(format(today, "%Y")) + 1
    }
  } else { # past october 1st 
    this_water_year <- as.integer(format(today, "%Y")) + 1
  }
  
  ## Determine water year of last update
  
  if(as.integer(format(last_update, "%m")) < 10) { # before october
    last_water_year <- as.integer(format(last_update, "%Y"))
  } else if(as.integer(format(last_update, "%m")) == 10) { # in october
    if(as.integer(format(last_update, "%d")) == 1) {
      last_water_year <- as.integer(format(last_update, "%Y"))
    } else {
      last_water_year <- as.integer(format(last_update, "%Y")) + 1
    }
  } else { # past october 1st 
    last_water_year <- as.integer(format(last_update, "%Y")) + 1
  }
  
  ## Loop through every missing / incomplete water year and download data
  
  for(i in last_water_year:this_water_year) {
    
    ## Set/format year and dates

    FallStart <- paste(toString(i-1), "-10-1", sep="")
    SpringEnd <- paste(toString(i), "-9-30", sep="")
    
    StDate <- as.Date(FallStart)
    EndDate <- as.Date(SpringEnd)
    
    ## Get the year's data from USGS, clean it, and interpolate
    
    Temps_This_Year <- get_USGS(WTP, GD, TD, TC, Temp, Dis, FallStart, SpringEnd, StDate, EndDate)
    
    ## Write the year's data to a CSV file
    
    filename = paste("data/", 
                     toString(i-1), 
                     "-", 
                     toString(i), 
                     "_Temp_Discharge.csv", sep="")
    write.csv(Temps_This_Year, filename)
    
    ## Run Brown Trout model to update critical dates
    
    source("./helpers/model.R")
    BT_crit_dates <- BT_model(Temps_This_Year, i)
    print(BT_crit_dates)
    
    ## Update the CSV file with Brown Trout critical dates
    
    filename = "data/BT_Critical_Dates.csv"
    old_data <- read.csv(filename, 
                         colClasses = c("integer", "Date", "Date", "Date", "Date", 
                                        "Date", "Date", "Date"))
    
    check_to_replace <- old_data %>%
      filter(Year == i)
    
    if(nrow(check_to_replace) == 1) { # if the critical dates already has an entry for this water year
      old_data[old_data$Year == i,] <- BT_crit_dates
    } else {
      old_data <- old_data %>%
        bind_rows(BT_crit_dates)
    }
    
    write.csv(old_data, filename, row.names=FALSE)
    
    ## Run Rainbow Trout model to update critical dates

    RT_crit_dates <- RT_model(Temps_This_Year, i)

    ## Update the CSV file with Rainbow Trout critical dates

    filename = "data/RT_Critical_Dates.csv"
    old_data <- read.csv(filename,
                         colClasses = c("integer", "integer", "Date", "Date", "Date", "Date", "Date", "Date", "Date","integer",
                                        "Date", "Date", "Date", "Date", "Date", "Date", "Date"))
    check_to_replace <- old_data %>%
      filter(Year == i)

    if(nrow(check_to_replace) == 1) { # if the critical dates already has an entry for this water year
      old_data[old_data$Year == i,] <- RT_crit_dates
    } else {
      old_data <- old_data %>%
        bind_rows(RT_crit_dates)
    }
    
    write.csv(old_data, filename, row.names=FALSE)
    

  }

  
  
  
}


### Check date of last update, determine whether new data must be downloaded
load_data <- function() {
  
  ## Determine current date and date of last update (all in UTC)
  today_UTC <- as.Date(Sys.time(), tz = "UTC")
  my_data <- read_delim("last_update.txt", delim = "\t", col_names = FALSE)
  this_last_update <- as.Date(my_data[[1,1]])
  
  ## Check if data needs to be updated/downloaded
  if(today_UTC > this_last_update) { 
    
    ## Update the data CSV files
    update_data(today_UTC, this_last_update) 
    
    ## Update last_update.txt
    my_data[1,1] <- today_UTC
    write.table(my_data, file = "last_update.txt", sep = "\t",
                row.names = FALSE, col.names = FALSE)
  }
  
}

