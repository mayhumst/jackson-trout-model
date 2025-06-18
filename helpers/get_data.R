
#
# This module establishes helper functions to gather, clean, and interpolate 
#   data from USGS.
#



### Query USGS for necessary data; interpolate and clean data, then return. 
query_USGS <- function(WTP, GD, TD, TC, Temp, Dis, FallStart, SpringEnd, StDate, EndDate) {
  
  ## Download continuous water data from USGS servers
  
  GD_Data <- readNWISuv(GD, TD, startDate = FallStart, 
                        endDate = SpringEnd)
  WTP_Data <- readNWISuv(WTP, TC, startDate = FallStart, 
                         endDate = SpringEnd)
  
  GD_Data$Date <- as.Date(GD_Data$dateTime)
  GD_mean <- ddply(GD_Data,c("Date"), summarise,
                   meanT = mean(X_00010_00000, na.rm=TRUE),
                   maxD = max(X_00060_00000, na.rm=TRUE))
  
  GD_mean <- complete(GD_mean, Date=seq.Date(StDate, EndDate, by="1 day"))
  
  ## Interpolate to fill missing values in daily temperature data
  
  x <- zoo(GD_mean$meanT,GD_mean$Date)
  x <- as.ts(x)
  x <- na.interp(x)
  GD_mean$meanT <- x
  
  WTP_Data$Date <- as.Date(WTP_Data$dateTime)
  WTP_mean <- ddply(WTP_Data,c("Date"), summarise,
                    meanT = mean(X_00010_00000, na.rm=TRUE))
  
  WTP_mean <- complete(WTP_mean, Date=seq.Date(StDate, EndDate, by="1 day"))
  
  ## Interpolate to fill missing values in daily temperature data
  
  x <- zoo(WTP_mean$meanT,WTP_mean$Date)
  x <- as.ts(x)
  x <- na.interp(x)
  WTP_mean$meanT <- x
  
  ## Merge data from gages into one matrix
  
  Temps <- merge(GD_mean, WTP_mean, by="Date")
  Temps <- complete(Temps, Date=seq.Date(StDate, EndDate, by="1 day"))
  
  Temps$Year <- format(Temps$Date,'%Y')
  
  Temps$MeanT <- rowMeans(cbind(Temps$meanT.x, Temps$meanT.y), na.rm = TRUE)
  
  return(Temps)
  
}



# ### Clean and interpolate raw USGS data, then return it. 
# clean_year_data <- function()


### Get the data for Brown Trout. 
get_BT_data <- function(StYear, EndYear) {
  
  ## Set range of dates where data is available
  lp <- EndYear - StYear+1
  
  ## Set ID numbers for gage locations and parameters to download data from USGS
  
  WTP <-"02012800"
  GD <- "02011800"
  
  TD <- c("00010", "00060")
  TC <- "00010"
  Temp <- "X_00010_00000"
  Dis <- "X_00060_00000"
  
  ## Create an "All_Temps" variable that will hold the data across all years

  All_Temps <- matrix(data=NA, nrow=0, ncol=6)
  colnames(All_Temps) <- c("Date", "meanT.x", "maxD",
                               "meanT.y", "Year", "MeanT")
  All_Temps <- as.data.frame(All_Temps)
  All_Temps$Date <- as.Date(All_Temps$Date)
  All_Temps$Year <- as.character(All_Temps$Year)
  All_Temps$meanT.x <- as.numeric(All_Temps$meanT.x)
  All_Temps$meanT.y <- as.numeric(All_Temps$meanT.y)
  All_Temps$MeanT <- as.numeric(All_Temps$MeanT)
  All_Temps$maxD <- as.numeric(All_Temps$maxD)
  
  ## Loop: for every year available, download the data from USGS
  
  for(i in StYear:EndYear) {
    
    ## Set/format year and dates

    SpringYear <- i
    FallYear <- SpringYear-1
    lp <- i-StYear+1        ## Used for indexing matrix storing critical dates
    
    FallStart <- paste(toString(FallYear), "-10-1", sep="")
    SpringEnd <- paste(toString(SpringYear), "-9-30", sep="")
    
    StDate <- as.Date(FallStart)
    EndDate <- as.Date(SpringEnd)
    
    ## Download, interpolate, and format data from USGS. 
    
    Temps_This_Year <- query_USGS(WTP, GD, TD, TC, Temp, Dis, FallStart, SpringEnd, StDate, EndDate)
    
    ## Add this year's data to the final total dataframe.
    
    All_Temps <- All_Temps %>%
      bind_rows(Temps_This_Year)
    
  }
  
  return(All_Temps)
}



### Uses a linear model to calculate critical dates for each year.
model_crit_dates <- function(Temps, StYear, EndYear) {
  
  ## Set range of years where data is available
  lp <- EndYear - StYear+1
  
  ## Create matrix to store critical dates from each year of analysis
  
  BNT_CritDates = matrix(data=NA, nrow=lp, ncol=5)
  colnames(BNT_CritDates) <- c("Year", "SpawnStart", "SpawnPeak", 
                               "HaPeak", "EmPeak")
  BNT_CritDates <- as.data.frame(BNT_CritDates)
  BNT_CritDates$SpawnStart <- as.Date(BNT_CritDates$SpawnStart)
  BNT_CritDates$SpawnPeak <- as.Date(BNT_CritDates$SpawnPeak)
  BNT_CritDates$HaPeak <- as.Date(BNT_CritDates$HaPeak)
  BNT_CritDates$EmPeak <- as.Date(BNT_CritDates$EmPeak)
  
  ## Create BIGGER matrix to store critical dates necessary for graph
  
  Graph_Crit_Dates = matrix(data=NA, nrow=lp, ncol=8)
  colnames(Graph_Crit_Dates) <- c("Year", "SpawnStart", "SpawnPeak", "SpawnEnd", 
                                  "HaStart", "HaPeak", "EmStart", "EmPeak")
  Graph_Crit_Dates <- as.data.frame(Graph_Crit_Dates)
  Graph_Crit_Dates$SpawnStart <- as.Date(Graph_Crit_Dates$SpawnStart)
  Graph_Crit_Dates$SpawnPeak <- as.Date(Graph_Crit_Dates$SpawnPeak)
  Graph_Crit_Dates$SpawnEnd <- as.Date(Graph_Crit_Dates$SpawnEnd)
  Graph_Crit_Dates$HaPeak <- as.Date(Graph_Crit_Dates$HaPeak)
  Graph_Crit_Dates$EmPeak <- as.Date(Graph_Crit_Dates$EmPeak)
  Graph_Crit_Dates$HaStart <- as.Date(Graph_Crit_Dates$HaStart)
  Graph_Crit_Dates$EmStart <- as.Date(Graph_Crit_Dates$EmStart)
  
  ## Loop through each year available.
  
  for(i in StYear:EndYear) {
    
    SpringYear <- i
    FallYear <- SpringYear-1
    lp <- i-StYear+1        ## Used for indexing matrix storing critical dates
    
    FallStart <- paste(toString(FallYear), "-10-1", sep="")
    SpringEnd <- paste(toString(SpringYear), "-9-30", sep="")
    
    StDate <- as.Date(FallStart)
    EndDate <- as.Date(SpringEnd)
    
    ## Get the subset of "Temps" that contains one hatch year.
    
    This_Year_Temps <- subset(Temps, Date >= StDate)
    This_Year_Temps <- subset(This_Year_Temps, Date <= EndDate)
    
    ## Determine start and end dates for Fall spawn
    
    F1 <- subset(This_Year_Temps, Year == FallYear)
    F1 <- subset(F1, MeanT < 12)
    F1 <- subset(F1, MeanT > 6)
    FSp_Start <- min(F1$Date, na.rm = TRUE)
    FSp_End <- max(F1$Date, na.rm=TRUE)
    
    ## Determine average date for peak Fall spawn
    
    F1 <- subset(F1, MeanT < 10 | MeanT > 8)
    FSp_peak <- mean.Date(F1$Date)
    
    HaEarly <- subset(This_Year_Temps, Date > FSp_Start)
    HaPeak <- subset(This_Year_Temps, Date > FSp_peak)
    
    ## The linear model below mimics the approach used in the model
    ## by Zeug et al 2012.  It was abandoned in favor of the nonlinear
    ## model of Elliott and Hurley, but could easily be retained as an option.
    
    # HaPeak$dmatZ <- 0.0032*HaPeak$MeanT-0.0056
    # HaPeak$pmatZ <- cumsum(HaPeak$dmatZ)
    
    
    ## Parameters for Elliott and Hurley 1998 model
    
    T0 <- -2.487
    T1 <- 22.403
    C_A50 <- 36.35
    C_H50 <- 39.86
    
    # The nonlinear model below was proposed by Elliott and Hurley 1998
    # It does a decent job of predicting hatch and emergence in Syrjanen et al. 2008
    
    HaEarly$dmatEH <- 1 / (C_H50*(T1-HaEarly$MeanT)/(HaEarly$MeanT-T0))
    HaEarly$pmatEH <- cumsum(HaEarly$dmatEH)
    
    HaPeak$dmatEH <- 1 / (C_H50*(T1-HaPeak$MeanT)/(HaPeak$MeanT-T0))
    HaPeak$pmatEH <- cumsum(HaPeak$dmatEH)
    
    threshold <- 1  # Set your desired threshold value
    
    ## Find the first index where "Value" exceeds the threshold
    
    # Z_first_exceed_index <- which.max(HaPeak$pmatZ > threshold)
    EH_first_exceed_index <- which.max(HaPeak$pmatEH > threshold)
    EH_early_first_exceed_index <- which.max(HaEarly$pmatEH > threshold)
    
    ## Extract the corresponding date
    
    # Z_hatch_start_date <- HaPeak$Date[Z_first_exceed_index]
    EH_hatch_start_date <- HaPeak$Date[EH_early_first_exceed_index]
    EH_hatch_peak_date <- HaPeak$Date[EH_first_exceed_index]
    
    # Calculate emergence date following Elliott and Hurley 1998
    
    EmEarly <- subset(This_Year_Temps, Date > EH_hatch_start_date)
    EmEarly$dmat <- 1 / (C_A50*(T1-EmEarly$MeanT)/(EmEarly$MeanT-T0))
    EmEarly$pmat <- cumsum(EmEarly$dmat)
    threshold <- 1
    first_exceed_index <- which.max(EmEarly$pmat > threshold)
    emergence_start_date <- EmEarly$Date[first_exceed_index]
    
    
    EmPeak <- subset(This_Year_Temps, Date > EH_hatch_peak_date)
    EmPeak$dmat <- 1 / (C_A50*(T1-EmPeak$MeanT)/(EmPeak$MeanT-T0))
    EmPeak$pmat <- cumsum(EmPeak$dmat)
    threshold <- 1
    first_exceed_index <- which.max(EmPeak$pmat > threshold)
    emergence_peak_date <- EmPeak$Date[first_exceed_index]
    
    
    ## Critical dates such as spawning start, peak spawn, etc. are appended to an output file
    BNT_CritDates[lp,1] <- SpringYear
    BNT_CritDates[lp,2] <- FSp_Start
    BNT_CritDates[lp,3] <- FSp_peak
    BNT_CritDates[lp,4] <- EH_hatch_peak_date
    BNT_CritDates[lp,5] <- emergence_peak_date
    
    Graph_Crit_Dates[lp,1] <- SpringYear
    Graph_Crit_Dates[lp,2] <- FSp_Start
    Graph_Crit_Dates[lp,3] <- FSp_peak
    Graph_Crit_Dates[lp,4] <- FSp_End
    Graph_Crit_Dates[lp,5] <- EH_hatch_start_date
    Graph_Crit_Dates[lp,6] <- EH_hatch_peak_date
    Graph_Crit_Dates[lp,7] <- emergence_start_date
    Graph_Crit_Dates[lp,8] <- emergence_peak_date
    
  }
  
  return(Graph_Crit_Dates)
  
}

