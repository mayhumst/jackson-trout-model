

### BROWN TROUT MODEL
### Uses a linear model to calculate critical dates for each year.
BT_model <- function(This_Year_Temps, this_year) {
  
  ## Create single-row matrix to store critical dates from this water year
  
  Graph_Crit_Dates = matrix(data=NA, nrow=1, ncol=8)
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
  
  ## Determine start/end date vars
    
  SpringYear <- this_year
  FallYear <- SpringYear-1

  FallStart <- paste(toString(FallYear), "-10-1", sep="")
  SpringEnd <- paste(toString(SpringYear), "-9-30", sep="")
  
  StDate <- as.Date(FallStart)
  EndDate <- as.Date(SpringEnd)
  
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
  
  
  ## Critical dates such as spawning start, peak spawn, etc. are appended to matrix
  
  Graph_Crit_Dates[1,1] <- SpringYear
  Graph_Crit_Dates[1,2] <- FSp_Start
  Graph_Crit_Dates[1,3] <- FSp_peak
  Graph_Crit_Dates[1,4] <- FSp_End
  Graph_Crit_Dates[1,5] <- EH_hatch_start_date
  Graph_Crit_Dates[1,6] <- EH_hatch_peak_date
  Graph_Crit_Dates[1,7] <- emergence_start_date
  Graph_Crit_Dates[1,8] <- emergence_peak_date
  
  
  return(Graph_Crit_Dates)
  
}



RT_model <- function(Temps, this_year) {
  
  ## Create single-row matrix to store critical dates from this water year
  
  RBT_CritDates = matrix(data=NA, nrow=1, ncol=9)
  colnames(RBT_CritDates) <- c("Year", "WinSpawnLen", "WinSpawn", "WinHatch", "WinEmerg", 
                               "SprSpawnLen", "SprSpawn","SprHatch", "SprEmerg")
  RBT_CritDates <- as.data.frame(RBT_CritDates)
  RBT_CritDates$WinSpawn <- as.Date(RBT_CritDates$WinSpawn)
  RBT_CritDates$WinHatch <- as.Date(RBT_CritDates$WinHatch)
  RBT_CritDates$WinEmerg <- as.Date(RBT_CritDates$WinEmerg)
  RBT_CritDates$SprSpawn <- as.Date(RBT_CritDates$SprSpawn)
  RBT_CritDates$SprHatch <- as.Date(RBT_CritDates$SprHatch)
  RBT_CritDates$SprEmerg <- as.Date(RBT_CritDates$SprEmerg)
  
  ## Determine start/end date vars
  
  SpringYear <- this_year
  FallYear <- SpringYear-1
  
  FallStart <- paste(toString(FallYear), "-10-1", sep="")
  SpringEnd <- paste(toString(SpringYear), "-10-1", sep="")
  
  WinterSpawnStart <- paste(toString(FallYear), "-12-14", sep="")
  WinterSpawnEnd <- paste(toString(SpringYear), "-1-16", sep="")
  SpringSpawnStart <- paste(toString(SpringYear), "-1-31", sep="") 
  
  ## In order to catch potential early spring (Feb) spawning periods,
  ## the SpringSpawnStart date is set to January 31
  
  StDate <- as.Date(FallStart)
  EndDate <- as.Date(SpringEnd)
  SpringSpawnStart <- as.Date(SpringSpawnStart)
  WinterSpawnStart <- as.Date(WinterSpawnStart)
  WinterSpawnEnd <- as.Date(WinterSpawnEnd)
  
  ## Parameters for Elliott and Hurley 1998 model
  ## The EH model is not as well supported for rainbow trout.
  ## The Zeug et al linear model works well, and is preferred.
  ## EH model could be retained as an option.
  
  # T0 <- -2.487
  # T1 <- 22.403
  # C_A50 <- 36.35
  # C_H50 <- 39.86
  
  #################### ******************************************************************************************** 
  ## ??
  #Temps <- complete(This_Year_Temps, Date=seq.Date(StDate, EndDate, by="1 day"))
  
  #Temps$Date <- as.Date(Temps$Date)
  
  ## Interpolate to fill missing values in daily temperature data
  # x <- zoo(Temps$MeanT,Temps$Date)
  # x <- as.ts(x)
  # x <- na.interp(x)
  # Temps$MeanT <- x
  
  # Determine length and average date of winter spawn
  S1 <- subset(Temps, Date > WinterSpawnStart)
  S1 <- subset(S1, Date < WinterSpawnEnd)
  S1 <- subset(S1, MeanT < 9)
  S1 <- subset(S1, MeanT > 6)
  WinSpawnLen <- length(S1$Date)
  WinSpawnAvg <- mean(S1$Date)
  
  ## THERE IS A PROBLEM WITH THE ABOVE ALGORITHM.###
  ## IF THERE ARE NO DATES IN THE WINTER RANGE WHERE TEMP IS 6-9 C, 
  ## THEN THIS LAST LINE RETURNS NaN.  IF THAT IS THE CASE, NEED TO SOMEHOW
  ## ADJUST TO NOT EXECUTE ALL THE FUNCTIONS RELATED TO WINTER SPAWN.
  ## WILL LIKELY NEED TO NEST SOME CONDITIONAL STATEMENTS IN HERE TO ACCOMODATE.
  
  # Determine average date for Spring spawn
  
  S1 <- subset(Temps, Date > SpringSpawnStart)
  S1 <- subset(S1, Date < SpringEnd)
  S1 <- subset(S1, MeanT < 9)
  S1 <- subset(S1, MeanT > 6)
  SpringSpawnLen <- length(S1$Date)
  SpringSpawnAvg <- mean(S1$Date)
  
  ## Calculate hatch /emergence for winter spawned embryos. The  
  ## linear model applied below mimics the approach used in the 
  ## embryo development model by Zeug et al 2012.  The emergence
  ## model is parameterized with data from Murray et al 1980.
  
  ## The following if/else structure is required to accomodate years where 
  ## temperatures are too cold for a winter spawn from Dec 15 - Jan 15.
  
  if(is.na(WinSpawnAvg)){
    Z_hatch_Win_date <- "Na"
    emergence_Win_date <- "Na"
  } else {
    # Determine hatch dates
    HaWin <- subset(Temps, Date > WinSpawnAvg)
    HaWin$dmatZ <- 0.0032*HaWin$MeanT+0.003
    HaWin$pmatZ <- cumsum(HaWin$dmatZ)
    threshold <- 1
    # Find the first index where "Value" exceeds the threshold
    Z_Win_first_exceed_index <- which.max(HaWin$pmatZ > threshold)
    # Extract the corresponding dates
    Z_hatch_Win_date <- HaWin$Date[Z_Win_first_exceed_index]
    # Determine emergence dates
    EmWin <- subset(Temps, Date > Z_hatch_Win_date)
    EmWin$dmatZ <- 0.0056*EmWin$MeanT-0.0045
    EmWin$pmatZ <- cumsum(EmWin$dmatZ)
    first_exceed_index <- which.max(EmWin$pmatZ > threshold)
    emergence_Win_date <- EmWin$Date[first_exceed_index]
  }
  
  
  ##  Calculate hatch dates for winter spawned embryos.
  
  HaSpr<- subset(Temps, Date > SpringSpawnAvg)
  HaSpr$dmatZ <- 0.0032*HaSpr$MeanT+0.003
  HaSpr$pmatZ <- cumsum(HaSpr$dmatZ)
  
  threshold <- 1  # Set your desired threshold value
  
  # Find the first index where "Value" exceeds the threshold
  
  Z_Spr_first_exceed_index <- which.max(HaSpr$pmatZ > threshold)
  
  # Extract the corresponding dates
  
  Z_hatch_Spr_date <- HaSpr$Date[Z_Spr_first_exceed_index]
  
  ##  Calculate emergence dates for winter spawned embryos.
  
  EmSpr <- subset(Temps, Date > Z_hatch_Spr_date)
  EmSpr$dmatZ <- 0.0056*EmSpr$MeanT-0.0045
  EmSpr$pmatZ <- cumsum(EmSpr$dmatZ)
  
  threshold <- 1
  first_exceed_index <- which.max(EmSpr$pmatZ > threshold)
  emergence_Spr_date <- EmSpr$Date[first_exceed_index]
  
  
  ## Critical dates such as spawning start, peak spawn, etc. are appended to matrix
  
  RBT_CritDates[1,1] <- this_year
  
  if(is.na(WinSpawnAvg)) {
    RBT_CritDates[1,6] <- SpringSpawnLen
    RBT_CritDates[1,7] <- SpringSpawnAvg
    RBT_CritDates[1,8] <- Z_hatch_Spr_date
    RBT_CritDates[1,9] <- emergence_Spr_date
  } else {
    RBT_CritDates[1,2] <- WinSpawnLen
    RBT_CritDates[1,3] <- WinSpawnAvg
    RBT_CritDates[1,4] <- Z_hatch_Win_date
    RBT_CritDates[1,5] <- emergence_Win_date
    RBT_CritDates[1,6] <- SpringSpawnLen
    RBT_CritDates[1,7] <- SpringSpawnAvg
    RBT_CritDates[1,8] <- Z_hatch_Spr_date
    RBT_CritDates[1,9] <- emergence_Spr_date
  }
  
  return(RBT_CritDates)
}







