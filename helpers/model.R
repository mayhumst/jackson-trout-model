

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
  
  ## Dummy variables at beginning and end of water year
    
  SpringYear <- this_year
  FallYear <- SpringYear-1

  FallStart <- paste(toString(FallYear), "-10-1", sep="")
  SpringEnd <- paste(toString(SpringYear), "-9-30", sep="")
  
  StDate <- as.Date(FallStart)
  EndDate <- as.Date(SpringEnd)
  
  Graph_Crit_Dates[1,1] <- SpringYear
  
  ## Determine start and end dates for Fall spawn
  
  F1 <- subset(This_Year_Temps, Year == FallYear)
  F1 <- subset(F1, MeanT < 12)
  F1 <- subset(F1, MeanT > 6)
  
  ## CHECK: if there are NO dates in which temperatures were consistent with spawning, 
  ##        skip the calculations for spawn, hatch, and emergence.
  if(nrow(F1) == 0) {
    return(Graph_Crit_Dates)
  } 
    
  FSp_Start <- min(F1$Date, na.rm = TRUE)
  FSp_End <- max(F1$Date, na.rm=TRUE)
  Graph_Crit_Dates[1,2] <- FSp_Start
  Graph_Crit_Dates[1,4] <- FSp_End
  
  ## Determine average date for peak Fall spawn
  
  F1 <- subset(F1, MeanT < 10 | MeanT > 8)
  
  ## CHECK: if there are NO dates in which temperatures were consistent with PEAK spawning, 
  ##        skip the calculations for spawn peak, hatch, and emergence.
  if(nrow(F1) == 0) {
    return(Graph_Crit_Dates)
  } 
  
  FSp_peak <- mean.Date(F1$Date)
  Graph_Crit_Dates[1,3] <- FSp_peak
  
  # Begin calculating hatch
  
  HaStart <- subset(This_Year_Temps, Date > FSp_Start)
  HaPeak <- subset(This_Year_Temps, Date > FSp_peak)
  
  ## The linear model below mimics the approach used in the model
  ## by Zeug et al 2012.  It was abandoned in favor of the nonlinear
  ## model of Elliott and Hurley, but could easily be retained as an option.
  
  # HaPeak$dmatZ <- 0.0032*HaPeak$MeanT-0.0056
  # HaPeak$pmatZ <- cumsum(HaPeak$dmatZ)
  # threshold <- 1
  # Z_first_exceed_index <- which.max(HaPeak$pmatZ > threshold)
  # Z_hatch_start_date <- HaPeak$Date[Z_first_exceed_index]
  
  ## Parameters for Elliott and Hurley 1998 model
  
  T0 <- -2.487
  T1 <- 22.403
  C_A50 <- 36.35
  C_H50 <- 39.86
  
  # The nonlinear model below was proposed by Elliott and Hurley 1998
  # It does a decent job of predicting hatch and emergence in Syrjanen et al. 2008
  
  HaStart$dmatEH <- 1 / (C_H50*(T1-HaStart$MeanT)/(HaStart$MeanT-T0))
  HaStart$pmatEH <- cumsum(HaStart$dmatEH)
  
  HaPeak$dmatEH <- 1 / (C_H50*(T1-HaPeak$MeanT)/(HaPeak$MeanT-T0))
  HaPeak$pmatEH <- cumsum(HaPeak$dmatEH)
  
  threshold <- 1  # Set your desired threshold value
  
  ## Find the first index where "Value" exceeds the threshold, and extract the corresponding date
  
  if(nrow(subset(HaStart, pmatEH >= 1)) > 0) { # check if the threshold has yet been reached at all
    EH_start_first_exceed_index <- which.max(HaStart$pmatEH > threshold)
    EH_hatch_start_date <- HaPeak$Date[EH_start_first_exceed_index]
    Graph_Crit_Dates[1,5] <- EH_hatch_start_date
  } else {
    return(Graph_Crit_Dates)
  }
  if(nrow(subset(HaPeak, pmatEH >= 1)) > 0) { # check if the threshold has yet been reached at all
    
    EH_peak_first_exceed_index <- which.max(HaPeak$pmatEH > threshold)
    EH_hatch_peak_date <- HaPeak$Date[EH_peak_first_exceed_index]
    Graph_Crit_Dates[1,6] <- EH_hatch_peak_date
  } else {
    return(Graph_Crit_Dates)
  }

  # Calculate emergence dates following Elliott and Hurley 1998
  
  EmStart <- subset(This_Year_Temps, Date > EH_hatch_start_date)
  EmStart$dmat <- 1 / (C_A50*(T1-EmStart$MeanT)/(EmStart$MeanT-T0))
  EmStart$pmat <- cumsum(EmStart$dmat)
  threshold <- 1
  
  if(nrow(subset(EmStart, pmat >= 1)) > 0) { # check if the threshold has yet been reached at all
    first_exceed_index <- which.max(EmStart$pmat > threshold)
    emergence_start_date <- EmStart$Date[first_exceed_index]
    Graph_Crit_Dates[1,7] <- emergence_start_date
  } 
  
  EmPeak <- subset(This_Year_Temps, Date > EH_hatch_peak_date)
  EmPeak$dmat <- 1 / (C_A50*(T1-EmPeak$MeanT)/(EmPeak$MeanT-T0))
  EmPeak$pmat <- cumsum(EmPeak$dmat)
  
  if(nrow(subset(EmPeak, pmat >= 1)) > 0) { # check if the threshold has yet been reached at all
    first_exceed_index <- which.max(EmPeak$pmat > threshold)
    emergence_peak_date <- EmPeak$Date[first_exceed_index]
    Graph_Crit_Dates[1,8] <- emergence_peak_date
  } 

  ## Return the final matrix of known critical dates
  
  return(Graph_Crit_Dates)
  
}



RT_model <- function(Temps, this_year) {
  
  ## Create single-row matrix to store critical dates from this water year
  
  RBT_CritDates = matrix(data=NA, nrow=1, ncol=17)
  colnames(RBT_CritDates) <- c("Year", "WinSpawnLen", "WinSpawnStart", 
                               "WinSpawnPeak", "WinSpawnEnd", "WinHatchStart", 
                               "WinHatchPeak", "WinEmergStart", "WinEmergPeak", 
                               "SprSpawnLen", "SprSpawnStart", "SprSpawnPeak", 
                               "SprSpawnEnd", "SprHatchStart", "SprHatchPeak",
                               "SprEmergStart", "SprEmergPeak")
  RBT_CritDates <- as.data.frame(RBT_CritDates)
  RBT_CritDates$WinSpawnStart <- as.Date(RBT_CritDates$WinSpawnStart)
  RBT_CritDates$WinSpawnPeak <- as.Date(RBT_CritDates$WinSpawnPeak)
  RBT_CritDates$WinSpawnEnd <- as.Date(RBT_CritDates$WinSpawnEnd)
  RBT_CritDates$WinHatchStart <- as.Date(RBT_CritDates$WinHatchStart)
  RBT_CritDates$WinHatchPeak <- as.Date(RBT_CritDates$WinHatchPeak)
  RBT_CritDates$WinEmergStart <- as.Date(RBT_CritDates$WinEmergStart)
  RBT_CritDates$WinEmergPeak <- as.Date(RBT_CritDates$WinEmergPeak)
  RBT_CritDates$SprSpawnStart <- as.Date(RBT_CritDates$SprSpawnStart)
  RBT_CritDates$SprSpawnPeak <- as.Date(RBT_CritDates$SprSpawnPeak)
  RBT_CritDates$SprSpawnEnd <- as.Date(RBT_CritDates$SprSpawnEnd)
  RBT_CritDates$SprHatchStart <- as.Date(RBT_CritDates$SprHatchStart)
  RBT_CritDates$SprHatchPeak <- as.Date(RBT_CritDates$SprHatchPeak)
  RBT_CritDates$SprEmergStart <- as.Date(RBT_CritDates$SprEmergStart)
  RBT_CritDates$SprEmergPeak <- as.Date(RBT_CritDates$SprEmergPeak)

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
  
  RBT_CritDates[1,1] <- this_year
  
  ## Parameters for Elliott and Hurley 1998 model
  ## The EH model is not as well supported for rainbow trout.
  ## The Zeug et al linear model works well, and is preferred.
  ## EH model could be retained as an option.
  
  # T0 <- -2.487
  # T1 <- 22.403
  # C_A50 <- 36.35
  # C_H50 <- 39.86
  
  #
  # WINTER
  #
  
  # Determine length of winter spawn
  
  S1 <- subset(Temps, Date > WinterSpawnStart)
  S1 <- subset(S1, Date < WinterSpawnEnd)
  S1 <- subset(S1, MeanT < 9)
  S1 <- subset(S1, MeanT > 6)
  
  WinSpawnLen <- length(S1$Date)
  RBT_CritDates[1,2] <- WinSpawnLen
  
  # If there is a winter spawn, calculate and store dates
  if(WinSpawnLen > 0) { 

    WinSpawnStart <- min(S1$Date, na.rm = TRUE)
    WinSpawnEnd <- max(S1$Date, na.rm=TRUE)
    WinSpawnAvg <- mean(S1$Date)
    RBT_CritDates[1,3] <- WinSpawnStart
    RBT_CritDates[1,4] <- WinSpawnAvg
    RBT_CritDates[1,5] <- WinSpawnEnd
    

    ## Calculate hatch /emergence for winter spawned embryos. The  
    ## linear model applied below mimics the approach used in the 
    ## embryo development model by Zeug et al 2012.  The emergence
    ## model is parameterized with data from Murray et al 1980.
    
    # Determine winter hatch start date
    HaWinStart <- subset(Temps, Date > WinSpawnStart)
    HaWinStart$dmatZ <- 0.0032*HaWinStart$MeanT+0.003
    HaWinStart$pmatZ <- cumsum(HaWinStart$dmatZ)
    
    threshold <- 1
    
    # Find the first index where "Value" exceeds the threshold
    if(nrow(subset(HaWinStart, pmatZ >= 1)) > 0) {
      Z_Win_hatch_start_first_exceed_index <- which.max(HaWinStart$pmatZ > threshold)
      Z_win_hatch_start_date <- HaWinStart$Date[Z_Win_hatch_start_first_exceed_index]
      RBT_CritDates[1,6] <- Z_win_hatch_start_date
      bool_win_hatch_start <- TRUE
    } else {
      bool_win_hatch_start <- FALSE
    }
    

    # Determine winter hatch peak date
    HaWinPeak <- subset(Temps, Date > WinSpawnAvg)
    HaWinPeak$dmatZ <- 0.0032*HaWinPeak$MeanT+0.003
    HaWinPeak$pmatZ <- cumsum(HaWinPeak$dmatZ)
    
    threshold <- 1
    
    # Find the first index where "Value" exceeds the threshold
    if(nrow(subset(HaWinPeak, pmatZ >= 1)) > 0) {
      Z_Win_hatch_peak_first_exceed_index <- which.max(HaWinPeak$pmatZ > threshold)
      Z_win_hatch_peak_date <- HaWinPeak$Date[Z_Win_hatch_peak_first_exceed_index]
      RBT_CritDates[1,7] <- Z_win_hatch_peak_date
      bool_win_hatch_peak <- TRUE
    } else {
      bool_win_hatch_peak <- FALSE
    }
    
    # Determine emergence start dates IFF hatch start dates have been calculated
    if(bool_win_hatch_start) {
      EmWinStart <- subset(Temps, Date > Z_win_hatch_start_date)
      EmWinStart$dmatZ <- 0.0056*EmWinStart$MeanT-0.0045
      EmWinStart$pmatZ <- cumsum(EmWinStart$dmatZ)
      if(nrow(subset(EmWinStart, pmatZ >= 1)) > 0) {
        em_win_start_first_exceed_index <- which.max(EmWinStart$pmatZ > threshold)
        emergence_Win_start_date <- EmWinStart$Date[em_win_start_first_exceed_index]
        RBT_CritDates[1,8] <- emergence_Win_start_date
      }
    }
    
    # Determine emergence peak dates IFF hatch peak dates have been calculated
    if(bool_win_hatch_peak) {
      EmWinPeak <- subset(Temps, Date > Z_win_hatch_peak_date)
      EmWinPeak$dmatZ <- 0.0056*EmWinPeak$MeanT-0.0045
      EmWinPeak$pmatZ <- cumsum(EmWinPeak$dmatZ)
      if(nrow(subset(EmWinPeak, pmatZ >= 1)) > 0) {
        em_win_peak_first_exceed_index <- which.max(EmWinPeak$pmatZ > threshold)
        emergence_Win_peak_date <- EmWinPeak$Date[em_win_peak_first_exceed_index]
        RBT_CritDates[1,9] <- emergence_Win_peak_date
      }
    }
  }
  
  #
  # SPRING
  #
  
  # Determine length of Spring spawn
  
  S1 <- subset(Temps, Date > SpringSpawnStart)
  S1 <- subset(S1, Date < SpringEnd)
  S1 <- subset(S1, MeanT < 9)
  S1 <- subset(S1, MeanT > 6)
  
  SpringSpawnLen <- length(S1$Date)
  RBT_CritDates[1,10] <- SpringSpawnLen
  
  if(SpringSpawnLen > 0) { # If there is a Spring spawn, calculate and store dates
    SprSpawnStart <- min(S1$Date, na.rm = TRUE)
    SprSpawnEnd <- max(S1$Date, na.rm=TRUE)
    SprSpawnAvg <- mean(S1$Date)
    RBT_CritDates[1,11] <- SprSpawnStart
    RBT_CritDates[1,12] <- SprSpawnAvg
    RBT_CritDates[1,13] <- SprSpawnEnd
    
    ## Calculate hatch /emergence for Spring spawned embryos. The  
    ## linear model applied below mimics the approach used in the 
    ## embryo development model by Zeug et al 2012.  The emergence
    ## model is parameterized with data from Murray et al 1980.
    
    # Determine Spring hatch start date
    HaSprStart <- subset(Temps, Date > SprSpawnStart)
    HaSprStart$dmatZ <- 0.0032*HaSprStart$MeanT+0.003
    HaSprStart$pmatZ <- cumsum(HaSprStart$dmatZ)
    
    threshold <- 1
    
    # Find the first index where "Value" exceeds the threshold
    if(nrow(subset(HaSprStart, pmatZ >= 1)) > 0) {
      Z_spr_hatch_start_first_exceed_index <- which.max(HaSprStart$pmatZ > threshold)
      Z_spr_hatch_start_date <- HaSprStart$Date[Z_spr_hatch_start_first_exceed_index]
      RBT_CritDates[1,14] <- Z_spr_hatch_start_date
      bool_spr_hatch_start <- TRUE
    } else {
      bool_spr_hatch_start <- FALSE
    }
    
    # Determine spring hatch peak date
    HaSprPeak <- subset(Temps, Date > SprSpawnAvg)
    HaSprPeak$dmatZ <- 0.0032*HaSprPeak$MeanT+0.003
    HaSprPeak$pmatZ <- cumsum(HaSprPeak$dmatZ)
    
    threshold <- 1
    
    # Find the first index where "Value" exceeds the threshold
    if(nrow(subset(HaSprPeak, pmatZ >= 1)) > 0) {
      Z_spr_hatch_peak_first_exceed_index <- which.max(HaSprPeak$pmatZ > threshold)
      Z_spr_hatch_peak_date <- HaSprPeak$Date[Z_spr_hatch_peak_first_exceed_index]
      RBT_CritDates[1,15] <- Z_spr_hatch_peak_date
      bool_spr_hatch_peak <- TRUE
    } else {
      bool_spr_hatch_peak <- FALSE
    }
    
    # Determine emergence start dates IFF hatch start dates have been calculated
    if(bool_spr_hatch_start) {
      EmSprStart <- subset(Temps, Date > Z_spr_hatch_start_date)
      EmSprStart$dmatZ <- 0.0056*EmSprStart$MeanT-0.0045
      EmSprStart$pmatZ <- cumsum(EmSprStart$dmatZ)
      if(nrow(subset(EmSprStart, pmatZ >= 1)) > 0) {
        em_spr_start_first_exceed_index <- which.max(EmSprStart$pmatZ > threshold)
        emergence_spr_start_date <- EmSprStart$Date[em_spr_start_first_exceed_index]
        RBT_CritDates[1,16] <- emergence_spr_start_date
      }
    }
    
    # Determine emergence peak dates IFF hatch peak dates have been calculated
    if(bool_spr_hatch_peak) {
      EmSprPeak <- subset(Temps, Date > Z_spr_hatch_peak_date)
      EmSprPeak$dmatZ <- 0.0056*EmSprPeak$MeanT-0.0045
      EmSprPeak$pmatZ <- cumsum(EmSprPeak$dmatZ)
      if(nrow(subset(EmSprPeak, pmatZ >= 1)) > 0) {
        em_spr_peak_first_exceed_index <- which.max(EmSprPeak$pmatZ > threshold)
        emergence_spr_peak_date <- EmSprPeak$Date[em_spr_peak_first_exceed_index]
        RBT_CritDates[1,17] <- emergence_spr_peak_date
      }
    }
  }
  

  return(RBT_CritDates)
  
}







