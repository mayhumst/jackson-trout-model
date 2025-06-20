

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

