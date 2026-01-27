
#
# This module takes in data and returns a plot
#


### This function builds a plot depicting Brown Trout model data
BT_graph <- function(This_Year_Temps, This_Year_Crit_Dates) {
  
  ## Scale the discharge information to the same scale/Y-axis as temperature
  coeff <- 6000/20
  This_Year_Temps$scaled_dis <- This_Year_Temps$maxD/coeff
  x1 <- min(This_Year_Temps$Date, na.rm=TRUE)
  x2 <- max(This_Year_Temps$Date, na.rm=TRUE)
  
  ## Get critical dates
  FSp_Start <- This_Year_Crit_Dates$SpawnStart[[1]]
  FSp_Peak <- This_Year_Crit_Dates$SpawnPeak[[1]]
  FSp_End <- This_Year_Crit_Dates$SpawnEnd[[1]]
  EH_hatch_start_date <- This_Year_Crit_Dates$HaStart[[1]]
  EH_hatch_peak_date <- This_Year_Crit_Dates$HaPeak[[1]]
  emergence_start_date <- This_Year_Crit_Dates$EmStart[[1]]
  emergence_peak_date <- This_Year_Crit_Dates$EmPeak[[1]]
  
  # Get today's date
  today_date <- as.Date(Sys.time(), tz = "America/New_York")
  
  ## Build graph
  TDPlot <- ggplot(data = This_Year_Temps, aes(x=Date))
  
  # show ideal temperature window 
  #TDPlot <- TDPlot +
  #  annotate('rect', xmin=This_Year_Temps$Date[[1]], xmax=This_Year_Temps$Date[[nrow(This_Year_Temps)]], ymin=6, ymax=12, alpha=.2, fill='green') +
  #  annotate(geom="text", x=emergence_peak_date+50, y=11, label="Ideal Temperature Range",
  #           color="darkgreen", angle=0, size = 6)
  
  # show spawn window
  if(!is.na(FSp_Start) && !is.na(FSp_End)) { # if we have known spawn start/end
    TDPlot <- TDPlot +
      annotate('rect', xmin=FSp_Start,
               xmax=FSp_End, ymin=0, ymax=Inf,
               alpha = .6, fill="#2c7fb8") +
      annotate(geom="text", x=FSp_Start, y=20, label="Spawn",
               color="black", angle=0) 
  } 
  
  
  # show hatch window
  if(!is.na(EH_hatch_start_date) && !is.na(EH_hatch_peak_date)) { # if we have known hatch start/peak
    TDPlot <- TDPlot +
      annotate('rect', xmin=EH_hatch_start_date,
               xmax=EH_hatch_peak_date+7, ymin=0, ymax=Inf,
               alpha = .7, fill="#7fcdbb") +
      annotate(geom="text", x=EH_hatch_start_date, y=20, label="Hatch",
               color="black", angle=0)
  } else if(!is.na(EH_hatch_start_date) && is.na(EH_hatch_peak_date)) { # if we have hatch start but NOT peak
    TDPlot <- TDPlot +
      annotate('rect', xmin=EH_hatch_start_date,
               xmax=today_date, ymin=0, ymax=Inf,
               alpha = .7, fill="#7fcdbb") +
      annotate(geom="text", x=EH_hatch_start_date, y=20, label="Hatch",
               color="black", angle=0)
  }
  
  
  # show emergence window
  if(!is.na(emergence_start_date) && !is.na(emergence_peak_date)) { # if we have known emerg start/peak
    TDPlot <- TDPlot +
      annotate('rect', xmin=emergence_start_date,
               xmax=emergence_peak_date+7, ymin=0, ymax=Inf,
               alpha = .7, fill="#edf8b1") +
      annotate(geom="text", x=emergence_start_date, y=20, label="Emergence",
               color="black", angle=0) 
  } else if(!is.na(emergence_start_date) && is.na(emergence_peak_date)) { # if we have emerg start but NOT peak
    TDPlot <- TDPlot +
      annotate('rect', xmin=emergence_start_date,
               xmax=today_date, ymin=0, ymax=Inf,
               alpha = .7, fill="#edf8b1") +
      annotate(geom="text", x=emergence_start_date, y=20, label="Emergence",
               color="black", angle=0) 
  }
  
  # Plot temperature and max daily discharge
  TDPlot <- TDPlot +
    geom_line( aes(y=MeanT, color = "Line1"), linewidth=1) +
    geom_line( aes(y=scaled_dis, color = "Line2"), linewidth=0.5) 
  
  # add second axis, label all axes
  TDPlot <- TDPlot +
    xlab("Month") +
    scale_x_date(date_breaks="1 month", date_labels="%b", expand = c(0, 0)) +
    scale_y_continuous(
      expand = c(0, 0),
      limits = c(0, 22),
      breaks=c(0,5, 10, 15, 20),
      name = "Temperature (C°)",
      # Second axis
      sec.axis = sec_axis(~.*coeff, 
                          name="Max Daily Discharge (cfs)",
                          breaks=c(0,1500, 3000, 4500, 6000))
    )
    
  # adjust aesthetics - axis labels, ticks, colors, size ; add today's date
  TDPlot <- TDPlot +
    theme_bw() +
    theme(
      axis.text = element_text(size = 13),
      axis.title = element_text(size = 14),
      axis.title.y = element_text(color = "blue"),
      axis.text.y = element_text(colour = "blue"),
      axis.title.y.right = element_text(color = "black"),
      axis.text.y.right = element_text(color = "black"),
    ) 
  
  # Today's date (replace year with correct historical year when necessary)
  if(today_date %in% This_Year_Temps$Date) {
    TDPlot <- TDPlot +
      geom_vline(aes(xintercept = today_date, color = "Line3"), linetype="dashed", 
                linewidth=0.5)
  } else {
    if(format(today_date, "%m") > 9) { # oct, nov, dec
      historical_date <- as.Date(paste(format(This_Year_Temps$Date[1], "%Y"), format(today_date, "%m"), format(today_date, "%d"), sep = "-"))
    } else {
      historical_date <- as.Date(paste(format(This_Year_Temps$Date[360], "%Y"), format(today_date, "%m"), format(today_date, "%d"), sep = "-"))
    }
    TDPlot <- TDPlot +
      geom_vline(aes(xintercept = historical_date, color = "Line3"), linetype="dashed", 
                 linewidth=0.5)
  }
  
  # Legend
  TDPlot <- TDPlot +
    # Real legend elements corresponding with temp, discharge, and today's date
    scale_color_manual(
      values = c("blue", "darkgray", "black"),
      labels = c("Temperature", "Discharge", "Current month/day")
    ) +
    # CHEAT/HARDCODED legend elements
    geom_col(aes(y = numeric(nrow(This_Year_Temps)), fill = "Spawn"), alpha = .6) + 
    geom_col(aes(y = numeric(nrow(This_Year_Temps)), fill = "Hatch"), alpha = .7) + 
    geom_col(aes(y = numeric(nrow(This_Year_Temps)), fill = "Emergence"), alpha = .7) + 
    scale_fill_manual(
      values = c("#2c7fb8", "#7fcdbb", "#edf8b1"),
      labels = c("Predicted Spawn Window", "Predicted Hatch Window", "Predicted Emergence Window")
    ) + 
    theme(legend.title = element_blank()) + 
    theme(legend.text = element_text(size = 13))
  
  return(TDPlot)
}


### This function builds a plot depicting Brown Trout model data
RT_graph <- function(This_Year_Temps, This_Year_Crit_Dates) {
  
  ## Scale the discharge
  coeff <- 6000/20
  This_Year_Temps$scaled_dis <- This_Year_Temps$maxD/coeff
  x1 <- min(This_Year_Temps$Date, na.rm=TRUE)
  x2 <- max(This_Year_Temps$Date, na.rm=TRUE)

  ## Get key dates
  NoWinSpawn <- as.Date(paste(toString(This_Year_Crit_Dates$Year[1] - 1), "-12-14", sep=""))
  WinSpawnLen <- This_Year_Crit_Dates$WinSpawnLen[1]
  SprSpawnLen <- This_Year_Crit_Dates$SprSpawnLen[1]
  
  WinSpawnStart <- This_Year_Crit_Dates$WinSpawnStart[1]
  WinSpawnPeak <- This_Year_Crit_Dates$WinSpawnPeak[1]
  WinSpawnEnd <- This_Year_Crit_Dates$WinSpawnEnd[1]
  WinHatchStart <- This_Year_Crit_Dates$WinHatchStart[1]
  WinHatchPeak <- This_Year_Crit_Dates$WinHatchPeak[1]
  WinEmergStart <- This_Year_Crit_Dates$WinEmergStart[1]
  WinEmergPeak <- This_Year_Crit_Dates$WinEmergPeak[1]
  SprSpawnStart <- This_Year_Crit_Dates$SprSpawnStart[1]
  SprSpawnPeak <- This_Year_Crit_Dates$SprSpawnPeak[1]
  SprSpawnEnd <- This_Year_Crit_Dates$SprSpawnEnd[1]
  SprHatchStart <- This_Year_Crit_Dates$SprHatchStart[1]
  SprHatchPeak <- This_Year_Crit_Dates$SprHatchPeak[1]
  SprEmergStart <- This_Year_Crit_Dates$SprEmergStart[1]
  SprEmergPeak <- This_Year_Crit_Dates$SprEmergPeak[1]
  
  #SprSpawnAvg <- This_Year_Crit_Dates$SprSpawn[1]
  #Z_hatch_Spr_date <- This_Year_Crit_Dates$SprHatch[1]
  #emergence_Spr_date <- This_Year_Crit_Dates$SprEmerg[1]
  #WinSpawnAvg <- This_Year_Crit_Dates$WinSpawn[1]
  #Z_hatch_Win_date <- This_Year_Crit_Dates$WinHatch[1]
  #emergence_Win_date <- This_Year_Crit_Dates$WinEmerg[1]
  
  
  ## DELETE all interpolated temp values after today's date
  today_date <- as.Date(Sys.time(), tz = "America/New_York")
  #for(i in 1:nrow(This_Year_Temps)) {
  #  if (This_Year_Temps$Date[[i]] > today_date) {
  #    This_Year_Temps$meanT.y[[i]] <- NA
  #    This_Year_Temps$meanT.x[[i]] <- NA
  #    This_Year_Temps$MeanT[[i]] <- NA
  #  }
  #}
  
  ## Build graph
  TDPlot <- ggplot(data = This_Year_Temps, aes(x=Date))
  
  ## show spawn window
  TDPlot <- TDPlot +
    annotate('rect', xmin=SprSpawnStart,
             xmax=SprSpawnEnd, ymin=11, ymax=Inf,
             alpha = .6, fill="#2c7fb8") +
    annotate(geom="text", x=SprSpawnStart, y=21, label="Spawn",
             color="black", angle=0) 
  if(This_Year_Crit_Dates$WinSpawnLen[1] == 0) {
    TDPlot <- TDPlot +
    annotate(geom="text", x=NoWinSpawn, y=18, label="No Winter Spawn",
             color="darkred", angle=90)
  } else {
    TDPlot <- TDPlot +
    annotate('rect', xmin=WinSpawnStart,
             xmax=WinSpawnEnd, ymin=0, ymax=11,
             alpha = .6, fill="#2c7fb8") +
    annotate(geom="text", x=WinSpawnStart, y=3, label="Spawn",
             color="black", angle=0) 
  }
  
  
  # show hatch window
  if(!is.na(SprHatchStart) && is.na(SprHatchPeak)) { # if spr hatch start known but peak unknown
    TDPlot <- TDPlot +
      annotate('rect', xmin=SprHatchStart,
               xmax=today_date, ymin=11, ymax=Inf,
               alpha = .7, fill="#7fcdbb") +
      annotate(geom="text", x=SprHatchStart, y=20, label="Hatch",
               color="black", angle=0)
  } else { # BOTH start and peak known OR NEITHER
    TDPlot <- TDPlot +
      annotate('rect', xmin=SprHatchStart,
               xmax=SprHatchPeak+7, ymin=11, ymax=Inf,
               alpha = .7, fill="#7fcdbb") +
      annotate(geom="text", x=SprHatchStart, y=20, label="Hatch",
               color="black", angle=0)
  }
  if(!is.na(WinHatchStart) && is.na(WinHatchPeak)) { # if win hatch start known but peak unknown
    TDPlot <- TDPlot +
      annotate('rect', xmin=WinHatchStart,
            xmax=today_date, ymin=0, ymax=11,
             alpha = .7, fill="#7fcdbb") +
      annotate(geom="text", x=WinHatchStart, y=2, label="Hatch",
               color="black", angle=0)
  } else { # BOTH start and peak known OR NEITHER
    TDPlot <- TDPlot +
      annotate('rect', xmin=WinHatchStart,
               xmax=WinHatchPeak+7, ymin=0, ymax=11,
               alpha = .7, fill="#7fcdbb") +
      annotate(geom="text", x=WinHatchStart, y=2, label="Hatch",
               color="black", angle=0)
  }
  
  # show emergence window
  if(!is.na(SprEmergStart) && is.na(SprEmergPeak)) { # if spr emerg start known but peak unknown
    TDPlot <- TDPlot +
      annotate('rect', xmin=SprEmergStart,
               xmax=today_date, ymin=11, ymax=Inf,
               alpha = .7, fill="#edf8b1") +
      annotate(geom="text", x=SprHatchStart, y=19, label="Hatch",
               color="black", angle=0)
  } else { # BOTH start and peak known OR NEITHER
    TDPlot <- TDPlot +
      annotate('rect', xmin=SprEmergStart,
               xmax=SprEmergPeak+7, ymin=11, ymax=Inf,
               alpha = .7, fill="#edf8b1") +
      annotate(geom="text", x=SprHatchStart, y=19, label="Hatch",
               color="black", angle=0)
  }
  if(!is.na(WinEmergStart) && is.na(WinEmergPeak)) { # if win emerg start known but peak unknown
    TDPlot <- TDPlot +
      annotate('rect', xmin=WinEmergStart,
               xmax=today_date, ymin=0, ymax=11,
               alpha = .7, fill="#edf8b1") +
      annotate(geom="text", x=WinHatchStart, y=1, label="Hatch",
               color="black", angle=0)
  } else { # BOTH start and peak known OR NEITHER
    TDPlot <- TDPlot +
      annotate('rect', xmin=WinEmergStart,
               xmax=WinEmergPeak+7, ymin=0, ymax=11,
               alpha = .7, fill="#edf8b1") +
      annotate(geom="text", x=WinHatchStart, y=1, label="Hatch",
               color="black", angle=0)
  }
  
  # Plot temperature and max daily discharge
  TDPlot <- TDPlot +
    geom_line( aes(y=MeanT, color = "Line1"), linewidth=1) +
    geom_line( aes(y=scaled_dis, color = "Line2"), linewidth=0.5) 
  
  # add second axis, label all axes
  TDPlot <- TDPlot +
    xlab("Month") +
    scale_x_date(date_breaks="1 month", date_labels="%b", expand = c(0, 0)) +
    scale_y_continuous(
      expand = c(0, 0),
      limits = c(0, 22),
      breaks=c(0,5, 10, 15, 20),
      name = "Temperature (C°)",
      # Second axis
      sec.axis = sec_axis(~.*coeff, 
                          name="Max Daily Discharge (cfs)",
                          breaks=c(0,1500, 3000, 4500, 6000))
    )
  
  # adjust aesthetics - axis labels, ticks, colors, size ; add today's date
  TDPlot <- TDPlot +
    theme_bw() +
    theme(
      axis.text = element_text(size = 13),
      axis.title = element_text(size = 14),
      axis.title.y = element_text(color = "blue"),
      axis.text.y = element_text(colour = "blue"),
      axis.title.y.right = element_text(color = "black"),
      axis.text.y.right = element_text(color = "black"),
    ) 
  
  # Today's date (replace year with correct historical year when necessary)
  if(today_date %in% This_Year_Temps$Date) {
    TDPlot <- TDPlot +
      geom_vline(aes(xintercept = today_date, color = "Line3"), linetype="dashed", 
                 linewidth=0.5)
  } else {
    if(format(today_date, "%m") > 9) { # oct, nov, dec
      historical_date <- as.Date(paste(format(This_Year_Temps$Date[1], "%Y"), format(today_date, "%m"), format(today_date, "%d"), sep = "-"))
    } else {
      historical_date <- as.Date(paste(format(This_Year_Temps$Date[360], "%Y"), format(today_date, "%m"), format(today_date, "%d"), sep = "-"))
    }
    TDPlot <- TDPlot +
      geom_vline(aes(xintercept = historical_date, color = "Line3"), linetype="dashed", 
                 linewidth=0.5)
  }
  
  # Legend
  TDPlot <- TDPlot +
    # Real legend elements corresponding with temp, discharge, and today's date
    scale_color_manual(
      values = c("blue", "darkgray", "black"),
      labels = c("Temperature", "Discharge", "Current month/day")
    ) +
    # CHEAT/HARDCODED legend elements
    geom_col(aes(y = numeric(nrow(This_Year_Temps)), fill = "Spawn"), alpha = .6) + 
    geom_col(aes(y = numeric(nrow(This_Year_Temps)), fill = "Hatch"), alpha = .7) + 
    geom_col(aes(y = numeric(nrow(This_Year_Temps)), fill = "Emergence"), alpha = .7) + 
    scale_fill_manual(
      values = c("#2c7fb8", "#7fcdbb", "#edf8b1"),
      labels = c("Predicted Spawn Window", "Predicted Hatch Window", "Predicted Emergence Window")
    ) + 
    theme(legend.title = element_blank()) + 
    theme(legend.text = element_text(size = 13))
  
  ## Return plot
  return(TDPlot)
  
}


### This function chooses which graph to display based on species input 
show_graph <- function(species, This_Year_Temps, This_Year_Crit_Dates) {
  
  if(species == "Brown") {
    return(BT_graph(This_Year_Temps, This_Year_Crit_Dates))
  } else if(species == "Rainbow") {
    return(RT_graph(This_Year_Temps, This_Year_Crit_Dates))
  } else {
    return(NULL)
  }
}


### This function builds a text summary of the graph elements 
show_summary <- function(current_year, species, This_Year_Temps, This_Year_Crit_Dates) {
  
  today_date <- as.Date(Sys.time(), tz = "America/New_York")
  
  ## Brown trout summary 
  if(species == "Brown") {

    ## Check if currently in ideal spawn conditions: in possible date range AND in ideal temp range
    ## Must be WITHIN Oct. 1 - Dec. 31 AND within 6 degrees and 12 degrees
    if(
      as.integer(format(today_date, "%m")) >= 10 && 
      This_Year_Temps$MeanT[This_Year_Temps$Date == today_date] > 6 && 
      This_Year_Temps$MeanT[This_Year_Temps$Date == today_date] < 12
    ) {
      line1 <- "<p>Today's water temperature is <span style='color:red;'>within</span> the temperature range for Brown Trout spawning. Spawn activity is possible.</p>"
    } else {
      line1 <- ""
    }
    
    
    ## Check if in window from spawn to emergence
    ## spawn must have at least STARTED, and emergence peak not yet known OR is today
    if(
      !is.na(This_Year_Crit_Dates$SpawnStart[[1]]) &&
      (is.na(This_Year_Crit_Dates$EmPeak[[1]]) || This_Year_Crit_Dates$EmPeak[[1]] == today_date)
    ) {
      line2 <- "<p>Brown trout embryos are growing in the gravel. Avoid wading on spawning gravels.</p>"
    } else {
      line2 <- ""
    }
    
    
    ## check if water temps critically high
    
    if(This_Year_Temps$MeanT[This_Year_Temps$Date == today_date] > 20) { 
      line3 <- "<p>Due to critically high water temperatures, fish are under stress and may be easily injured. Consider targeting other rivers.</p>"
    } else {
      line3 <- ""
    }
    
    
    ret <- paste(line1, line2, line3, sep="")
    return (ret)
    
  }
  
  
  ## Rainbow trout summary 
  if(species == "Rainbow") {
    
    ## Check if currently in ideal spawn conditions: in possible date range AND in ideal temp range
    ## Winter: Must be WITHIN Dec 15 - Jan 15 AND within 6 degrees and 9 degrees
    if(
      (
        (as.integer(format(today_date, "%m")) == 12 && as.integer(format(today_date, "%d")) >= 15) || 
        (as.integer(format(today_date, "%m")) == 1 && as.integer(format(today_date, "%d")) <= 15)
      ) &&
      This_Year_Temps$MeanT[This_Year_Temps$Date == today_date] > 6 && 
      This_Year_Temps$MeanT[This_Year_Temps$Date == today_date] < 9
    ) {
      line1 <- "<p>Today's water temperature is <span style='color:red;'>within</span> the temperature range for Rainbow Trout spawning. Spawn activity is possible.</p>"
    } 
    ## Spring: Must be AFTER Jan 31 AND within 6 degrees and 9 degrees
    else if(
      as.integer(format(today_date, "%m")) >= 2 &&
      This_Year_Temps$MeanT[This_Year_Temps$Date == today_date] > 6 && 
      This_Year_Temps$MeanT[This_Year_Temps$Date == today_date] < 9
    ) {
      line1 <- "<p>Today's water temperature is <span style='color:red;'>within</span> the temperature range for Rainbow Trout spawning. Spawn activity is possible.</p>"
    }
    else {
      line1 <- ""
    }
    
    
    ## Check if in window from spawn to emergence
    ## spawn must have at least STARTED, and emergence peak not yet known OR is today
    ## Winter:
    if(
      !is.na(This_Year_Crit_Dates$WinSpawnStart[[1]]) &&
      (is.na(This_Year_Crit_Dates$WinEmergPeak[[1]]) || This_Year_Crit_Dates$WinEmergPeak[[1]] == today_date)
    ) {
      line2 <- "<p>Rainbow trout embryos are growing in the gravel. Avoid wading on spawning gravels.</p>"
    } 
    ## Spring:
    else if(
      !is.na(This_Year_Crit_Dates$SprSpawnStart[[1]]) &&
      (is.na(This_Year_Crit_Dates$SprEmergPeak[[1]]) || This_Year_Crit_Dates$SprEmergPeak[[1]] == today_date)
    ) {
      line2 <- "<p>Rainbow trout embryos are growing in the gravel. Avoid wading on spawning gravels.</p>"
    } else {
      line2 <- ""
    }
    
    
    ## check if water temps critically high
    
    if(This_Year_Temps$MeanT[This_Year_Temps$Date == today_date] > 20) { 
      line3 <- "<p>Due to critically high water temperatures, fish are under stress and may be easily injured. Consider targeting other rivers.</p>"
    } else {
      line3 <- ""
    }
    
    
    ret <- paste(line1, line2, line3, sep="")
    return (ret)
    
    
  }
  
}
