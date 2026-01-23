
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
  
  # show ideal temperature window 
  #TDPlot <- TDPlot +
  #  annotate('rect', xmin=This_Year_Temps$Date[[1]], xmax=This_Year_Temps$Date[[nrow(This_Year_Temps)]], ymin=6, ymax=12, alpha=.2, fill='green') +
  #  annotate(geom="text", x=emergence_peak_date+50, y=11, label="Ideal Temperature Range",
  #           color="darkgreen", angle=0, size = 6)
  
  # show spawn window
  TDPlot <- TDPlot +
    annotate('rect', xmin=FSp_Start,
             xmax=FSp_End, ymin=0, ymax=Inf,
             alpha = .6, fill="#2c7fb8") +
    annotate(geom="text", x=FSp_Start, y=20, label="Spawn",
             color="black", angle=0) 
  
  # show hatch window
  TDPlot <- TDPlot +
    annotate('rect', xmin=EH_hatch_start_date,
             xmax=EH_hatch_peak_date+7, ymin=0, ymax=Inf,
             alpha = .7, fill="#7fcdbb") +
    annotate(geom="text", x=EH_hatch_start_date, y=20, label="Hatch",
             color="black", angle=0)
  
  # show emergence window
  TDPlot <- TDPlot +
    annotate('rect', xmin=emergence_start_date,
             xmax=emergence_peak_date+7, ymin=0, ymax=Inf,
             alpha = .7, fill="#edf8b1") +
    annotate(geom="text", x=emergence_start_date, y=20, label="Emergence",
             color="black", angle=0) 
  
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
  print(This_Year_Crit_Dates)
  
  ## Get key dates
  NoWinSpawn <- paste(toString(This_Year_Crit_Dates$Year[1] - 1), "-12-14", sep="")
  NoWinSpawn <- as.Date(NoWinSpawn)
  SprSpawnAvg <- This_Year_Crit_Dates$SprSpawn[1]
  Z_hatch_Spr_date <- This_Year_Crit_Dates$SprHatch[1]
  emergence_Spr_date <- This_Year_Crit_Dates$SprEmerg[1]
  WinSpawnAvg <- This_Year_Crit_Dates$WinSpawn[1]
  Z_hatch_Win_date <- This_Year_Crit_Dates$WinHatch[1]
  emergence_Win_date <- This_Year_Crit_Dates$WinEmerg[1]
  WinSpawnLen <- This_Year_Crit_Dates$WinSpawnLen[1]
  SprSpawnLen <- This_Year_Crit_Dates$SprSpawnLen[1]
  
  ## DELETE all interpolated temp values after today's date
  today_date <- as.Date(Sys.time(), tz = "America/New_York")
  for(i in 1:nrow(This_Year_Temps)) {
    if (This_Year_Temps$Date[[i]] > today_date) {
      This_Year_Temps$meanT.y[[i]] <- NA
      This_Year_Temps$meanT.x[[i]] <- NA
      This_Year_Temps$MeanT[[i]] <- NA
    }
  }
  
  ## Build graph
  TDPlot <- ggplot(data = This_Year_Temps, aes(x=Date))
  
  # show spawn window
  TDPlot <- TDPlot +
    annotate('rect', xmin=SprSpawnAvg,
             xmax=SprSpawnAvg+SprSpawnLen, ymin=11, ymax=Inf,
             alpha = .6, fill="#2c7fb8") +
    annotate(geom="text", x=SprSpawnAvg, y=21, label="Spawn",
             color="black", angle=0) 
  if(is.na(This_Year_Crit_Dates$WinSpawnLen[1])) {
    TDPlot <- TDPlot +
    annotate(geom="text", x=NoWinSpawn, y=18, label="No Winter Spawn",
             color="darkred", angle=90)
  } else {
    TDPlot <- TDPlot +
    annotate('rect', xmin=WinSpawnAvg,
             xmax=WinSpawnAvg+WinSpawnLen, ymin=0, ymax=11,
             alpha = .6, fill="#2c7fb8") +
    annotate(geom="text", x=WinSpawnAvg, y=3, label="Spawn",
             color="black", angle=0) 
  }
  
  
  # show hatch window
  TDPlot <- TDPlot +
    annotate('rect', xmin=Z_hatch_Spr_date,
             xmax=Z_hatch_Spr_date+20, ymin=11, ymax=Inf,
             alpha = .7, fill="#7fcdbb") +
    annotate(geom="text", x=Z_hatch_Spr_date, y=20, label="Hatch",
             color="black", angle=0)
  if(is.na(This_Year_Crit_Dates$WinSpawnLen[1]) == FALSE) {
    TDPlot <- TDPlot +
      annotate('rect', xmin=Z_hatch_Win_date,
            xmax=Z_hatch_Win_date+20, ymin=0, ymax=11,
             alpha = .7, fill="#7fcdbb") +
      annotate(geom="text", x=Z_hatch_Win_date, y=2, label="Hatch",
               color="black", angle=0)
  }
  
  # show emergence window
  TDPlot <- TDPlot +
    annotate('rect', xmin=emergence_Spr_date,
             xmax=emergence_Spr_date+20, ymin=11, ymax=Inf,
             alpha = .7, fill="#edf8b1") +
    annotate(geom="text", x=emergence_Spr_date, y=19, label="Emergence",
             color="black", angle=0) 
  if(is.na(This_Year_Crit_Dates$WinSpawnLen[1]) == FALSE) {
    TDPlot <- TDPlot +
      annotate('rect', xmin=emergence_Win_date,
               xmax=emergence_Win_date+20, ymin=0, ymax=11,
               alpha = .7, fill="#edf8b1") +
      annotate(geom="text", x=emergence_Win_date, y=1, label="Emergence",
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
  
  if(current_year == TRUE) {
    if(species == "Brown") {
      
      print(111)
      
      ## Check if currently in ideal water temperature range
      
      ret <- "<p>Today's water temperature is "
      if(
        This_Year_Temps$MeanT[This_Year_Temps$Date == today_date] > 6 && 
        This_Year_Temps$MeanT[This_Year_Temps$Date == today_date] < 12
        ) {
            
          ret <- paste(ret, "<span style='color:red;'>within</span> the ideal temperature range for <b>Brown Trout.</b>", sep="")
      }
      else {
        ret <- paste(ret, "<span style='color:green;'>not within</span> the ideal temperature range for <b>Brown Trout.</b>", sep="")
      }
      
      print(222)
      
      ## Check if within spawn window
      
      #FSp_Start <- This_Year_Crit_Dates$SpawnStart[[1]]
      #FSp_Peak <- This_Year_Crit_Dates$SpawnPeak[[1]]
      #FSp_End <- This_Year_Crit_Dates$SpawnEnd[[1]]
      #EH_hatch_start_date <- This_Year_Crit_Dates$HaStart[[1]]
      #EH_hatch_peak_date <- This_Year_Crit_Dates$HaPeak[[1]]
      #emergence_start_date <- This_Year_Crit_Dates$EmStart[[1]]
      #emergence_peak_date <- This_Year_Crit_Dates$EmPeak[[1]]
      
      if(
        This_Year_Crit_Dates$SpawnStart[[1]] >= today_date &&
        This_Year_Crit_Dates$SpawnEnd[[1]] <= today_date
         ) {
        ret <- paste(ret, " Today's date is <span>within</span> the predicted spawn window.", sep="")
      }
      else {
        ret <- paste(ret, " Today's date is <span>not within</span> the predicted spawn window.", sep="")
      }
      
      print(333)
      ret <- paste(ret, "</p>", sep="")
      return (ret)
    }
  }
}
