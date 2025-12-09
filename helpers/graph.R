
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
  for(i in 1:nrow(This_Year_Temps)) {
    if (This_Year_Temps$Date[[i]] > today_date) {
      This_Year_Temps$meanT.y[[i]] <- NA
      This_Year_Temps$meanT.x[[i]] <- NA
      This_Year_Temps$MeanT[[i]] <- NA
    }
  }
  
  ## Build graph
  
  # Plot temperature and max daily discharge
  TDPlot <- ggplot(data = This_Year_Temps, aes(x=Date)) +
    geom_line( aes(y=MeanT, color = "Line1"), linewidth=1) +
    geom_line( aes(y=scaled_dis, color = "Line2"), linewidth=0.5) 
  
  # show ideal temperature window 
  TDPlot <- TDPlot +
    annotate('rect', xmin=This_Year_Temps$Date[[1]], xmax=This_Year_Temps$Date[[nrow(This_Year_Temps)]], ymin=6, ymax=12, alpha=.2, fill='green') +
    annotate(geom="text", x=emergence_peak_date+50, y=11, label="Ideal Temperature Range",
             color="darkgreen", angle=0, size = 6)
  
  # show spawn window
  TDPlot <- TDPlot +
    annotate('rect', xmin=FSp_Start,
             xmax=FSp_End, ymin=0, ymax=Inf,
             alpha = .2, fill="red") +
    annotate(geom="text", x=FSp_Start, y=20, label="Predicted \nSpawn \nWindow",
             color="darkred", angle=0) 
  
  # show hatch window
  TDPlot <- TDPlot +
    annotate('rect', xmin=EH_hatch_start_date,
             xmax=EH_hatch_peak_date+7, ymin=0, ymax=Inf,
             alpha = .2, fill="red") +
    annotate(geom="text", x=EH_hatch_start_date, y=20, label="Predicted \nHatch \nWindow",
             color="darkred", angle=0)
  
  # show emergence window
  TDPlot <- TDPlot +
    annotate('rect', xmin=emergence_start_date,
             xmax=emergence_peak_date+7, ymin=0, ymax=Inf,
             alpha = .2, fill="red") +
    annotate(geom="text", x=emergence_start_date, y=20, label="Predicted \nEmergence \nWindow",
             color="darkred", angle=0) 
  
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
    ) + 
    geom_vline(aes(xintercept = today_date, color = "Line3"), linetype="dashed", 
               linewidth=0.5)
  
  temp_data = data.frame(Date = today_date, MeanT = 10)
  print(temp_data)
  
  # Legend
  TDPlot <- TDPlot +
    # Real legend elements corresponding with temp, discharge, and today's date
    scale_color_manual(
      values = c("blue", "darkgray", "black"),
      labels = c("Temperature", "Discharge", "Today's Date")
    ) +
    # CHEAT/HARDCODED legend elements
    geom_col(aes(y = numeric(nrow(This_Year_Temps)), fill = "Spawn"), alpha = .2) + 
    geom_col(aes(y = numeric(nrow(This_Year_Temps)), fill = "Hatch"), alpha = .2) + 
    geom_col(aes(y = numeric(nrow(This_Year_Temps)), fill = "Emergence"), alpha = .2) + 
    geom_col(aes(y = numeric(nrow(This_Year_Temps)), fill = "Temp"), alpha = .2) + 
    scale_fill_manual(
      values = c("red", "red", "red", "green"),
      labels = c("Predicted Spawn Window", "Predicted Hatch Window", "Predicted Emergence Window", "Ideal Temperature Range")
    ) + 
    theme(legend.title = element_blank())
  
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
  SpringSpawnAvg <- This_Year_Crit_Dates$SprSpawn[1]
  Z_hatch_Spr_date <- This_Year_Crit_Dates$SprHatch[1]
  emergence_Spr_date <- This_Year_Crit_Dates$SprEmerg[1]
  WinSpawnAvg <- This_Year_Crit_Dates$WinSpawn[1]
  Z_hatch_Win_date <- This_Year_Crit_Dates$WinHatch[1]
  emergence_Win_date <- This_Year_Crit_Dates$WinEmerg[1]

  ## Plotting for years with no winter spawn.
  if(is.na(This_Year_Crit_Dates$WinSpawnLen[1])) {
    TDPlot <- ggplot(data = This_Year_Temps, aes(x=Date)) +
      geom_line( aes(y=MeanT), linewidth=1, color="blue") +
      geom_line( aes(y=scaled_dis), linewidth=0.5, color="black") +
      annotate(geom="text", x=NoWinSpawn, y=18, label="No Winter Spawn",
               color="darkred", angle=90) +
      geom_vline(xintercept=SpringSpawnAvg, linetype="dashed", 
                 color = "green", linewidth=0.5) +
      geom_vline(xintercept=Z_hatch_Spr_date, linetype="dashed", 
                 color = "green", linewidth=0.5) +
      geom_vline(xintercept=emergence_Spr_date, linetype="dashed", 
                 color = "green", linewidth=0.5) +
      annotate(geom="text", x=SpringSpawnAvg+4, y=18, label="Sp Spawn Avg",
               color="darkgreen", angle=90) +
      annotate(geom="text", x=Z_hatch_Spr_date+2, y=18, label="Sp Hatching",
               color="darkgreen", angle=90) +
      annotate(geom="text", x=emergence_Spr_date+2, y=18, label="Sp Emerging",
               color="darkgreen", angle=90) +
      ylim(0,25) +
      xlab("Month") +
      scale_x_date(date_breaks="1 month", date_labels="%b", expand = c(0, 0)) +
      scale_y_continuous(
        
        # Features of the first axis
        name = "Temperature (C°)",
        
        # Add a second axis and specify its features
        sec.axis = sec_axis(~.*coeff, name="Max Daily Discharge (cfs)")
      )
    
  } 
  
  ## Plotting for years with winter spawn.
  else {
    TDPlot <- ggplot(data = This_Year_Temps, aes(x=Date)) +
      geom_line( aes(y=MeanT), linewidth=1, color="blue") +
      geom_line( aes(y=scaled_dis), linewidth=0.5, color="black") +
      geom_vline(xintercept=WinSpawnAvg, linetype="dashed", 
                 color = "red", linewidth=0.5) +
      geom_vline(xintercept=Z_hatch_Win_date, linetype="dashed", 
                 color = "red", linewidth=0.5) +
      geom_vline(xintercept=emergence_Win_date, linetype="dashed", 
                 color = "red", linewidth=0.5) +
      annotate(geom="text", x=WinSpawnAvg+4, y=19, label="Wn Spawn Avg",
               color="darkred", angle=90, size=3, hjust=1) +
      annotate(geom="text", x=Z_hatch_Win_date+4, y=19, label="Wn Hatching",
               color="darkred", angle=90, size=3, hjust=1) +
      annotate(geom="text", x=emergence_Win_date+4, y=19, label="Wn Emerging",
               color="darkred", angle=90, size=3, hjust=1) +
      geom_vline(xintercept=SpringSpawnAvg, linetype="dashed", 
                 color = "green", linewidth=0.5) +
      geom_vline(xintercept=Z_hatch_Spr_date, linetype="dashed", 
                 color = "green", linewidth=0.5) +
      geom_vline(xintercept=emergence_Spr_date, linetype="dashed", 
                 color = "green", linewidth=0.5) +
      annotate(geom="text", x=SpringSpawnAvg+4, y=19, label="Sp Spawn Avg",
               color="darkgreen", angle=90, size=3, hjust=1) +
      annotate(geom="text", x=Z_hatch_Spr_date+4, y=19, label="Sp Hatching",
               color="darkgreen", angle=90, size=3, hjust=1) +
      annotate(geom="text", x=emergence_Spr_date+4, y=19, label="Sp Emerging",
               color="darkgreen", angle=90, size=3, hjust=1) +
      ylim(0,25) +
      xlab("Month") +
      scale_x_date(date_breaks="1 month", date_labels="%b", expand = c(0, 0)) +
      scale_y_continuous(
        
        # Features of the first axis
        name = "Temperature (C°)",
        
        # Add a second axis and specify its features
        sec.axis = sec_axis(~.*coeff, name="mean Discharge cfs")
      )
  }
  
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

