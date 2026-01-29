
#
# This module takes in data and returns a plot
#


### This function builds a plot depicting Brown Trout model data
BT_graph <- function(This_Year_Temps, This_Year_Crit_Dates) {
  
  
  ## Scale the discharge information to the same scale/Y-axis as temperature
  
  coeff <- max(This_Year_Temps$maxD, na.rm=TRUE)/20
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
  
  ## Build graph
  
  TDPlot <- ggplot(data = This_Year_Temps, aes(x=Date)) +
    geom_line( aes(y=MeanT), linewidth=1, color="blue") +
    geom_line( aes(y=scaled_dis), linewidth=0.5, color="black") +
    annotate('rect', xmin=FSp_Start, xmax=FSp_End, ymin=6, ymax=8, alpha=.2, fill='red') +
    annotate('rect', xmin=FSp_Start, xmax=FSp_End, ymin=8, ymax=10, alpha=.2, fill='green') +
    annotate('rect', xmin=FSp_Start, xmax=FSp_End, ymin=10, ymax=12, alpha=.2, fill='red') +
    annotate('rect', xmin=EH_hatch_start_date,
             xmax=EH_hatch_peak_date, ymin=0, ymax=Inf,
             alpha = .2, fill="red") +
    annotate('rect', xmin=EH_hatch_peak_date,
             xmax=EH_hatch_peak_date+7, ymin=0, ymax=Inf,
             alpha = .2, fill="green") +
    annotate('rect', xmin=emergence_start_date,
             xmax=emergence_peak_date, ymin=0, ymax=Inf,
             alpha = .2, fill="red") +
    annotate('rect', xmin=emergence_peak_date,
             xmax=emergence_peak_date+7, ymin=0, ymax=Inf,
             alpha = .2, fill="green") +
    annotate(geom="text", x=EH_hatch_start_date, y=15, label="Hatching",
             color="darkred", angle=90) +
    annotate(geom="text", x=emergence_start_date, y=15, label="Emerging",
             color="darkgreen", angle=90) +
    ylim(0,20) +
    xlab("Month") +
    geom_vline(xintercept=FSp_Start, linetype="dashed", 
               color = "red", linewidth=0.5) +
    annotate(geom="text", x=FSp_Start+4, y=18, label="Spn Start",
             color="darkred", angle=90) +
    annotate(geom="text", x=FSp_Peak+4, y=18, label="Spn Peak",
             color="darkgreen", angle=90) +
    geom_vline(xintercept=FSp_Peak, linetype="dashed", 
               color = "green", linewidth=0.5) +
    #  geom_vline(xintercept=Z_hatch_start_date, linetype="dashed", 
    #            color = "blue", linewidth=0.5) +
    #ggtitle(plot_title) +
    scale_x_date(date_breaks="1 month", date_labels="%b", expand = c(0, 0)) +
    scale_y_continuous(
      
      # Features of the first axis
      name = "Mean Daily Temperature (C)", 
      # Add a second axis and specify its features
      sec.axis = sec_axis(~.*coeff, name="Maximum Daily Discharge (cfs)")
    )+
    theme(
      axis.title.y = element_text(color = "blue", size=13),
      axis.title.y.right = element_text(color = "black", size=13),
      axis.title.x = element_text(size = 13)
      axis.text = element_text(size=12)
    )
  
  return(TDPlot)
}


### This function builds a plot depicting Rainbow Trout model data
RT_graph <- function(This_Year_Temps, This_Year_Crit_Dates) {
  
  print(3)
  ## Scale the discharge
  coeff <- 7000/20
  This_Year_Temps$scaled_dis <- This_Year_Temps$maxD/coeff
  x1 <- min(This_Year_Temps$Date, na.rm=TRUE)
  x2 <- max(This_Year_Temps$Date, na.rm=TRUE)
  print(4)
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
  print(5)
  
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
      ylim(0,20) +
      xlab("Month") +
      scale_x_date(date_breaks="1 month", date_labels="%b", expand = c(0, 0)) +
      scale_y_continuous(
        
        # Features of the first axis
        name = "Temperature C",
        
        # Add a second axis and specify its features
        sec.axis = sec_axis(~.*coeff, name="Mean Discharge cfs")
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
      ylim(0,20) +
      xlab("Month") +
      scale_x_date(date_breaks="1 month", date_labels="%b", expand = c(0, 0)) +
      scale_y_continuous(
        
        # Features of the first axis
        name = "Temperature C",
        
        # Add a second axis and specify its features
        sec.axis = sec_axis(~.*coeff, name="mean Discharge cfs")
      )
  }
  
  ## Return plot
  return(TDPlot)
}


### This functions chooses which graph to display based on species input 
show_graph <- function(species, This_Year_Temps, This_Year_Crit_Dates) {
  
  print(1)
  
  if(species == "Brown") {
    return(BT_graph(This_Year_Temps, This_Year_Crit_Dates))
  } else if(species == "Rainbow") {
    print(2)
    return(RT_graph(This_Year_Temps, This_Year_Crit_Dates))
  } else {
    return(NULL)
  }
}