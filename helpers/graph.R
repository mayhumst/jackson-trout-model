
#
# This module takes in the data and builds a graph
#

show_graph <- function(This_Year_Temps, This_Year_Crit_Dates) {
  
  
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
      name = "Temperature C",
      
      # Add a second axis and specify its features
      sec.axis = sec_axis(~.*coeff, name="max Discharge cfs")
    )
  
  return(TDPlot)
}

