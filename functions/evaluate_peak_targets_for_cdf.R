#Author: Alex Brooks
#Date Created: 10/09/22 

#
#' #Evaluates peak targets based on inputs of Q and meta data for use in cdf.
#' 
#' @param Q_df - dataframe with q_cfs and date columns. Using daily streamflow data
#' @param target_meta_df dataframe with target metadata. Requires columns target_flows and target_proportion
#' @param peak_window_size size of rolling windows [days]
#' @param roll_statistic statistic used for rolling window
#' @param start_month  starting month of analysis 
#' @param start_day starting day of month
#' @param end_month ending month of analysis
#' @param end_day edning month of analysis
#' @return dataframe with number of years target was met and pct of years for each flow target value

evaluate_peak_targets_for_cdf_plot<- function(Q_df,
                                          peak_window_size=1, roll_statistic='mean',
                                          start_month=5,start_day =01,end_month=6, end_day=30){
  
  peak_Q<- Q_df %>% 
    mutate(
      q_cfs_roll = rollapply(q_cfs, FUN=roll_statistic, width=peak_window_size,fill=NA)
    ) %>% 
    filter(date >= as.Date(paste(year(date), start_month, start_day, sep = "-")),
           date <= as.Date(paste(year(date), end_month, end_day, sep = "-"))) %>% 
    group_by(water_year) %>% 
    summarize(springPeakQ=max(q_cfs_roll)) #%>% 
  
  peak_Q_cdf <- peak_Q%>% 
    mutate(exceed_p= 1-rank(springPeakQ)/(length(springPeakQ)+1))
}
  