
#Author: Alex Brooks
#Date Created: 10/09/22 

#
#' #Evaluates peak targets based on inputs of Q and meta data.
#' 
#' @param Q_df - dataframe with q_cfs and date columns. Using daily streamflow data
#' @param target_meta_df dataframe with target metadata. Requires columns target_flows and target_proportion
#' @param peak_window_size size of rolling windows [days]
#' @param roll_statistic statistic used for rolling window
#' @param start_month  starting month of analysis 
#' @param start_day starting day of month
#' @param end_month ending month of analysis
#' @param end_day ending month of analysis
#' @param flow_tolerance tolerance allowed for just missing target [cfs]

#' @return dataframe with number of years target was met and pct of years for each flow target value

evaluate_peak_targets<- function(Q_df, target_meta_df,
                               peak_window_size=1, roll_statistic='mean',
                               start_month=5,start_day =01,end_month=6, end_day=30,flow_tolerance=5 ){
  
  peak_Q<- Q_df %>% 
    mutate(
      q_cfs_roll = rollapply(q_cfs, FUN=roll_statistic, width=peak_window_size,fill=NA)
    ) %>% 
    filter(date >= as.Date(paste(year(date), start_month, start_day, sep = "-")),
           date <= as.Date(paste(year(date), end_month, end_day, sep = "-"))) %>% 
    group_by(water_year) %>% 
    summarize(springPeakQ=max(q_cfs_roll))
  
  out<- target_meta_df %>% 
    rowid_to_column() %>% 
    group_by(rowid) %>% 
    group_split %>% 
    map_df(calculate_peak_target_success, Q=peak_Q,tolerance=flow_tolerance) %>% 
    dplyr::select(target_name, target_goal,result = tgt_met_pct)
  
}
