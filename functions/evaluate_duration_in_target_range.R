#Author: Alex Brooks
#Date Created: 10/09/22 

#
#' #Evaluates duration of  flows between minimum and maximum flow value
#' 
#' @param Q_df - dataframe with q_cfs and date columns. Using daily streamflow data
#' @param target_meta_df dataframe with target metadata. Requires columns target_flows, target_days, target_type
#' @param start_month  starting month of analysis 
#' @param start_day starting day of month
#' @param end_month end month of analysis
#' @param end_day end month of analysis
#' @param flow_tolerance tolerance for flow below target [cfs] 
#' 
#' @return dataframe with average duration of days in target range for each water year


evaluate_duration_in_target_range <-  function(Q_df, target_meta_df,
                                           start_month=1,start_day =01,end_month=12, end_day=31, 
                                           flow_tolerance=5){
  flow_tolerance <- flow_tolerance
  Q_filter<- Q_df %>% 
       filter(date >= as.Date(paste(year(date), start_month, start_day, sep = "-")),
           date <= as.Date(paste(year(date), end_month, end_day, sep = "-"))) 
  
  out<- target_meta_df %>% 
    rowid_to_column() %>% 
    group_by(rowid) %>% 
    group_split() %>% 
    map_df(calculate_duration_in_target_range, Q=Q_filter,flow_tol =flow_tolerance)
 
}

