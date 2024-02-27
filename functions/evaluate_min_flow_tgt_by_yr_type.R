
#Author: Alex Brooks
#Date Created: 10/09/22 

#
#'  Evaluates a minimum flow target defined by a recurrence interval)
#' 
#' @param Q_df - dataframe with q_cfs and date columns. Using daily streamflow data
#' @param target_meta_df dataframe with target metadata. Requires columns of year_type, target_proportion, targer_flows and month. 
#' Should have a long format 
#' @param start_month  starting month of analysis 
#' @param start_day starting day of month
#' @param end_month ending month of analysis
#' @param end_day edning month of analysis
#' @param deficit_days_tolerance tolerance for number of days below target  [days]
#' @param flow_tolerance tolerance for flow below target [cfs] 
#' @return dataframe with year type, number and % of years target value was met, and the target's goal (as % of years)

evaluate_min_flow_tgt_by_yr_type <-  function(Q_df, target_meta_df,
                                                  start_month=1,start_day =01,end_month=12, end_day=31,
                                                  deficit_days_tolerance = 0,flow_tolerance= 0){
  # filter to selected timeframe
  Q_filter<- Q_df %>% 
    filter(date >= as.Date(paste(year(date), start_month, start_day, sep = "-")),
           date <= as.Date(paste(year(date), end_month, end_day, sep = "-"))) 
  
  # Calculates days below baseflow target. Adjust results based on flow tolerance and duration days   
  out<- Q_filter %>% 
    left_join(target_meta_df) %>% 
    group_by(target_name,target_goal,water_year) %>% 
    mutate(target_flows_adj = target_flows-flow_tolerance) %>% 
    dplyr::summarize(
      blw_base_flow = sum(q_cfs <=(target_flows_adj))
    ) %>% 
    mutate(result= ifelse(blw_base_flow<=deficit_days_tolerance, TRUE,FALSE))
  
}

summarize_evaluate_min_flow_tgt_by_yr_type <- function(x){
  x %>% 
    group_by(target_name, target_goal) %>% 
    dplyr::summarise(
      tgt_met_yrs = sum(result),
      result = round(100*tgt_met_yrs/n(),1)
    ) %>% 
    dplyr::select(target_name,result,target_goal ) %>% 
    arrange(target_goal )
  
}
