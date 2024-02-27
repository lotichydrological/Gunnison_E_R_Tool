
#Author: Alex Brooks
#Date Created: 10/09/22 

#
#' #Evaluates a minimum flow targets for Black Canyon
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

evaluate_min_flow_tgt_by_yr_type_bc <-  function(Q_df,
                                                  start_month=1,start_day =01,end_month=12, end_day=31,
                                                  deficit_days_tolerance = 0,flow_tolerance= 0){
  
  # Q_filter<- Q_df %>% 
  #   filter(date >= as.Date(paste(year(date), start_month, start_day, sep = "-")),
  #          date <= as.Date(paste(year(date), end_month, end_day, sep = "-"))) 
  
  out<- high_flow_min_flow_Q %>% 
    left_join(bc_peak_targets_by_year,multiple = "all") %>% 
    group_by(water_year,year_type,bc_high_flow_minimum,may1 ) %>% 
    mutate(target_flows_adj = bc_high_flow_minimum-flow_tolerance) %>% 
    summarize(
      blw_base_flow_days = sum(q_cfs <=(target_flows_adj))/(24*4)
    ) %>% 
    mutate(high_min_flag= ifelse(blw_base_flow_days<=deficit_days_tolerance, TRUE,FALSE))
  
}

# summarize_evaluate_min_flow_tgt_by_yr_type <- function(x){
#   x %>% 
#     group_by(target_name, target_goal) %>% 
#     summarise(
#       tgt_met_yrs = sum(result),
#       result = round(100*tgt_met_yrs/n(),1)
#     ) %>% 
#     dplyr::select(target_name,result,target_goal ) %>% 
#     arrange(target_goal )
#   
# }
