#Author: Alex Brooks
#Date Created: 10/09/22 

#' Helper function to calculate duration for each year with a set of flows 
#' Function is called in evaluate_duration_in_target_range
#' 
#' @param x Defined target
#' @param Q calculated flow metric
#' @param flow_tol defined tolerance allowed for missing target [cfs]

calculate_duration_in_target_range <- function(x, Q,flow_tol){
  
  min_flow_value <- x$target_min_flow %>% unique(.)
  max_flow_value <- x$target_max_flow %>% unique(.)
  target_name <- x$target_name %>% unique(.)
  metric <- x$metric %>% unique(.)

  out<- Q %>% 
    group_by(water_year) %>% 
    summarize(
      duration_days = sum(q_cfs >=(min_flow_value - flow_tol) & q_cfs <= (max_flow_value+flow_tol)),
      n_days = n(),
      pct_days=100*duration_days/n_days
    ) %>% 
    mutate(
      target_min_flow = min_flow_value,
      target_max_flow = max_flow_value,
      target_name = target_name,
      metric= metric
    )
}
