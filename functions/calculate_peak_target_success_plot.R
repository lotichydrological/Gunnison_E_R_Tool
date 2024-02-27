#Author: Alex Brooks
#Date Created: 10/09/22 

#' Helper function to calculate number and proportion of years that met flows in target metadata df. 
#' Function is called in evaluate_peak_targets
#' 
#' @param x target peak flow for defined recurrence interval
#' @param Q calculated peak flow 
#' @param tolerance defined tolerance allowed for missing target [cfs]

calculate_peak_target_success_plot <- function(x, Q, tolerance){
  
  peak_value <- x$target_flows %>% unique(.)
  target_proportion_value <- x$target_goal %>% unique(.)
  target_name <- x$target_name %>% unique(.)
  
  out<- Q %>% 
    mutate(
    result = springPeakQ >=(peak_value-tolerance),  
    target_name = target_name,
    target_goal =target_proportion_value
  )
}
