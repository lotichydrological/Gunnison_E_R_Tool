#Author: Alex Brooks
#Date Created: 10/09/22 

#
#' #Summarizes results from evaluate_duration_in_target_range across period of record
#' 
#' @param x output from evaluate_duration_in_target_range
#' @param target_meta_df target meta data
#' @return dataframe with summarized values across period of record


summarize_evaluate_duration_in_target_range <- function(x,target_meta_df){
  x %>% 
    group_by(target_name) %>% 
    summarize(result= round(mean(duration_days),1)) %>% 
    left_join(target_meta_df) %>% 
    dplyr::select(target_name,metric,target_min_flow,target_max_flow,result,target_goal )
}
