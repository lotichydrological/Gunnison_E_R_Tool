
#Author: Alex Brooks
#Date Created: 10/09/22 

#
#' Evaluates ramping rates for ascending and descending periods in record using daily mean flow. 
#' Ascending and descending periods are identified as the direction of flow over a define window size (default 45 days)
#' Ramping rates are defined as the maximum of a single value and a proportion of flow. 
#' 
#' @param Q_df - dataframe with q_cfs and date columns. Using daily streamflow data
#' @param ramp_up_value maximum value for ramping up [cfs]. Selects maximum of this value and ramp_up_pct_flow
#' @param ramp_up_pct_flow maximum percent of flow for ramping up [%]. Selects maximum of this value and ramp_up_value
#' @param ramp_down_value maximum value for ramping down [cfs]. Selects maximum of this value and ramp_down_pct_flow
#' @param ramp_down_pct_flow  maximum percent of flow for ramping down [%]. Selects maximum of this value and ramp_down_value 
#' @param tolerance tolerance for exceedances of ramping rate [cfs]
#' @param limb_window_size window size for rolling mean flow used to identify ascending and descending periods
#' @return dataframe with weighted mean of percent of days where ramping rates are met and the target. 


evaluate_ramping_rate <- function(Q_df, ramp_up_value = 500, ramp_up_pct_flow = 0.25,
                         ramp_down_value= 400,ramp_down_pct_flow = 0.15,ramp_tolerance=0,
                         limb_window_size=45){ 
  Q_df %>% 
    mutate(
      mean_cfs_rollmean_45= rollmean(q_cfs, k=limb_window_size,fill=NA),
      ramping_rate=c(0,diff(q_cfs)),
      ramping_rate_45=c(NA,diff(mean_cfs_rollmean_45)),
      ramping_rate_max_target = case_when(
        ramping_rate_45 > 0 ~ pmax(ramp_up_value, ramp_up_pct_flow*q_cfs),
        ramping_rate_45<=0 ~ pmax(ramp_down_value, ramp_down_pct_flow*q_cfs),
        is.na(ramping_rate_45) ~pmax(ramp_down_value, ramp_down_pct_flow*q_cfs)
      ),
      ramping_rate_achievement = (ramping_rate - ramp_tolerance)<= ramping_rate_max_target,
      water_year = calcWaterYear(date)
    ) %>% 
    group_by(water_year) %>% 
    summarize(
      days_blw_ramping_rate = sum(ramping_rate_achievement==TRUE,na.rm=TRUE),
    ) %>% 
    mutate(
      days_in_year = ifelse(leap_year(water_year), 366,365),
      pct_year_abv_ramping_rate = 100*days_blw_ramping_rate/days_in_year,
      result= ifelse(pct_year_abv_ramping_rate>=100, TRUE,FALSE)
    ) 
}

summarize_evaluate_ramping_rate <- function(x){
  x %>% 
    summarize(result = round(mean(pct_year_abv_ramping_rate),1)) %>% 
    mutate(target_goal = 100)
  
}
