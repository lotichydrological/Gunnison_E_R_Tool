library(rvest)

# Helper function to download yearly cbrfc forecast data

downloadForecast <- function(year){
  
  url <- paste0('https://www.cbrfc.noaa.gov/wsup/graph/espplot_data.py?id=BMDC2&year=',year,'&bper=7&eper=10&qpf=0&c=1704205009&esp=0&official=1&observed=0&average=0&median=0&maxmin=0&status=1&unapproved=0&table=1&quantiles=1')
  
  
  if(year>=2016){
    # load cbrfc steamboat forecast
    forecast<- rvest::read_html(url) %>% 
      html_nodes("table") %>% 
      html_table(fill = T) %>% 
      .[[1]] %>% 
    rename('ID'=1, 'Date'=2,'Official_10'=3,'Official_30'=4,'Official_50'=5,'Official_70'=6,'Official_90'=7) %>% 
    mutate(Date = ymd(Date)) 
  } else{
    forecast<- rvest::read_html(url) %>% 
      html_nodes("table") %>% 
      html_table(fill = T) %>% 
      .[[1]] %>% 
      rename('ID'=1, 'Date'=2,'Official_10'=3,'Official_50'=4,'Official_90'=5) %>% 
      mutate(Date = ymd(Date)) 
    
  }
  apr1_forecast <- forecast %>% 
    filter(month(Date)==04, day(Date)==1 ) %>% 
    pull(Official_50)
  
  may1_forecast <- forecast %>% 
    filter(month(Date)==05, day(Date)==1 ) %>% 
    pull(Official_50)
  
  forecast_output <- 
    tibble(
      year = year,
      apr1=apr1_forecast,
      may1 = may1_forecast
    )
  
}

