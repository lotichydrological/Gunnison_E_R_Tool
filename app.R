#' Title: Gunnison River Environmental and Recreational Flow Tool
#'
#'Description:
#' This is a Shiny web application used for characterizing
#' environmental and recreational flow targets collated from multiple sources
#' including stakeholder input, environmental impact statements and local research reports. 

#' author: Alex Brooks & Seth Mason
#' organization: American Whitewater - Developed by Lotic Hydrological
#' revision date: Jan 16, 2024
#' development phase: working
#' 
#

#load shiny
library(dataRetrieval)
library(shiny)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
#library(dygraphs)
library(xts)
library(htmlwidgets)
library(rhandsontable)
library(lubridate)
library(ggthemes)
#library(shiny.semantic)
library(shinycssloaders)
#library(semantic.dashboard)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(DT)
library(shinybusy)
library(dataRetrieval)
#library(plotly)
library(EflowStats)
library(patchwork)
library(ggiraph)
library(ggh4x)
library(glue)
library(ggnewscale)
library(ggpattern)
library(ungeviz)
library(scales)
library(readxl)
library(knitr)

##### Read in global data and functions

#setwd('G:/Shared drives/Projects/AmericanWhitewater/Gunnison_Rec_Flow_Framework/data/R/shiny/Gunnison_ER_Tool_development')

#load functions
sapply(list.files(path = "./functions", pattern = "[.][R]", full.names = TRUE), source)

#### Ingest Forecast Data ####

# cur_yr<- year(Sys.Date())
# mth <- month(Sys.Date())

cur_yr<- 2024
mth <- 1
# 
# forecast_year_range<- c(1991:(cur_yr-1))
# cbrfc_fc<- forecast_year_range %>%
#   map_df(downloadForecast) %>%
#   mutate(year_type =
#            case_when(
#              may1 < 381 ~ 'Dry',
#              may1 >= 381 & may1 <= 516 ~ 'Mod Dry',
#              may1 > 516 & may1 <= 709 ~ 'Avg Dry',
#              may1 > 709 & may1 <= 831 ~ 'Avg Wet',
#              may1 > 831 & may1 <= 1123 ~ 'Mod Wet',
#              may1 > 1123 ~ 'Wet'
#            ))# %>%
# cbrfc_fc %>% saveRDS('shiny/Environmental_and_Recreational_Flow_Tool/dataIn/cbrfc_May1st_yrTypes.RDS')
cbrfc_may1_yrType <- readRDS('./dataIn/cbrfc_May1st_yrTypes.RDS') %>% 
  dplyr::select(-apr1) %>% 
  rename(water_year = year)

#### Ingest Real Time Historical Discharge ####
parameterCd <- "00060"  # Discharge

startDate <- paste0('1991-04-01')
# if (mth >10){
#   endDate <- paste0(cur_yr, '-09-30')
# } else {
#   endDate <- paste0(cur_yr-1, '-09-30')}
# 
endDate <- ('2024-03-31')
# 
# Q_whitewater <- readNWISuv('09152500', parameterCd,
#                        startDate, endDate, tz='America/Denver') %>%
#   renameNWISColumns() %>%
#   tibble() %>%
#   dplyr::select(datetime=dateTime, q_cfs=Flow_Inst  )%>%
#   mutate(
#     q_cfs_roll_24hrs = rollmean(q_cfs, k=24*4,fill=NA),
#     water_year = calcWaterYear(datetime),
#     month= month(datetime),
#     date= date(datetime)
#   )
# #
# #
# Q_whitewater_daily<- Q_whitewater %>%
#   group_by(date, water_year, month) %>%
#   summarize(q_cfs= mean(q_cfs,na.rm=FALSE))
# # #
#  Q_whitewater %>%  saveRDS('dataIn/Q_whitewater.RDS')
# Q_whitewater_daily %>%  saveRDS('dataIn/Q_whitewater_daily.RDS')

#Q_whitewater <-   readRDS('dataIn/Q_whitewater.RDS')
Q_whitewater_daily <- readRDS('dataIn/Q_whitewater_daily.RDS') %>% 
  mutate(water_year = ifelse(month %in% c(10:12, 1:3), water_year -1, water_year))

# #
# Q_tunnel <- readNWISuv('09128000', parameterCd,
#                        startDate, endDate, tz='America/Denver') %>%
#   renameNWISColumns() %>%
#   tibble() %>%
#   dplyr::select(datetime=dateTime, q_cfs=Flow_Inst  )%>%
#   mutate(
#     q_cfs_roll_24hrs = rollmean(q_cfs, k=24*4,fill=NA),
#     water_year = calcWaterYear(datetime),
#     month= month(datetime),
#     date= date(datetime)
#   )
# #
# Q_tunnel_daily<- Q_tunnel %>%
#   group_by(date, water_year, month) %>%
#   summarize(q_cfs= mean(q_cfs,na.rm=FALSE))
# 
# #
# Q_tunnel %>%  saveRDS('dataIn/Q_tunnel.RDS')
# Q_tunnel_daily %>%  saveRDS('dataIn/Q_tunnel_daily.RDS')


#Q_tunnel <<-   readRDS('dataIn/Q_tunnel.RDS')
Q_tunnel_daily <<- readRDS('dataIn/Q_tunnel_daily.RDS') %>% 
  ungroup() %>% 
  mutate(water_year = ifelse(month %in% c(10:12, 1:3), water_year -1, water_year))

# peak_Q_tunnel<- Q_tunnel %>%
#   mutate(
#     q_cfs_roll = rollapply(q_cfs, FUN='mean', width=1*(4*24),fill=NA)
#   ) %>% 
#   filter(date >= as.Date(paste(year(date), 5, 01, sep = "-")),
#          date <= as.Date(paste(year(date), 6, 30, sep = "-"))) %>% 
#   group_by(water_year) %>% 
#   summarize(springPeakQ=max(q_cfs_roll))
# peak_Q_tunnel %>% saveRDS('dataIn/Q_tunnel_springPeakQ.RDS')
peak_Q_tunnel <<- readRDS('dataIn/Q_tunnel_springPeakQ.RDS')

### calculate year type for inflows
# 
# historical_year_types<- read_csv('dataIn/blue_mesa_inflow_daily.csv') %>%
#   mutate(Date = dmy(Date),
#          Date = if_else(year(Date)< 2023,Date,Date-years(100)),
#          month =month(Date),
#          water_year=year(Date),
#          `Unregulated Inflow*** (cfs)`=as.numeric(`Unregulated Inflow*** (cfs)`)) %>%
#   filter(month %in% 4:7) %>%
#   group_by(water_year) %>%
#   summarize(Q_af_apr_jul = sum(`Unregulated Inflow*** (cfs)`*1.983)) %>%
#   arrange(Q_af_apr_jul) %>%
#   mutate(year_type =
#            case_when(
#              Q_af_apr_jul < 381000 ~ 'Dry',
#              Q_af_apr_jul >= 381000 & Q_af_apr_jul <= 516001 ~ 'Mod Dry',
#              Q_af_apr_jul > 516001 & Q_af_apr_jul <= 709001 ~ 'Avg Dry',
#              Q_af_apr_jul > 709001 & Q_af_apr_jul <= 831001 ~ 'Avg Wet',
#              Q_af_apr_jul > 831001 & Q_af_apr_jul <= 1123000 ~ 'Mod Wet',
#              Q_af_apr_jul > 1123000 ~ 'Wet'
#            )) %>%
#   dplyr::select(water_year, year_type) %>%
#   arrange(water_year) %>%
#   bind_rows(
#     tibble(water_year = 2023, year_type = 'Mod Wet')
#   )
# 
# historical_year_types %>% saveRDS('dataIn/historical_year_types.RDS')
historical_year_types <-  readRDS('dataIn/historical_year_types.RDS')


#### Read in Flow target examples ####
flw_target_dat_whitewater <<-readxl::read_excel('dataIn/target_hydrographs.xlsx',sheet='whitewater') %>% 
  mutate(date = date(date)) %>% 
  mutate(year_type = factor(year_type, levels=c('Wet','Mod Wet','Avg Wet','Avg Dry','Mod Dry','Dry'))) %>% 
  pivot_longer(cols=base:hydrograph_improvement) %>% 
  mutate(doy = get_waterYearDay(date, wy=4L)) %>%
  mutate(
    fake_date = as.Date(doy-1, origin= '2013-04-01'),
    fake_date_customQ = as.Date(yday(date)-1,origin = '2049-01-01'))

flw_target_dat_black_canyon <<-readxl::read_excel('dataIn/target_hydrographs.xlsx',
                                                 sheet='black_canyon') %>% 
  mutate(date = date(date)) %>% 
  mutate(year_type = factor(year_type, levels=c('Wet','Mod Wet','Avg Wet','Avg Dry','Mod Dry','Dry'))) %>% 
  pivot_longer(cols=base:hydrograph) %>% 
  mutate(doy = get_waterYearDay(date, wy=4L)) %>%
  mutate(
    fake_date = as.Date(doy-1, origin= '2013-04-01'),
    fake_date_customQ = as.Date(yday(date)-1,origin = '2049-01-01'))


flw_target_dat_gunnison_gorge <-readxl::read_excel('dataIn/target_hydrographs.xlsx',
                                                   sheet='gunnison_gorge') %>% 
  mutate(date = date(date)) %>% 
  pivot_longer(cols=base:max_safety) %>% 
  filter(name != 'max_safety') %>% 
  mutate(doy = get_waterYearDay(date, wy=4L)) %>%
  mutate(
    fake_date = as.Date(doy-1, origin= '2013-04-01'),
    fake_date_customQ = as.Date(yday(date)-1,origin = '2049-01-01'))

## ingest target tables

rec_targets_meta <- read_excel("dataIn/target_metadata.xlsx",sheet='Recreational_Targets') %>% 
  dplyr::select(-Note)

bc_eflow_targets_meta <- read_excel("dataIn/target_metadata.xlsx",sheet='BC_eflows')
gg_eflow_targets_meta <- read_excel("dataIn/target_metadata.xlsx",sheet='GG_eflows')
lg_eflow_targets_meta <- read_excel("dataIn/target_metadata.xlsx",sheet='LG_eflows')


#### Targets ####

##### Black Canyon #####

# Targets by year based on equation and May 1st forecast

bc_peak_targets_by_year<<- cbrfc_may1_yrType %>% 
  mutate(
    bc_peak_flow_target = 
      case_when(
        may1 <= 372 ~ 482.95+1.44*(may1),
        may1>372 & may1<=715 ~ 15.24*may1 - 4651.66,
        may1> 715 & may1<=925 ~5449.13+1.15*may1,
        may1>925 & may1<=1001 ~ 14.57*may1-6975.28,
        may1>1001 & may1<=1050 ~ 70.4*may1-62886,
        may1>= 1050 ~ 10.68*may1-180),
    bc_high_flow_minimum = 
      case_when(
        may1<=561 ~ 300,
        may1>561 & may1<= 690 ~ 2.692*may1 -1207.69,
        may1 > 690 & may1<=1000 ~ 1.129 * may1 - 129,
        may1>1000 ~ 1000
      )
  )

# Spring Peak flow
black_canyon_peak_target_meta <<- tibble(
  target_flows =c(627,1155,3898,6153,6450,11814),
  target_goal = c(100,90,70,50,30,10),
  target_name= c("Peak Flow Target: 627 cfs","Peak Flow Target: 1155 cfs",
                 "Peak Flow Target: 3898 cfs","Peak Flow Target: 6154 cfs","Peak Flow Target: 6450 cfs","Peak Flow Target: 11814 cfs")
)

# Peak Flow Period Minimums
black_canyon_high_flow_minimum_meta<<-
  tibble(
    target_name = c('High Flow Minimum: 300 cfs',
                    'High Flow Minimum: 671 cfs','High Flow Minimum: 854 cfs',
                    'High Flow Minimum: 1000 cfs'),
    target_goal =c(100,50,30,10),
    May = c(300,671,854,1000),
    Jun = c(300,671,854,1000),
    Jul = c(300,671,854,1000),
    year_type = c('Dry, Moderately Dry and Average Dry','Average Wet','Moderately Wet','Wet')
  ) %>%
  pivot_longer(cols=c(May:Jul),names_to='month_abb',values_to = 'target_flows') %>%
  mutate(month = match(month_abb,month.abb)) %>%
  dplyr::select(-month_abb)

black_canyon_baseflow_duration_target <<- tibble(
  target_min_flow =c(300),
  target_max_flow =c(Inf),
  target_days = c(365),
  target_goal = c(365),
  target_name=c('Baseflow Minimum Flow'),
  metric= 'Mean Duration Meeting Target (Days)')

black_canyon_recreation_targets<<- tibble(
  target_min_flow =c(600, 800, 1600, 600),
  target_max_flow =c( 800, 1600, 3000, 3000),
  target_goal = c(36.2,53.9,17.9,107.7),
  target_name=c('Lower Acceptable',
                'Optimal',
                'Upper Acceptable',
                'Total')) %>%
  mutate(
    metric = 'Mean Boatable Days')

##### GGNCA #####

ggnca_peak_duration_target<<- tibble(
  target_min_flow =c(3500),
  target_max_flow =c(Inf),
  target_goal = c(0),
  target_name=c(''),
  metric= 'Duration Exceeding Target (Days)')

ggnca_range_duration_target<- tibble(
  target_min_flow =c(400),
  target_max_flow =c(1200),
  target_goal = c(186),
  target_name=c('Ideal Flow Range'),
  metric= 'Mean Duration Within Target Range (Days)')

ggnca_baseflow_duration_target<- tibble(
  target_min_flow =c(300),
  target_max_flow =c(Inf),
  target_goal = c(365),
  target_name=c('Baseflow Minimum Flow'),
  metric= 'Mean Duration Above Minimum Flow (Days)')

ggcna_recreation_targets<<- tibble(
  target_min_flow =c(600, 800, 3000, 600,400,500,1000, 400,0,0),
  target_max_flow =c( 800, 3000, 15000, 15000,500,1000,3000,3000,4000,9000),
  target_goal = c(36.2,71.6,13.8,121.4,12.5,73.4,50.6,136.3,177,183.5),
  target_name=c('WW Lower Acceptable','WW Optimal',
                'WW Upper Acceptable', 'WW Total',
                'Ang Lower Acceptable','Ang Optimal',
                'Ang Upper Acceptable', 'Ang Total)',
                'Max Flow','Max'),
  target_type= c('Whitewater Recreation','Whitewater Recreation',
                 'Whitewater Recreation', 'Whitewater Recreation',
                 'Angling','Angling ',
                 'Angling', 'Angling',
                 'Camping/Trail Access','Safety')
) %>%
  mutate(
    metric = 'Mean Boatable Days')

##### Lower Gunnison #####

whitewater_peak_target_meta<<- tibble(
  target_flows =c(900,2600,8070,14350,15000),
  target_name= c("Peak Flow Target: 900","Peak Flow Target: 2600",
                 "Peak Flow Target: 8070","Peak Flow Target: 14350","Peak Flow Target: 15000"),
  target_goal = c(100,90,70,50,10)
)

whitewater_peak_duration_target<<- tibble(
  target_min_flow =c(8070, 14350, 8070, 14350),
  target_max_flow =c(Inf, Inf, Inf, Inf),
  target_goal = c(20,4,32,7),
  target_name=c('Maintenance (>8070 cfs)','Maintenance (>14350 cfs)','Improvement (>8070 cfs)','Improvement (>14350 cfs)')) %>%
  mutate(metric = 'Mean Duration of Flows Above Target')

whitewater_base_meta<<- read_csv('dataIn/whitewater_baseflow_targets.csv') %>%
  pivot_longer(cols=c(Jan:Dec),names_to='month_abb',values_to = 'target_flows') %>%
  mutate(month = match(month_abb,month.abb)) %>%
  dplyr::select(-month_abb) %>%
  rename(target_name = year_type,target_goal = target_proportion)

lower_gunnison_recreation_targets<<- tibble(
  target_min_flow =c(800, 1000, 15000, 800),
  target_max_flow =c( 1000, 10000, 20000, 20000),
  target_goal = c(30.6,306.6,1.3,342.2),
  target_name=c('Lower Acceptable','Optimal',
                'Upper Acceptable', 'Total')) %>%
  mutate(metric = 'Mean Boatable Days')

#### Precalculate Metrics to Save on Load Time ####



#### Shiny UI #####

# App uses Shiny dashboard UI format

ui <- shinydashboard::dashboardPage(skin = "black",
                                    dashboardHeader(title='E&R Tool'),
                                    
                                    #Create sidebar itrms
                                    dashboardSidebar(sidebarMenu(
                                      menuItem("Home",tabName = "home"),
                                      menuItem("Tool Overview",tabName = "instructions"),
                                      menuItem("User Selections",tabName = "userpref"),
                                      menuItem(tabName = "blackcanyon",text='Black Canyon'),
                                      menuItem(tabName = 'ggnca',text='Gunnison Gorge NCA'),
                                      menuItem('Lower Gunnison',tabName = 'lowergun'),
                                      menuItem("Forecast Tool",tabName = "forecast")
                                      
                                    )),
                                    
                                    ###### create dashboard body #####
                                    dashboardBody(
                                      #start up spinner
                                      busy_start_up(loader = spin_kit(
                                        spin = "cube-grid",
                                        color = "#337ab7",
                                        style = "width:150px; height:150px;"),
                                        mode = "timeout",
                                        timeout = 2000,
                                      ),
                                      useShinyjs(),  # Set up shinyjs
                                      
                                      tabItems(
                                        tabItem(tabName='home',
                                                fluidRow(
                                                  box(
                                                    width=12,title=h2("Lower Gunnison Environmental and Recreational Flow Tool",align='center'),
                                                    tags$figure(
                                                      align = "center",
                                                      href="https://www.americanwhitewater.org/", 
                                                      tags$img(
                                                        src = "AW_logo.png",
                                                        width="25%",height="10%",
                                                        alt = "AW logo"
                                                      ),
                                                    ),
                                                    br(),
                                                    br(),
                                                    p("This tool enables users to assess achievement of environmental and recreational flow targets on the Lower Gunnison River. 
                        Users can assess targets across a user defined historical period of record or input a custom hydrograph for analysis. 
                        Environmental metrics and targets in the tool are obtained from a review of operational agreements and published biological opinions related to the Aspinall Unit. 
                        Recreational flow preferences are primarily derived from studies developed by American Whitewater."
                                                    ),
                                                    br(),
                                                    tags$h5(
                                                    span("Reaches include: "), 
                                                    tags$a(href="https://www.americanwhitewater.org/content/River/view/river-detail/401/main", "Black Canyon of the Gunnison"),
                                                    span(', '),
                                                    tags$a(href="https://www.americanwhitewater.org/content/River/view/river-detail/402/main", "Gunnison Gorge"),
                                                    span(', and the'), 
                                                    tags$a(href="https://www.americanwhitewater.org/content/River/view/river-detail/10564/main", "Lower Gunnison"),
                                                    span('. Follow the links to learn more on the American Whitewater website. ')
                                                    )
                                                  )
                                                ),
                                                fluidRow(
                                                  tags$figure(
                                                    align = "left",
                                                    tags$img(
                                                      src = "Gunnison_Gorge_Mike_Sawyer.jpeg",
                                                      width="100%",height="20%",
                                                      alt = "Picture of river"
                                                    ),
                                                  )
                                                ),
                                                fluidRow(
                                                  align = "center",
                                                  p('Developed by'),
                                                  tags$figure(
                                                    align = "center",
                                                    href="https://lotichydrological.com/", 
                                                    tags$img(
                                                      src = "2015_logo_black.png",
                                                      width="10%",height="10%",
                                                      alt = "Lotic"
                                                    ),
                                                  )
                                                )
                                        ),
                                        ###### Tool Overview #####
                                        
                                        tabItem(tabName = "instructions",
                                                fluidRow(
                                                  box(width=12,title='Tool Instructions',
                                                    p('The Lower Gunnison Environmental & Recreational (E&R) Tool enables users to assess achievement of environmental and recreational flow targets on the Lower Gunnison River. Users can evaluate E&R targets across a user defined historical period of record or input a custom hydrograph for analysis. Environmental metrics and targets in the tool are obtained from a review of operational agreements and published biological opinions related to the Aspinall Unit. Recreational flow preferences are primarily derived from studies developed by American Whitewater.'),
                                                    p(strong('Historical Analysis')),
                                                    p('The user selection tab enables the user to select a historical period of record for the analysis from between 1991 - 2023. The user can also select two water years that are plotted as hydrographs against E&R targets. Once selected, the user can start the analysis by clicking the "Run Historical Analysis" button. If input years are changed, the user can select the button again to rerun the analysis. The user can then navigate to the Black Canyon, Gunnison Gorge NCA or Lower Gunnison tabs to view the historical analysis results.'),
                                                    p('The Tool performs evaluations of both environmental and recreation flow targets by water year. The Tool defines the water year as from April to March. This definition of the water year better aligns with Blue Mesa Reservoir’s operations than the more traditional October to September definition of a water year. In particular, the Apr-Mar water year is better for describing how winter streamflows are managed since winter streamflow targets on the Lower Gunnison are based on the year-type designation derived from the most recent Apr-Jul inflow period.'),
                                                    p(strong('User Defined Time Series')),
                                                    p('The user selection tab also enables users to analysis a user-defined streamflow time series. The user can enter data for Black Canyon and Gunnison Gorge NCA in the "Gunnison.BelowTunnel" column and for the Lower Gunnison in the "Gunnison.Whitewater" column. The user also needs to select the forecasted Blue Mesa Apr-Jul Inflow (kaf). The forecast Blue Mesa inflow is used to define "forward looking" targets that are defined by year type and/or the forecasted inflow magnitude. The time series should be entered for a water year (starting on April 1st and ending on March 31st).')
                                                  ),
                                                ),
                                                fluidRow(
                                                  box(width=12,title='Environmental Flow Targets',
                                                  #  h3(),
                                                    p('Environmental flow targets are obtained from a review of operational agreements and published biological opinions related to the Aspinall Unit, and recreational flow preference information collected by American Whitewater for reaches of the Gunnison River below the Crystal Dam. The review identified several key documents that reported on environmental flow targets including the 2009 Programmatic Biological Assessment (PBO, USFWS, 2009), the 2012 Final Environmental Impact Statement (FEIS, USBR 2012), the 2012 FEIS Record of Decision (ROD, USBR 2012), and the 2009 Black Canyon Water of the Gunnison Water Right Degree. Related reports, presentations and manuscripts are also examined (McAda 2003, USBR 2021). '),
                                                    p('Peak flow, maintenance & improvement flow, and baseflow targets on the Lower Gunnison are ‘forward-looking’. Targets are assigned to one of six different year-types. The development of these targets is based on the expected recurrence interval of specific inflows as described in the 2009 PBO. Peak and high flow minimum targets for the Black Canyon reach are adapted from the Black Canyon National Park Water Right decree. The water right identifies year-types but does not use them for calculating flow thresholds. Target levels are instead calculated using stepwise equations based on the May 1st Blue Mesa Apr-Jul Inflow forecast (hereafter referred to as May 1st Forecast). The National Park Service designed this approach to ensure a more ‘continuous’ range of flow conditions than would arise from using the year-type approach which bins flows into specific categories. '),
                                                    p('The DSS Tool accommodates ‘forward-looking’ target by identifying the target magnitudes based on the historical record of May 1st Forecasts, obtained from the Colorado Basin River Forecast Center. The May 1st Forecast is then used to identify year-type and continuous-based targets for each year in the user select period.'),
                                                    br(),
                                                    p('Details on environmental targets  for each reach can be found in the tables below:'),
                                                    br(),
                                                    DT::DTOutput("BC_eflow"),
                                                    br(),
                                                    DT::DTOutput("GG_eflow"),
                                                    br(),
                                                    DT::DTOutput("LG_eflow")
                                                  
                                                  )
                                                ),
                                                fluidRow(
                                                  box(width=12,title='Recreational Flow Preferences',
                                                    p('Recreational flow targets for whitewater activities across the reaches are identified from the Assessing Streamflow Needs for Whitewater Recreation in the Gunnison River basin report (Menges et al, 2013). Supplemental information for other recreational activities including angling preferences are obtained for Gunnison Gorge National Conservation Area (Gunnison Gorge NCA) from the 2012 FEIS. '),
                                                    p('Recreational flow preferences for whitewater recreation and angling are expressed as user preference flow ranges corresponding to Optimal, Lower Acceptable and Upper Acceptable streamflow conditions. Flow preferences for angling are only available for the Gunnison Gorge reach. The number of Boatable Days is then evaluated for each year. Boatable days are calculated as the number of days that streamflow is within a specific flow preference category. '),
                                                    p('There are no existing targets for the number of Boatable Days falling in each category in any given year. However, the optimal and acceptable flow ranges allow for comparison with environmental flow targets. We assume that one acceptable goal is to maximize the number of Boatable Days while also meeting environmental flow needs. An additional goal is to provide a range of recreational experiences across each acceptable and optimal flow range.'),
                                                    p('Annual boatable days calculations in the decision-support tool are calculated only for ‘in-season’ periods. Recreation on the Black Canyon and Gunnison Gorge NCA is considered in-season only during the May through October boating season. During other months, roads are often not accessible to these reaches. Botable days for the Lower Gunnison are calculated throughout the full water year. '),
                                                    p('Details on recreational flow preferences  for each reach can be found in the tables below:'),
                                                    br(),
                                                    DT::DTOutput("rec_meta_DT")
                                                  

                                                  )
                                                )
                                        ),
                                        ###### User Preferences #####

                                        tabItem(tabName = "userpref",
                                                fluidRow(
                                                  box(title='User Selections For Historical Analysis',
                                                      p("Make selections from each of the dropdowns below to run the E&R tool.
                Select a start water year and end water year from the dropdown menus below that you want to use to calculate E&R results. 
                You can also select two water years to display as hydrographs. 
                Tolerances for environmental flow targets can also be adjusted in the advanced settings."),
                                                      p("  
                Start the analysis by clicking the 'Run Historical Analysis' button. 
                If inputs are changed, click the  button again to rerun the analysis"),
                                                      # flow_layout(
                                                      #   min_cell_width = 300,
                                                      #   column_gap = "25px",
                                                      #   row_gap = "25px",
                                                      
                                                      shiny::uiOutput("select_startyear"),
                                                      shiny::uiOutput("select_endyear"),
                                                      shiny::uiOutput("select_year1"),
                                                      shiny::uiOutput("select_year2"),
                                                      # shiny::uiOutput('select_flow_tolerance'),
                                                      #shiny::uiOutput('select_duration_tolerance'),
                                                      #shiny::uiOutput('select_ramping_tolerance'),
                                                      
                                                      div(
                                                        style="padding-top: 20px", 
                                                        shiny::actionButton("evalButton", "Run Historical Analysis", 
                                                                            style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                        br(),   
                                                        br(),
                                                        p('Once the button is clicked, you can navigate to the Black Canyon, Gunnison Gorge NCA or Lower Gunnison tabs to view results.')
                                                      )
                                                  ),
                                                ),
                                                
                                                fluidRow(
                                                  box(width=12,title='User Defined Time Series',
                                                      p("You may enter your own streamflow time series data for Black Canyon and Gunnison Gorge NCA in the 'Gunnison.BelowTunnel' column and for the Lower Guinnison in the 'Gunnison.Whitewater'. 
              Values can be entered into the table clicking and editing one cell at a time. Alternatively, multiple values can be copied from a spreadsheet (Ctrl + C) and pasted 
              into a column by clicking a starting cell and then using a keystroke shortcut (Ctrl + V) to enter values into the selected cell and the cells below it. 
                                                        The timeseries should be entered starting on April 1st and ending on March 31st."),
                                                      
                                                      br(),
                                                      p('Select the forecasted Blue Mesa Apr-Jul Inflow (kaf) below. The forcasted Blue Mesa inflow will be used to define "forward looking" targets that are defined
                                                        by year type and/or the forecasted inflow magnitude.'),
                                                      shiny::uiOutput("blue_mesa_forecast"), 
                                                      
                                                      p("Enter the user-defined timeseries below and then start analysis by clicking the 'Run Analysis' button and then navigate to the Forecast Tool tab.
              If the full table does not appear below at start up, click within the table to expand it."),
                                                      div(style="padding: 10px",
                                                          actionButton("runButton_custom", "Run Custom Analysis",
                                                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                          # actionButton("resetButton_custom", "Reset Table",
                                                          #              style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                                      ),
                                                      p('Once button is clicked, you can navigate to the Forecast Tool tab to view results.'),
                                                      div(rHandsontableOutput("customQ"))
                                                  )
                                                ),
                                                fluidRow(
                                                  # box(title='Tolerances for Environmental Flow Targets.',id='tol_box',
                                                  accordion(width=8,id='tol_box',
                                                            accordionItem(title='Advanced Settings',#status='black', solidHeader=TRUE,
                                                                          
                                                                          p('Click `Update Tool Setting` button to make changes to E&R tool settings. 
               Once settings are updated, re-run either the historical or custom analysis for updates results.'),
                                                                          
                                                                          div(style="padding: 10px",
                                                                              actionButton("runButton_advSet", "Update Tool Setting",
                                                                                           style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                                          ),
                                                                          
                                                                          h4('Set Environmental Flow Tolerances'),
                                                                          
                                                                          p("Environmental flow tolerances have been set to defaults values based on stakeholder input. Tolerances allow for near-misses to be counted as meeting target goals. 
               The user may adjust these tolerances below. 
               Flow tolerances do not currently apply to the recreational preferences assessments"),
                                                                          br(),
                                                                          
                                                                          p("Select a flow tolerance (CFS) for target assessment Enables near misses based on streamflow magnitude to be treated as achieving target. 
               Example: When flow tolerance is set to 5 CFS, then a minimum flow of 296 CFS would meet a baseflow target of 300 CFS."),
                                                                          br(),
                                                                          shiny::uiOutput('select_flow_tolerance'),
                                                                          
                                                                          br(),
                                                                          p("Select a duration tolerance (days) for target assessment. Enables near misses based on streamflow duration to be treated as achieving target.
               Example: When duration tolerance is set to 3 days, a water year with streamflow below 300 CFS for 2 days would meet a target goals of zero days below 300 CFS."),
                                                                          br(),
                                                                          shiny::uiOutput('select_duration_tolerance'),
                                                                          
                                                                          br(),
                                                                          
                                                                          p("Select a ramping rate tolerance (CFS) for target assessment. Enables near misses based on ramping rates to be treated as achieving target.
               Example: When ramping rate tolerance is set to 5 cfs, a daily ramping rate of 104 cfs would meet a target of <100 cfs change in flow per day."),
                                                                          
                                                                          br(),
                                                                          shiny::uiOutput('select_ramping_tolerance')
                                                            )
                                                  )        
                                                  #)
                                                )
                                        ),
                                        ###### black canyon #####
                                        
                                        tabItem(tabName = "blackcanyon",
                                                
                                                fluidRow(
                                                  tabBox(width=12,title='',id='bc_ef',  
                                                         tabPanel('Flow Targets',
                                                                  p("E&R targets and historical hydrographs for user selected water years. Water years are defined as between April - March. Peak and high flow minimum targets are calculated based on the May 1st Blue Mesa Apr-Jul inflow forecast. Ramping rate are not displayed on this figure."),
                                                                  withSpinner(plotOutput('BC_hydro_plot',width='80%',height='900px')),
                                                                  br(),
                                                                  p("The figure below displays peak flow and high flow minimum targets as described in 2008 Black Canyon Water Right."),
                                                                  withSpinner(plotOutput('BC_tgt_plot',width='85%')),
                                                                ),
                                                         tabPanel('Environmental Flows',
                                                                  p("Histograms for environmental flow metrics over the user defined period of record."),     
                                                                  br(),
                                                                  withSpinner(plotOutput("BC_env_results",width='101%'))),
                                                         tabPanel('Recreational Flows',
                                                                  p("Total annual and monthly histograms for whitewater boatable days by user preference class for the user-defined period of record."),     
                                                                  withSpinner(plotOutput("BC_rec_by_User",width='50%',height='500px')),
                                                                  br(),
                                                                  withSpinner(plotOutput("BC_rec_monthly",width='50%',height='900px'))
                                                         ),
                                                        
                                                        tabPanel('Results by Water Year',
                                                                 p("Annual results for environmental and recreational flows over the user-defined period of record. Results are summarized by water year (April - March). Red lines indicate target thresholds for a given environmental flow metric."),     
                                                                 withSpinner(girafeOutput('BC_results_by_year',width='101%',height='1000px'))
                                                          )
                                                  )
                                                ),
                                        ),
                                        ###### GGCNA #####
                                        
                                        tabItem(tabName = "ggnca",
                                                
                                                fluidRow(
                                                  tabBox(width=12,title='',id='gg_ef',
                                                         tabPanel('Flow Targets',
                                                                  p("E&R targets and historical hydrographs for user selected water years. Water years are defined as between April - March."),
                                                                  plotOutput('GG_hydro_plot',width='101%')
                                                         ),
                                                         tabPanel('Environmental Flows',
                                                                  p("Histograms for environmental flow metrics over the user defined period of record."),     
                                                                  br(),
                                                                  withSpinner(plotOutput("GG_env_results",width='101%'))
                                                         ),
                                                         tabPanel('Recreational Flows',
                                                                    p("Total annual and monthly histograms for whitewater and angling boatable days by user preference class for the user-defined period of record."),     
                                                                    withSpinner(plotOutput("GG_rec_by_User",width='100%','600px')),
                                                                    br(),
                                                                    withSpinner(plotOutput("GG_rec_monthly",width='100%',height='900px'))
                                                         ),
                                                         tabPanel('Results by Water Year',
                                                                  p("Yearly Results for environmental and recreational flows over the user-defined period of record. Results are summarized by water year (April - March)."),     
                                                                  withSpinner(girafeOutput('GG_results_by_year',width='101%',height='900px'))
                                                         )
                                                  )
                                                )
                                        ),
                                         ###### lower gunnison #####

                                        tabItem(tabName = "lowergun",
                                                fluidRow(
                                                  tabBox(width=12,title='',id='lg_ef',
                                                         tabPanel('Flow Targets',
                                                                  p("E&R targets and historical hydrographs for user-selected water years (April - March) based on expected exceedance frequency of target (year type). Corresponding operational year types are noted in parentheses for historical hydrographs. Ramping rate and flow variability environmental targets are not displayed on this figure."),
                                                                  shiny::uiOutput("select_target_yr_type_lg"),
                                                                  plotOutput('LG_hydro_plot',width='101%')
                                                         ),
                                                         tabPanel('Environmental Flows',
                                                                  p("Histograms for environmental flow metrics over the user defined period of record. Reach outline indicates expected frequency of environmental flows based on historical year type recurrence interval (as described in 2009 PBO)."), 
                                                                  br(),
                                                                  withSpinner(plotOutput("LG_env_results",width='101%',height='600px'))),
                                                         tabPanel('Recreational Flows',
                                                                  p("Total annual and monthly histograms for whitewater boatable days by user preference class for the user-defined period of record."),     
                                                                  withSpinner(plotOutput("LG_rec_by_User",width='75%',height='500px')),
                                                                  br(),
                                                                  withSpinner(plotOutput("LG_rec_monthly",width='65%',height='1000px'))
                                                         ),
                                                         
                                                         tabPanel('Results by Water Year',
                                                                  p("Yearly results for environmental and recreational flows over the user-defined period of record. Red lines indicate target thresholds for a given environmental flow metric. Results are summarized by water year (April - March)"),     
                                                                  withSpinner(girafeOutput('LG_results_by_year',width='100%',height='1000px'))
                                                         )
                                                   )

                                                )
                                        ),
                                      ###### forecast #####
                      
                                        tabItem(tabName='forecast',
                                                h2("Black Canyon"),
                                                fluidRow(
                                                  tabBox(width=12,title='',id='bc_forecast',
                                                         tabPanel('Flow Targets',
                                                                  shiny::uiOutput("select_target_yr_type_bc_customQ"),
                                                                  p("User-defined hydrograph and E&R targets based on user-defined May 1st Blue Mesa Apr-Jul Forecast. Ramping rate environmental targets are not displayed on this figure."),
                                                                  withSpinner(plotOutput('BC_hydro_plot_customQ',width='101%'))
                                                         ),
                                                         tabPanel('Summary Table',
                                                                  p("Assessment results of custom hydrograph."),
                                                                  withSpinner(DT::DTOutput("BC_customQ_dt"))),
                                                         tabPanel('Whitewater Boatable Days',
                                                                  p("Whitwater boatable days assessment by month for user-defined hydrograph. "),
                                                                  withSpinner(plotOutput("bc_rec_plot_custom",width='75%')),
                                                                  br(),
                                                                  withSpinner(plotOutput("bc_rec_plot_total",width='50%',height='300px'))
                                                         )
                                                         
                                                  )
                                                ),
                                                br(),
                                                h2("Gunnison Gorge National Conservation Area"),
                                                fluidRow(
                                                  tabBox(width=12,title='',id='gg_forecast',
                                                         tabPanel('Flow Targets',
                                                                  # shiny::uiOutput("select_target_yr_type_gg_customQ"),
                                                                  p("User-defined hydrograph and E&R targets."),
                                                                  withSpinner(plotOutput('GG_hydro_plot_customQ',width='101%'))
                                                         ),
                                                         tabPanel('Summary Table',
                                                                  p("Assessment results of custom hydrograph. "),
                                                                  withSpinner(DT::DTOutput("GG_customQ_dt"))),
                                                         tabPanel('Whitewater Boatable Days',
                                                                  p("Whitwater boatable days assessment by month for user-defined hydrograph."),
                                                                  withSpinner(plotOutput("gg_rec_plot_custom_ww",width='75%')),
                                                                  br(),
                                                                  withSpinner(plotOutput("gg_rec_plot_total_ww",width='50%',height='300px'))
                                                         ),
                                                        tabPanel('Angling Boatable Days',
                                                                  p("Angling boatable days assessment by month for user-defined hydrograph. "),
                                                                  withSpinner(plotOutput("gg_rec_plot_custom_ang",width='75%')),
                                                                  br(),
                                                                  withSpinner(plotOutput("gg_rec_plot_total_ang",width='50%',height='300px'))
                                                         )
                                                  )
                                                ),
                                                br(),
                                                h2("Lower Gunnison"),
                                                fluidRow(
                                                  tabBox(width=12,title='',id='lg_forecast',
                                                         tabPanel('Flow Targets',
                                                                  shiny::uiOutput("select_target_yr_type_lg_customQ"),
                                                                  p("User-defined hydrograph and E&R targets based on user selected frequency target levels."),
                                                                  withSpinner(plotOutput('LG_hydro_plot_customQ',width='101%'))
                                                         ),
                                                         tabPanel('Summary Table',
                                                                  p("Assessment results of user-defined hydrograph."),
                                                                  withSpinner(DT::DTOutput("LG_customQ_dt"))),
                                                         tabPanel('Whitewater Boatable Days',
                                                                  p("Whitewater boatable days assessment by month of user-defined hydrograph. "),
                                                                  withSpinner(plotOutput("lg_rec_plot_custom",width='100%')),
                                                                  br(),
                                                                  withSpinner(plotOutput("lg_rec_plot_total",width='50%',height='300px'))
                                                        )
                                                  )
                                                )
                                        )
                                      )
                                    )
)

#### Server Side ####

server = function(input, output, session){
  
  options(shiny.sanitize.errors = TRUE)
  
  observeEvent(input$evalButton, {
    updateAccordion(id = "accordion1",selected=  "accordion1")
  })
  
  ####Defaults #########
  
  ramp_tol <<- 0
  flow_tol <<- 5
  duration_tol <<- 0
  
  ####User Selections #########
  
  # Filter Q for user selected start and end years
  output$select_startyear <- renderUI({
    shiny::selectInput(inputId = 'select_startyear',
                       label = 'Select Start Water Year',
                       multiple = FALSE,
                       choices = c(1991:2023),
                       selected = 1991)
  })
  
  output$select_endyear <- renderUI({
    shiny::selectInput(inputId = 'select_endyear',
                       label = 'Select End Water Year',
                       multiple = FALSE,
                       choices = c(1991:2023),
                       selected = 2023)
  })
  
  # Filter years for hydrograph figure dropdowns menus 
  output$select_year1 <- renderUI({
    req(input$select_endyear)
    shiny::selectInput(inputId = 'select_year1',
                       label = 'Select Water Year 1 for Hydrograph',
                       multiple = FALSE,
                       choices = c(as.numeric(input$select_startyear):as.numeric(input$select_endyear)))
  })
  
  output$select_year2 <- renderUI({
    req(input$select_endyear)
    shiny::selectInput(inputId = 'select_year2',
                       label = 'Select Water Year 2 for Hydrograph',
                       multiple = FALSE,
                       choices = c(as.numeric(input$select_startyear):as.numeric(input$select_endyear)))
  })
  
  # # Year type selections for target figures 
  # output$select_target_yr_type_bc <- renderUI({
  #   shiny::selectInput(inputId = 'select_target_yr_type_bc',
  #                      label = 'Select Frequency Based Target (Percent of years where target should be met/exceeded)',
  #                      multiple = FALSE,
  #                      choices = c('10% frequency','30% frequency','50% frequency','70% frequency','90% frequency','100% frequency'),
  #                      selected = '50% frequency')
  # })
  
  # output$select_target_yr_type_bc_customQ <- renderUI({
  #   shiny::selectInput(inputId = 'select_target_yr_type_bc_customQ',
  #                      label = 'Select Frequency Based Target (Percent of years where target should be met/exceeded)',
  #                      multiple = FALSE,
  #                      choices = c('10% frequency','30% frequency','50% frequency','70% frequency','90% frequency','100% frequency'),
  #                      selected = '50% frequency')
  # })
  # 
  # Year type selections for target figures 
  output$select_target_yr_type_lg <- renderUI({
    shiny::selectInput(inputId = 'select_target_yr_type_lg',
                       label = 'Select Year Type Frequency Target (Percent of years where target should be met/exceeded)',
                       multiple = FALSE,
                       choices = c('10% frequency (Wet)','30% frequency (Mod Wet)','50% frequency (Avg Wet)','70% frequency (Avg Dry)','90% frequency (Mod Dry)','100% frequency (Dry)'),
                       selected = '50% frequency (Avg Wet)')
  })
  
  # Year type selections for target figures 
  output$select_target_yr_type_lg_customQ <- renderUI({
    shiny::selectInput(inputId = 'select_target_yr_type_lg_customQ',
                       label = 'Select Year Type Frequency Target (Percent of years where target should be met/exceeded)',
                       multiple = FALSE,
                       choices = c('10% frequency (Wet)','30% frequency (Mod Wet)','50% frequency (Avg Wet)','70% frequency (Avg Dry)','90% frequency (Mod Dry)','100% frequency (Dry)'),
                       selected = '50% frequency (Avg Wet)')
  })
  
  # Slider input for flow tolerance values
  output$select_flow_tolerance<- renderUI({
    sliderInput("select_flow_tolerance", "Flow Tolerance (cfs)",
                min = 0, max = 50, value = 5
    )
  })
  
  
  # Slider input for duration tolerance values
  output$select_duration_tolerance<- renderUI({
    sliderInput("select_duration_tolerance", "Duration Tolerance (days)",
                min = 0, max = 30, value = 0
    )
  })
  
  # Slider input for duration tolerance values
  output$select_ramping_tolerance<- renderUI({
    sliderInput("select_ramping_tolerance", "Ramping Rate Tolerance (CFS)",
                min = 0, max = 100, value = 0
    )
  })
  
  #input for blue mesa forecast
  
  output$blue_mesa_forecast<- renderUI({
    sliderInput("blue_mesa_forecast", "Blue Mesa Apr-Jul Inflow Forecast (kaf) ",
                min = 0, max = 1500, value = 500, step=50
    )
  })
  
  observeEvent(input$runButton_advSet, {
    ramp_tol <<- input$select_ramping_tolerance
    flow_tol <<- input$select_flow_tolerance
    duration_tol <<- input$select_duration_tolerance
  })
  
  #### User Defined Tables ###########
  
  dates <- seq(as.Date("2049/4/1"), by = "day", length.out = 365)
  
  userTable <- reactive({
    data = data.frame(
      Month = as.character(month(dates, label = TRUE)),
      Day = mday(dates),
      `Gunnison.BelowTunnel` = rep(0,365),
      `Gunnison Whitewater` = rep(0,365)#,
      #`Black Canyon Recreational Flow Targets` = rep(0,365),
      #`GGNCA Recreational Flow Targets` = rep(0,365),
      #`Lower Gunnison Recreational Flow Targets` = rep(0,365)
      
    )
  })
  userValues <- reactiveValues()
  
  observe({
    userValues$data <- userTable()
  })
  
  # #reset the table when the user requests it
  # observeEvent(input$resetButton_tunnel, {
  #   userValues$data <- userTable()
  # }) 
  # 
  
  #create a table of user-defined streamflow values
  observe({
    if(!is.null(input$customQ)){
      userValues$data <- as.data.frame(hot_to_r(input$customQ))
      output$customQ <- renderRHandsontable({
        rhandsontable(userValues$data, height = 500 ) %>%
          hot_table(stretchH = "all") %>% 
          hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
          #  hot_cols(fixedColumnsLeft = 4) %>%
          hot_rows(rowHeights = 25) %>%
          hot_col("Month", readOnly = TRUE) %>%
          hot_col("Day", readOnly = TRUE) %>%
          hot_col("Gunnison.BelowTunnel", format = "0.0") %>%
          hot_col("Gunnison.Whitewater", format = "0.0") %>%
        #  hot_col("Black.Canyon.Recreational.Flow.Targets", format = "0.0") %>%
        #  hot_col("GGNCA.Recreational.Flow.Targets", format = "0.0") %>%
        #  hot_col("Lower.Gunnison.Recreational.Flow.Targets", format = "0.0") %>%
          hot_validate_numeric(cols = 2, min = 0, max = 1000000)
      })
      outputOptions(output, "customQ", suspendWhenHidden = FALSE, priority = 1)
    }
  })
  
  output$customQ <- renderRHandsontable({
    rhandsontable(userValues$data, height = 500, stretchH = "all")
  })
  
  outputOptions(output, "customQ", suspendWhenHidden = FALSE, priority = 1)
  
  
  #### Create Q Record for Analysis ####
  # 
  # choice_Q_whitewater <-   eventReactive(input$evalButton, {
  #   Q_ww <- Q_whitewater %>%
  #     filter(water_year >= as.numeric(input$select_startyear) & water_year <= as.numeric(input$select_endyear)) %>% 
  #     mutate(date_type = 'Historical') 
  #   Q_ww
  #   
  # })
  # 
  choice_Q_whitewater_daily <-   eventReactive(input$evalButton, {
    Q_whitewater_daily %>%
      filter(water_year >= as.numeric(input$select_startyear) & water_year <= as.numeric(input$select_endyear)) %>% 
      mutate(date_type = 'Historical') 
    
  })
  
  choice_Q_tunnel_daily <- eventReactive(input$evalButton, {
    Q_tunnel_daily %>%
      filter(water_year >= as.numeric(input$select_startyear) & water_year <= as.numeric(input$select_endyear)) %>% 
      mutate(date_type = 'Historical')
    
  })
  # 
  # choice_Q_tunnel <- eventReactive(input$evalButton, {
  #   Q_tunnel %>%
  #     filter(water_year >= as.numeric(input$select_startyear) & water_year <= as.numeric(input$select_endyear)) %>% 
  #     mutate(date_type = 'Historical')
  #   
  # })
  # 
  
  
  year1 <- eventReactive(input$evalButton, {
    as.numeric(input$select_year1)
  })
  
  year2 <- eventReactive(input$evalButton, {
    as.numeric(input$select_year2)
  })
  
  start_year <<- eventReactive(input$evalButton, {
    as.numeric(input$select_startyear)
  })
  
  end_year <<- eventReactive(input$evalButton, {
    as.numeric(input$select_endyear)
  })
  
  ### Target Tables for Overview #######
  
  output$rec_meta_DT <-DT::renderDT({rec_targets_meta  %>%
      datatable(rownames = FALSE,filter = 'none',extensions = 'RowGroup',
                options=(list(pageLength = 15,rowsGroup = list(0),dom = 't')),
                caption = "Table 4: Recreational flow preferences identified for study reaches.")
  })
  
  output$BC_eflow <-DT::renderDT({bc_eflow_targets_meta  %>%
      datatable(rownames = FALSE,filter = 'none',extensions = 'RowGroup',
                options=(list(pageLength = 15,rowsGroup = list(0),dom = 't')),
                caption = 'Table 1: Environmental flow targets for the Black Canyon.')
  })
  
  output$GG_eflow <-DT::renderDT({gg_eflow_targets_meta  %>%
      datatable(rownames = FALSE,filter = 'none',extensions = 'RowGroup',
                options=(list(pageLength = 15,rowsGroup = list(0),dom = 't')),
                caption = 'Table 2: Environmental flow targets for the Gunnison Gorge.')
  })
  
  output$LG_eflow <-DT::renderDT({lg_eflow_targets_meta  %>%
      datatable(rownames = FALSE,filter = 'none',extensions = 'RowGroup',
                options=(list(pageLength = 15,rowsGroup = list(0),dom = 't')),
                caption = 'Table 4: Environmental flow targets for the Lower Gunnison.')
  })
  
  
  
  ### Env. Targets #########
  
  observe({
    
    ##### Black Canyon ####
    
    #flag by year where peak flow target was met
    black_canyon_spring_peak <<- 
      evaluate_peak_targets_bc_by_year_preLoad(
        Q_df=choice_Q_tunnel_daily(),
        roll_statistic='mean', start_month=5,
        start_day =01,end_month=6, end_day=30,flow_tolerance= flow_tol
      ) %>% 
      filter(water_year >= start_year() & water_year <= end_year()) 
    
    # annual minimum daily flow during high flow period. 
    start_month=05
    start_day =04
    end_month=07
    end_day=25
    high_flow_min_flow_Q<<- choice_Q_tunnel_daily() %>% 
      filter(date >= as.Date(paste(year(date), start_month, start_day, sep = "-")),
             date <= as.Date(paste(year(date), end_month, end_day, sep = "-"))) 
    
    # duration below High flow minimum flow 
    black_canyon_high_flow_min_raw<<-
      evaluate_min_flow_tgt_by_yr_type_bc(
        Q_df= choice_Q_tunnel_daily(),
        start_month=05,start_day =04,
        end_month=07, end_day=25,
        deficit_days_tolerance = duration_tol,flow_tolerance= flow_tol
      )
    
    high_flow_min_flow <<- high_flow_min_flow_Q%>% 
      group_by(water_year) %>% 
      summarize(minQ = min(q_cfs)) %>% 
      left_join(black_canyon_high_flow_min_raw %>% dplyr::select(water_year,bc_high_flow_minimum,high_min_flag))
    
    
    #Evaluate Baseflow Minimum Target
    black_canyon_baseflow_duration_target<<- tibble(
      target_min_flow =c(300),
      target_max_flow =c(Inf),
      target_days = c(365),
      target_goal = c(365),
      target_name=c('Baseflow Minimum Flow'),
      metric= 'Mean Duration Meeting Target (Days)')
   
    # annual minimum daily flow
    start_month=01
    start_day =01
    end_month=12
    end_day=31
    
    annual_min_flow_Q <<- choice_Q_tunnel_daily() %>% 
      filter(date >= as.Date(paste(year(date), start_month, start_day, sep = "-")),
             date <= as.Date(paste(year(date), end_month, end_day, sep = "-"))) 

    black_canyon_baseflow_avg_days_raw <<- evaluate_duration_in_target_range_bc_baseflow(
      Q_df=choice_Q_tunnel_daily(),
      target_meta_df=black_canyon_baseflow_duration_target,
      start_month=01,start_day =01,end_month=12, end_day=31,
      flow_tolerance=flow_tol
    ) %>%
      mutate(
        result = pct_days >= 100*(365 - duration_tol)/365,
        value=duration_days
      )
    annual_min_flow <<-annual_min_flow_Q %>% 
      group_by(water_year) %>% 
      summarize(minQ = min(q_cfs)) %>% 
      left_join(black_canyon_baseflow_avg_days_raw %>% dplyr::select(water_year,target_min_flow ,achievement_flag=result))
   
    #Evaluate ramping rate
    black_canyon_ramping_raw<<- evaluate_ramping_rate(
      Q_df=choice_Q_tunnel_daily(),
      ramp_tolerance = ramp_tol
    ) %>%
      mutate(
        target_name = 'Maximum Ramping Rate',
        value=days_blw_ramping_rate 
      )
    
  }) %>%
    bindEvent(input$evalButton)
  
  observe({
    
    ##### GGNCA ##############
    
    # Evaluation of Maximum Limits for Peak Flows Suitable For Trout 
    ggnca_max_flow_exceedance_duration_rainbow_raw <<-
      evaluate_duration_in_target_range(Q_df=choice_Q_tunnel_daily(), target_meta_df=ggnca_peak_duration_target,
                                        start_month=6,start_day =01,end_month=6, end_day=30,
                                        flow_tolerance=flow_tol) %>%
      mutate(
        target_name ='Rainbow Trout Max Flow',
        result = ifelse((duration_days - duration_tol)  <= 0, 'Met','Not Met'),
        value=duration_days
      )
    # 
    # ggnca_max_flow_exceedance_duration_rainbow_sum<<- ggnca_max_flow_exceedance_duration_rainbow_raw %>%
    #   mutate(target_name='') %>%
    #   summarize_evaluate_duration_in_target_range(target_meta_df =ggnca_peak_duration_target ) %>%
    #   mutate(
    #     target_name ='Rainbow Trout Max Flow',
    #     status = ifelse(result  <= (target_goal + duration_tol), 'Met','Not Met'),
    #   )
    # 
    ggnca_max_flow_exceedance_duration_brown_raw <<-
      evaluate_duration_in_target_range(Q_df=choice_Q_tunnel_daily(),target_meta_df=ggnca_peak_duration_target,
                                        start_month=4,start_day =15,end_month=6, end_day=01,
                                        flow_tolerance=5) %>%
      mutate(
        target_name ='Brown Trout Max Flow',
        result = ifelse( (duration_days - duration_tol)  <=0, 'Met','Not Met'),
        value=duration_days
      )
    
    # ggnca_max_flow_exceedance_duration_brown_sum <<- ggnca_max_flow_exceedance_duration_brown_raw %>%
    #   mutate(target_name='') %>%
    #   summarize_evaluate_duration_in_target_range(target_meta_df =ggnca_peak_duration_target ) %>%
    #   mutate(target_name ='Brown Trout Max Flow',
    #          status = ifelse(result <=target_goal + duration_tol, 'Met','Not Met'),
    #   )
    
    ggnca_fish_max_flows_raw <<- ggnca_max_flow_exceedance_duration_rainbow_raw %>%
      bind_rows(ggnca_max_flow_exceedance_duration_brown_raw) %>%
      mutate(target_type = 'Maximum Spring Flow for Fish')
    
    # ggnca_fish_max_flows_sum <<- ggnca_max_flow_exceedance_duration_rainbow_sum %>%
    #   bind_rows(ggnca_max_flow_exceedance_duration_brown_sum) %>%
    #   mutate(target_type = 'Maximum Spring Flow for Fish')
    
    #Evaluation of Ideal Flow Range for TrouT
    
    ggnca_fish_range_raw <<- evaluate_duration_in_target_range(Q_df=choice_Q_tunnel_daily(),target_meta_df=ggnca_range_duration_target,
                                                              start_month=01,start_day =01,end_month=12, end_day=31,
                                                              flow_tolerance=flow_tol) %>%
      mutate(result = ifelse((duration_days +duration_tol) >=186, 'Met','Not Met'),
             target_type = 'Trout Habitat Flows',
             value=duration_days)
    
    # ggnca_fish_range_sum <<-ggnca_fish_range_raw %>%
    #   summarize_evaluate_duration_in_target_range(target_meta_df =ggnca_range_duration_target ) %>%
    #   mutate(status = ifelse(result >=target_goal, 'Met','Not Met'),
    #          target_type = 'Trout Habitat Flows')
    
    #Evaluation of Baseflow Minimum Flow
    ggnca_baseflow_avg_days_raw <<- evaluate_duration_in_target_range(Q_df=choice_Q_tunnel_daily(),target_meta_df=ggnca_baseflow_duration_target,
                                                                     start_month=01,start_day =01,end_month=12, end_day=31,
                                                                     flow_tolerance=flow_tol)%>%
      mutate(result = ifelse( (duration_days +duration_tol) >=365, 'Met','Not Met'),
             target_type = 'Baseflow',
             value=duration_days)
    # 
    # ggnca_baseflow_avg_days_sum <<- ggnca_baseflow_avg_days_raw %>%
    #   summarize_evaluate_duration_in_target_range(target_meta_df =ggnca_baseflow_duration_target) %>%
    #   mutate(status = ifelse((result + duration_tol) >=365, 'Met','Not Met'),
    #          target_type = 'Baseflow')
    
    
    # ggnca_df_sum <<- reactive({
    #   ggnca_fish_max_flows_sum %>%
    #     bind_rows(ggnca_fish_range_sum) %>%
    #     bind_rows(ggnca_baseflow_avg_days_sum) %>%
    #     dplyr::select(Type= target_type, Name = target_name, Metric = metric,`Target Goal`=target_goal, Result= result, Status =status)
    # })
    
    # output$GG_EF_results <-DT::renderDT({ggnca_df_sum() %>%
    #     datatable(rownames = FALSE,filter = 'top',extensions = 'RowGroup',
    #               options=(list(pageLength = 15,rowsGroup = list(0)))) %>%
    #     formatStyle(columns = 6,
    #                 color = styleEqual(levels=c('Not Met','Met'), values = c("red", "blue")),
    #                 fontWeight = "bold")
    # })
    # 
    # gg_df_plot<<- reactive({
    #   ggnca_fish_max_flows_raw %>%
    #     bind_rows(ggnca_fish_range_raw,ggnca_baseflow_avg_days_raw) %>%
    #     dplyr::select(Type= target_type, Name = target_name,water_year = water_year,
    #                   Metric = metric, Result= result,Value=value) %>%
    #     mutate(
    #       Type = factor(Type),
    #       Name = fct_rev(factor(Name, levels=c('Rainbow Trout Max Flow','Brown Trout Max Flow',
    #                                            'Ideal Flow Range','Baseflow Minimum Flow'))))
    # })
  }) %>%
    bindEvent(input$evalButton)
  
  observe({
    ##### Lower Gunnison ####
    # 
    # whitewater_spring_peak_sum <<- evaluate_peak_targets(Q_df=choice_Q_whitewater_daily(),
    #                                                    target_meta_df=whitewater_peak_target_meta,
    #                                                    peak_window_size=1, roll_statistic='mean',
    #                                                    start_month=5,start_day =01,end_month=6, end_day=30,
    #                                                    flow_tolerance=flow_tol)%>%
    #   mutate(
    #     metric= 'Proportion of Years Target is Met (%)',
    #     status = ifelse(result >=target_goal, 'Met','Not Met'),
    #     target_type = '24 Hour Instantaneous Peak Flow')
    
    whitewater_spring_peak_raw <<- evaluate_peak_targets_for_plot(Q_df=choice_Q_whitewater_daily(),
                                                                 target_meta_df=whitewater_peak_target_meta,
                                                                 peak_window_size=1, roll_statistic='mean',
                                                                 start_month=5,start_day =01,end_month=6, end_day=30,
                                                                 flow_tolerance=flow_tol) %>% 
      dplyr::select(water_year,value) %>% distinct()
    
   # print(whitewater_spring_peak_raw)
    whitewater_spring_peak_raw_yrtype <<- whitewater_spring_peak_raw %>% 
      dplyr::select(water_year,value) %>% distinct() %>% 
      left_join(black_canyon_high_flow_min_raw %>% ungroup() %>% dplyr::select(water_year,year_type)) %>% 
      mutate(
        achievement_flag= case_when(
          year_type == 'Dry' & value >900 ~ TRUE,
          year_type == 'Mod Dry' & value >=2600 ~ TRUE,
          year_type == 'Avg Dry' & value >=8070 ~ TRUE,
          year_type == 'Avg Wet' & value >=14350 ~ TRUE,
          year_type == 'Mod Wet' & value >=14350   ~ TRUE,
          year_type == 'Wet' & value >=15000  ~ TRUE,
          .default = FALSE
        ),
        min_target= case_when(
          year_type == 'Dry'~ 900,
          year_type == 'Mod Dry' ~2600 ,
          year_type == 'Avg Dry' ~8070 ,
          year_type == 'Avg Wet' ~14350,
          year_type == 'Mod Wet' ~14350 ,
          year_type == 'Wet' & value ~ 15000,
          .default = NA
        ),
      )
    
    whitewater_high_flow_duration_raw <<-evaluate_duration_in_target_range(Q_df=choice_Q_whitewater_daily(),
                                                                          target_meta_df=whitewater_peak_duration_target,
                                                                          start_month=05,start_day =01,
                                                                          end_month=06, end_day=30,
                                                                          flow_tolerance = flow_tol)#%>%
      # left_join(whitewater_peak_duration_target) %>%
      # mutate(
      #   metric= 'Mean Duration of Flows Above Target',
      #   result = ifelse((duration_days +flow_tol) >=target_goal, 'Met','Not Met'),
      #   target_type = 'High Flow Duration',
      #   value=duration_days)
      # 
    whitewater_high_flow_duration_yrType<<- whitewater_high_flow_duration_raw%>% 
      dplyr::select(water_year,duration_days,target_name) %>% 
      mutate(target_name = ifelse(target_name %in% c('Maintenance (>8070 cfs)','Improvement (>8070 cfs)'), 
                                  'Maintenance (>8070 cfs)','Improvement (>14350)')) %>%  
                                    distinct() %>% 
      left_join(black_canyon_high_flow_min_raw %>% ungroup() %>% dplyr::select(water_year,year_type)) %>% 
      pivot_wider(names_from = target_name, values_from = duration_days) %>% 
      mutate(
        maintenance_achievement_flag= case_when(
          year_type == 'Dry' & `Maintenance (>8070 cfs)` >=0 ~ TRUE,
          year_type == 'Mod Dry' & `Maintenance (>8070 cfs)` >=0~ TRUE,
          year_type == 'Avg Dry' & `Maintenance (>8070 cfs)` >=10~ TRUE,
          year_type == 'Avg Wet' & `Maintenance (>8070 cfs)` >=20 ~ TRUE,
          year_type == 'Mod Wet' & `Maintenance (>8070 cfs)` >=40   ~ TRUE,
          year_type == 'Wet' & `Maintenance (>8070 cfs)` >=60  ~ TRUE,
          .default = FALSE
        ),
        improvement_achievement_flag= case_when(
          year_type == 'Dry' & `Improvement (>14350)` >=0 ~ TRUE,
          year_type == 'Mod Dry' & `Improvement (>14350)` >=0~ TRUE,
          year_type == 'Avg Dry' & `Improvement (>14350)` >=0~ TRUE,
          year_type == 'Avg Wet' & `Improvement (>14350)` >=2 ~ TRUE,
          year_type == 'Mod Wet' & `Improvement (>14350)` >=10   ~ TRUE,
          year_type == 'Wet' & `Improvement (>14350)` >=15  ~ TRUE,
          .default = FALSE
        ),
        maintenance_min_target = case_when(
          year_type == 'Dry' ~ 0,
          year_type == 'Mod Dry' ~ 0,
          year_type == 'Avg Dry' ~ 10,
          year_type == 'Avg Wet' ~20,
          year_type == 'Mod Wet'~40,
          year_type == 'Wet' ~60,
          .default = NA
        ),
        improvement_min_target = case_when(
          year_type == 'Dry' ~ 0,
          year_type == 'Mod Dry' ~ 0,
          year_type == 'Avg Dry' ~ 0,
          year_type == 'Avg Wet' ~2,
          year_type == 'Mod Wet'~10,
          year_type == 'Wet' ~15,
          .default = NA
        )
      )
    
    # whitewater_high_flow_duration_sum <<- whitewater_high_flow_duration_raw %>%
    #   summarize_evaluate_duration_in_target_range(target_meta_df =whitewater_peak_duration_target ) %>%
    #   mutate(
    #     metric= 'Mean Duration of Flows Above Target',
    #     status = ifelse((result = flow_tol) >=target_goal, 'Met','Not Met'),
    #     target_type = 'High Flow Duration')
    
    whitewater_base_eval_raw <<- evaluate_min_flow_tgt_by_yr_type_lg_baseflow(Q_df=choice_Q_whitewater_daily(), target_meta_df=whitewater_base_meta,
                                                                start_month=1,start_day =01,end_month=12,
                                                                end_day=31, deficit_days_tolerance = duration_tol,
                                                                flow_tolerance= flow_tol) %>%
      mutate(metric= 'Proportion of Years Target is Met (%)' )%>%
      mutate(
        metric= 'Proportion of Years Target is Met (%)',
        result = ifelse(result == TRUE, 'Met','Not Met'),
        target_type = 'Baseflows') %>%
      mutate(target_name = case_when(
        target_name == "wet & mod_wet" ~ 'Baseflow Minimums (Wet / Mod Wet Year Target)',
        target_name == "avg wet & avg dry" ~ 'Baseflow Minimums (Avg Wet / Avg Dry Year Target)',
        target_name == "mod dry" ~ 'Baseflow Minimums (Mod Dry Year Target)',
        target_name == "dry" ~ 'Baseflow Minimums (Dry Year Target)'),
        value=blw_base_flow
      )
    
    # whitewater_base_eval_sum <<- whitewater_base_eval_raw %>%
    #   mutate(result = ifelse(result == 'Met', TRUE,FALSE)) %>%
    #   summarize_evaluate_min_flow_tgt_by_yr_type() %>%
    #   mutate(metric= 'Proportion of Years Target is Met (%)' )%>%
    #   mutate(
    #     metric= 'Proportion of Years Target is Met (%)',
    #     status = ifelse(result >=target_goal, 'Met','Not Met'),
    #     target_type = 'Baseflows')# %>%
    # mutate(target_name = case_when(
    #   target_name == "wet & mod_wet" ~ 'Baseflow Minimum (Wet / Mod Wet Year Target)',
    #   target_name == "avg wet & avg dry" ~ 'Baseflow Minimum (Avg Wet / Avg Dry Year Target)',
    #   target_name == "mod dry" ~ 'Baseflow Minimum (Mod Dry Year Target)',
    #   target_name == "dry" ~ 'Baseflow Minimum (Dry Year Target)')
    # )
    
    whitewater_base_eval_raw_yrType <<- whitewater_base_eval_raw %>% 
      ungroup() %>% 
      left_join(black_canyon_high_flow_min_raw %>% ungroup() %>% dplyr::select(water_year,year_type)) %>% 
      mutate(keep = ifelse(
        target_name =='Baseflow Minimums (Avg Wet / Avg Dry Year Target)' & year_type %in% c('Avg Wet','Avg Dry') |
        target_name =='Baseflow Minimums (Wet / Mod Wet Year Target)' & year_type %in% c('Mod Wet','Wet')|
        target_name =='Baseflow Minimums (Mod Dry Year Target)' & year_type %in% c('Mod Dry') |
        target_name =='Baseflow Minimums (Dry Year Target)' & year_type %in% c('Dry'), 'Keep' ,'Drop')
      ) %>% 
      filter(keep =='Keep') %>% 
      dplyr::select(water_year, blw_base_flow,min_flow,year_type) %>% distinct() %>% 
      mutate(flag_achievement = ifelse(blw_base_flow<=(0+duration_tol),'TRUE','FALSE'))
      
  }) %>%
    bindEvent(input$evalButton)
  
  ### Recreation ##############
  
  # classify boatable days
  
  observe({
    tunnel_BDA_class<<- 
     choice_Q_tunnel_daily() %>% 
     # filter(month %in% 5:10) %>%     
      mutate(
      black_canyon_WW = case_when(
        q_cfs < 600 ~ 'Not Acceptable',
        q_cfs >= 600 & q_cfs < 800 ~ 'Lower Acceptable',
        q_cfs >= 800 & q_cfs < 1600 ~ 'Optimal',
        q_cfs >= 1600 & q_cfs < 3000 ~ 'Upper Acceptable',
        q_cfs > 3000 ~'Not Acceptable'
      ),
      gunnison_gorge_WW = case_when(
        q_cfs <600 ~ 'Not Acceptable',
        q_cfs >=600 & q_cfs < 800 ~ 'Lower Acceptable',
        q_cfs >=800 & q_cfs < 3000 ~ 'Optimal',
        q_cfs >=3000 & q_cfs < 15000 ~ 'Upper Acceptable',
        q_cfs > 15000 ~'Not Acceptable'
      ),
      gunnison_gorge_fishing= case_when(
        q_cfs <400 ~ 'Not Acceptable',
        q_cfs >=400 & q_cfs < 500 ~ 'Lower Acceptable',
        q_cfs >=500 & q_cfs < 1000 ~ 'Optimal',
        q_cfs >=1000 & q_cfs < 3000 ~ 'Upper Acceptable',
        q_cfs > 3000 ~'Not Acceptable'
      )
    )
    
    whitewater_BDA_class<<- choice_Q_whitewater_daily() %>% 
     # filter(month %in% 5:10) %>%     
      mutate(
        lg_WW = case_when(
          q_cfs < 800 ~ 'Not Acceptable',
          q_cfs >= 800 & q_cfs < 1000 ~ 'Lower Acceptable',
          q_cfs >= 1000 & q_cfs < 15000 ~ 'Optimal',
          q_cfs >= 15000 & q_cfs < 20000 ~ 'Upper Acceptable'
        )
      )

  }) %>%
    bindEvent(input$evalButton)
  
  
  #long-term averages based on 1975 to 2021
  
  observe({
    
    ##### Black Canyon ##############
    
    # 
    # black_canyon_rec<<- evaluate_duration_in_target_range(Q_df=choice_Q_tunnel_daily(),
    #                                                      target_meta_df=black_canyon_recreation_targets,
    #                                                      start_month=05,start_day =01,end_month=10, end_day=31,
    #                                                      flow_tolerance=0) %>%
    #   summarize_evaluate_duration_in_target_range(target_meta_df =black_canyon_recreation_targets ) %>%
    #   mutate(target_name =
    #            factor(target_name, levels=c('Lower Acceptable',
    #                                         'Optimal',
    #                                         'Upper Acceptable',
    #                                         'Total')),
    #          target_type='Whitewater Recreation',
    #          status = ifelse(result >= target_goal, 'Above or Equal', 'Below')
    #   ) %>% arrange(target_name) %>%
    #   dplyr::select(Type= target_type, Name = target_name, Metric = metric,`Long Term Average`=target_goal, Result= result, Status =status)
    
    black_canyon_rec_raw<<- evaluate_duration_in_target_range(Q_df=choice_Q_tunnel_daily(),
                                                             target_meta_df=black_canyon_recreation_targets,
                                                             start_month=05,start_day =01,end_month=10, end_day=31,
                                                             flow_tolerance=0) %>%
      left_join(black_canyon_recreation_targets) %>% 
      mutate(target_name =
               factor(target_name, levels=c('Lower Acceptable',
                                            'Optimal',
                                            'Upper Acceptable',
                                            'Total')),
             target_type='Whitewater Recreation',
             status = ifelse(duration_days >= target_goal, 'Above or Equal', 'Below'),
             value= duration_days
      ) %>% arrange(target_name) %>%
      dplyr::select(water_year ,Type= target_type, Name = target_name, Metric = metric,`Long Term Average`=target_goal, Result= duration_days , Status =status,Value=value)
    
    # 
    # output$BC_REC_results <-DT::renderDT({black_canyon_rec %>%
    #     datatable(rownames = FALSE,filter = 'top',extensions = 'RowGroup',
    #               options=(list(pageLength = 15,rowsGroup = list(0)))) %>%
    #     formatStyle(columns = 6,
    #                 color = styleEqual(levels=c('Below','Above or Equal'), values = c("red", "green")),
    #                 fontWeight = "bold")},
    # )
    
    
    
    # bc_df_rec_plot<- reactive({
    #   p <- black_canyon_rec_raw
    #   
    # })
    # 
    # output$BC_results_plot <-renderGirafe({
    #   
    #   plot_tile_plot(x=bc_df_plot(),y=bc_df_rec_plot())
    # })
    # 
    # output$BC_sum_text <- renderUI({
    #   
    #   df_eft <- bc_df()
    #   create_bc_text_summary(EFT = df_eft, REC= black_canyon_rec)
    #   
    # })
    
    #### GGNNA ##############
    
    # ggnca_rec<<- evaluate_duration_in_target_range(Q_df=choice_Q_tunnel_daily(),
    #                                               target_meta_df=ggcna_recreation_targets,
    #                                               start_month=05,start_day =01,end_month=10, end_day=31,
    #                                               flow_tolerance=0) %>%
    #   summarize_evaluate_duration_in_target_range(target_meta_df =ggcna_recreation_targets ) %>%
    #   mutate(
    #     target_name =
    #       factor(target_name,levels=c('WW Lower Acceptable','WW Optimal',
    #                                   'WW Upper Acceptable', 'WW Total',
    #                                   'Ang Lower Acceptable','Ang Optimal',
    #                                   'Ang Upper Acceptable', 'Ang Total)',
    #                                   'Max Flow','Max'))) %>% arrange(target_name) %>%
    #   mutate(
    #     
    #     target_type= c('Whitewater Recreation','Whitewater Recreation',
    #                    'Whitewater Recreation', 'Whitewater Recreation',
    #                    'Angling','Angling ',
    #                    'Angling', 'Angling',
    #                    'Camping/Trail Access','Safety'),
    #     
    #     target_name = factor(target_name,labels=c('Lower Acceptable','Optimal',
    #                                               'Upper Acceptable', 'Total',
    #                                               'Lower Acceptable','Optimal',
    #                                               'Upper Acceptable', 'Total',
    #                                               'Max Flow','Max')),
    #     
    #     status = ifelse(result >= target_goal, 'Above or Equal', 'Below')
    #     
    #   ) %>% arrange(target_type,target_name) %>%
    #   dplyr::select(Type= target_type, Name = target_name, Metric = metric,`Long Term Average`=target_goal, Result= result, Status =status) %>% 
    #   arrange(Type) %>% 
    #   filter(Type != 'Safety')   
    #   
    # 
    # 
    # output$GG_REC_results <-DT::renderDT({
    #   ggnca_rec %>%
    #     filter(Type != 'Safety') %>% 
    #     datatable(rownames = FALSE,filter = 'top',extensions = 'RowGroup',
    #               options=(list(pageLength = 15,rowsGroup = list(0)))) %>%
    #     formatStyle(columns = 6,
    #                 color = styleEqual(levels=c('Below','Above or Equal'), values = c("red", "green")),
    #                 fontWeight = "bold")},
    # )
    # 
    #results by year for tile table
    ggnca_rec_raw<<- evaluate_duration_in_target_range(Q_df=choice_Q_tunnel_daily(),
                                                      target_meta_df=ggcna_recreation_targets,
                                                      start_month=05,start_day =01,end_month=10, end_day=31,
                                                      flow_tolerance=0) %>%
      left_join(ggcna_recreation_targets) %>%
      mutate(
        target_name = ifelse(target_name=='Max Flow','Camping/Trails Max Flow', target_name),
        target_name =
          factor(target_name,levels=c('WW Lower Acceptable','WW Optimal',
                                      'WW Upper Acceptable', 'WW Total',
                                      'Ang Lower Acceptable','Ang Optimal',
                                      'Ang Upper Acceptable', 'Ang Total)',
                                      'Camping/Trails Max Flow','Max'))) %>% arrange(target_name) %>%
      mutate(
        status = ifelse(duration_days >= target_goal , 'Above or Equal', 'Below')
      ) %>% 
      arrange(target_type,target_name) %>%
      arrange(target_name) %>%
      filter(target_name != 'Max') %>%  #removing due to updated guidance that this target is no longer valid.
      dplyr::select(water_year ,Type= target_type, Name = target_name, Metric = metric,`Long Term Average`=target_goal, Result= duration_days , Status =status,Value=duration_days)
    
    # ggcna_df_rec_plot<- reactive({
    #   p <- ggnca_rec_raw
    #   
    # }) 
    # 
    # output$GG_results_plot <-renderGirafe({
    #   
    #   plot_tile_plot(x=gg_df_plot(),y=ggcna_df_rec_plot())
    # })
    # 
    # 
    # output$GG_sum_text <- renderUI({
    #   
    #   gg_eft <- ggnca_df_sum()
    #   
    #   create_gg_text_summary(EFT = gg_eft, REC= ggnca_rec)
    #   
    # })
    # 
    
    #### LOWER GUNNSION #####
    
    # lower_gunnison_rec<- evaluate_duration_in_target_range(Q_df=choice_Q_whitewater_daily(),
    #                                                        target_meta_df=lower_gunnison_recreation_targets,
    #                                                        start_month=01,start_day =01,end_month=12, end_day=31,flow_tolerance=0) %>%
    #   summarize_evaluate_duration_in_target_range(target_meta_df =lower_gunnison_recreation_targets ) %>%
    #   mutate(target_name =
    #            factor(target_name, levels=c('Lower Acceptable',
    #                                         'Optimal',
    #                                         'Upper Acceptable',
    #                                         'Total'))
    #   ) %>% arrange(target_name) %>%
    #   mutate(
    #     status = ifelse(result >= target_goal, 'Above or Equal', 'Below'),
    #     target_type='Whitewater Recreation') %>%
    #   dplyr::select(Type= target_type, Name = target_name, Metric = metric,`Long Term Average`=target_goal, Result= result, Status =status)
    # 
    lower_gunnison_rec_raw<<- evaluate_duration_in_target_range(Q_df=choice_Q_whitewater_daily(),
                                                               target_meta_df=lower_gunnison_recreation_targets,
                                                               start_month=01,start_day =01,end_month=12, end_day=31,
                                                               flow_tolerance=0) %>%
      left_join(lower_gunnison_recreation_targets) %>%
      mutate(target_name =
               factor(target_name, levels=c('Lower Acceptable',
                                            'Optimal',
                                            'Upper Acceptable',
                                            'Total')),
             target_type='Whitewater Recreation',
             status = ifelse(duration_days >= target_goal, 'Above or Equal', 'Below'),
             value= duration_days
      ) %>% arrange(target_name) %>%
      dplyr::select(water_year ,Type= target_type, Name = target_name, Metric = metric,`Long Term Average`=target_goal, Result= duration_days , Status =status,Value=value)
    
    # output$LG_REC_results <-DT::renderDT({lower_gunnison_rec %>%
    #     datatable(rownames = FALSE,filter = 'top',extensions = 'RowGroup',
    #               options=(list(pageLength = 15,rowsGroup = list(0)))) %>%
    #     formatStyle(columns = 6,
    #                 color = styleEqual(levels=c('Below','Above or Equal'), values = c("red", "green")),
    #                 fontWeight = "bold")},
    #     
    # )
    # 
    # lg_df_rec_plot<- reactive({
    #   p <- lower_gunnison_rec_raw
    # })
    # 
    # output$LG_sum_text <- renderUI({
    #   
    #   lg_eft <- whitewater_df()
    #   
    #   create_lg_text_summary(EFT = lg_eft, REC= lower_gunnison_rec)
    #   
    # })
    # 
    # output$LG_results_plot <-renderGirafe({
    #   plot_tile_plot(x=lg_df_plot(),y=lg_df_rec_plot())
    # })
    # 
    
  }) %>%
    bindEvent(input$evalButton)
  
  
  #### Hydrographs and flow target figures ############
  
  observe({
    ## Tunnel Flows
     Q_dat_tunnel<<- #choice_Q_tunnel_daily() %>%
      Q_tunnel_daily %>%   
      mutate(
        doy = get_waterYearDay(date, wyMonth =4L),
        fake_date = as.Date(doy-1, origin= '2013-04-01'),
        # fake_datetime=ymd_hm(paste0(fake_date,' ',hour(datetime),':',
        #                             minute(datetime)), tz='America/Denver')
        fake_datetime=fake_date
      )
    
    Q_dat_tunnel_daily<<- choice_Q_tunnel_daily() %>%
      mutate(
        doy = get_waterYearDay(date, wyMonth =4L),
        fake_date = as.Date(doy, origin= '2013-04-01')
      )
    
    year1_tunnel <<-
      Q_dat_tunnel %>%
      #filter(water_year ==2018)
      filter(water_year == year1())
    
    year2_tunnel <<-
      Q_dat_tunnel %>%
      #filter(water_year ==2023)
      filter(water_year == year2())
    
    # Create version that includes targets
    output$BC_hydro_plot<- renderPlot({

      #Create Target Timeseries for Both Years
      peak_targets_year1 <<- bc_peak_targets_by_year %>%
       # filter(water_year %in% c(2018))
       filter(water_year == year1())

      year1_bc_targets <<- year1_tunnel %>%
        mutate(
          `Peak Target` =
            # ifelse(fake_datetime==ymd_hm('2014/06/01 12:00', tz='America/Denver'),
            #        peak_targets_year1$bc_peak_flow_target[1], NA),
          ifelse(fake_date==ymd('2013/06/01', tz='America/Denver'),
                 peak_targets_year1$bc_peak_flow_target[1], NA),
          `High Flow Minimum`=
            ifelse(fake_date>=ymd('2013/05/01') & fake_date<=ymd('2013/07/25'),
                   peak_targets_year1$bc_high_flow_minimum[1], NA),
          Baseflow= 300
        ) %>%
        pivot_longer(cols=c(`Peak Target`:Baseflow))

      peak_targets_year2 <<- bc_peak_targets_by_year %>%
        #filter(water_year %in% c(2023))
        filter(water_year %in% c(year2()))

      year2_bc_targets <<- year2_tunnel %>%
        mutate(
          `Peak Target` =
            # ifelse(fake_datetime==ymd_hm('2014/06/01 12:00', tz='America/Denver'),
            #        peak_targets_year2$bc_peak_flow_target[1], NA),
            ifelse(fake_date==ymd('2013/06/01', tz='America/Denver'),
                   peak_targets_year2$bc_peak_flow_target[1], NA),
          `High Flow Minimum`=
            ifelse(fake_date>=ymd('2013/05/01') & fake_date<=ymd('2013/07/25'),
                   peak_targets_year2$bc_high_flow_minimum[1], NA),
          Baseflow= 300
        )%>%
        pivot_longer(cols=c(`Peak Target`:Baseflow))

      line_key<- tibble(
        name = year2_bc_targets$name %>% unique(.),
        line_colors= c('yellow','#01665e','orange'),
        line_types=c('solid','solid','solid'),
      ) %>%
        filter(name != 'Peak Target') %>%
        mutate(
          name = factor(name, levels=c('Baseflow','High Flow Minimum','Peak Target','Hydrograph'))
        ) %>%
        arrange(name)

      flw_target_dat_black_canyon_fil <<- flw_target_dat_black_canyon %>%
        filter(name !='hydrograph') %>%
        filter(year_type=='Wet') %>%
        dplyr::select(date,fake_date, doy, month,min_acceptable:max_acceptable) %>%
        right_join(year1_tunnel  %>% ungroup() %>% dplyr::select(fake_date, fake_datetime))

      p<- plot_BC_hydro_plot_new(Q= year1_tunnel,linekey = line_key,dat=flw_target_dat_black_canyon_fil, yr1 = year1_tunnel,yr2 =year2_tunnel )
      p
    })

    line_key<- tibble(
      name = (flw_target_dat_gunnison_gorge$name %>% unique(.))[1:5],
      line_colors= c('orange','#6a3d9a','#b15928','#33a02c','#33a02c'),
      type = c('Aquatic','Aquatic','Aquatic','Aquatic','Aquatic'),

      line_types=c('solid','solid','solid','solid','solid'),
      labels=c('Baseflow','Rainbow Recruitment Max','Brown Recruitment Max','Optimal Trout Min','Optimal Trout Max')
    )
    output$GG_hydro_plot<- renderPlot({
      flw_target_dat_gunnison_gorge_fil <<- flw_target_dat_gunnison_gorge #%>%
      # filter(year_type   ==as.character(input$select_target_yr_type_bc)) %>%
      #filter(name !='hydrograph')
      
      p<- plot_GG_hydro_plot(Q= year1_tunnel,linekey = line_key,dat=flw_target_dat_gunnison_gorge_fil, yr1 = year1_tunnel,yr =year2_tunnel )
      
      p
    })
    
    #whitewater
    Q_dat_whitewater<- choice_Q_whitewater_daily() %>%
     # Q_whitewater_daily %>% 
      mutate(doy = get_waterYearDay(date, wyMonth = 4L)) %>%
      mutate(fake_date = as.Date(doy, origin= '2013-04-01'))
    
    year1_ww <-
      Q_dat_whitewater %>%
      filter(water_year == year1()) %>% 
      #filter(water_year == 2008) %>% 
      left_join(historical_year_types) %>% 
      mutate(water_year = paste0(water_year, ' (',year_type,')'))
    
    year2_ww <-
      Q_dat_whitewater %>%
      filter(water_year == year2())%>% 
      #filter(water_year == 2023) %>%
      left_join(historical_year_types)%>% 
      mutate(water_year = paste0(water_year, ' (',year_type,')'))
    
    output$LG_hydro_plot<- renderPlot({
      
      flw_target_dat_whitewater_fil <- flw_target_dat_whitewater %>%
        mutate(year_type = case_when(
          year_type == 'Wet' ~ '10% frequency (Wet)',
          year_type == 'Mod Wet' ~'30% frequency (Mod Wet)',
          year_type == 'Avg Wet' ~ '50% frequency (Avg Wet)',
          year_type == 'Avg Dry'~ '70% frequency (Avg Dry)',
          year_type == 'Mod Dry' ~ '90% frequency (Mod Dry)',
          year_type == 'Dry' ~ '100% frequency (Dry)')
        ) %>% 
        filter(!(name %in% c('hydrograph_improvement','hydrograph_maintenance')))%>% 
        mutate(fake_date_customQ= if_else(month>3, fake_date_customQ, fake_date_customQ+years(1)))
      
      if(length(input$select_target_yr_type_lg)==0){
        flw_target_dat_whitewater_fil <- flw_target_dat_whitewater_fil %>% 
          filter(year_type=='50% frequency (Avg Wet)')
      } else{
        flw_target_dat_whitewater_fil <- flw_target_dat_whitewater_fil %>% 
          filter(year_type   ==as.character(input$select_target_yr_type_lg))
      }
       
      line_key<- tibble(
        name = flw_target_dat_whitewater$name %>% unique(.),
        line_colors= c('orange','#01665e','#8c510a','#5ab4ac','#d8b365','yellow','black','black'),
        line_types=c('solid','solid','solid','solid','solid','solid','dashed','dotted'),
        labels=c('Baseflow','Half Fullbank Maintenance','Fullbank Maintenance',
                 'Half Fullbank Improvement','Fullbank Improvement',
                 'Instaneous Peak','Maintenance Hydrograph','Improvement Hydrograph')
      ) %>%
        filter(name != 'peak') %>%
        mutate(
          name = factor(name, levels=c('base', 'half_fullbank_improvement','half_fullbank_maintenance',
                                       'fullbank_improvement', 'fullbank_maintenance',
                                       'hydrograph_maintenance', 'hydrograph_improvement'))
        ) %>%
        arrange(name)
      
      p<- ggplot(Q_dat_whitewater)+
        geom_blank( aes(fake_date, q_cfs))+
        geom_ribbon(
          data=flw_target_dat_whitewater_fil %>%
            mutate(fill_name = 'low') %>%
            filter(month %in% 5:10),
          aes(fake_date,ymin=min_acceptable,ymax=min_optimal,fill=fill_name), alpha=.2
        ) +
        geom_ribbon(
          data=flw_target_dat_whitewater_fil %>%
            mutate(fill_name = 'optimal') %>%
            filter(month %in% 5:10),
          aes(fake_date,ymin=min_optimal,ymax=max_optimal,fill=fill_name), alpha=.2
        ) +
        geom_ribbon(
          data=flw_target_dat_whitewater_fil %>%
            mutate(fill_name = 'high') %>%
            filter(month %in% 5:10),
          aes(fake_date,ymin=max_optimal,ymax=max_acceptable,fill=fill_name), alpha=.2
        ) +
        scale_fill_manual(guide='legend',values=c('red','blue','green','black'),
                          breaks=c('low','optimal','high'),
                          labels = c('Low Acceptable','Optimal','High Acceptable'),name='Boating Recreational Preferences')+
        new_scale_fill() +
        # add flow targets
        geom_line(data=flw_target_dat_whitewater_fil %>% filter(name !='peak'),aes(fake_date, y=value, color=name, linetype=name),size=1.2)+
        scale_color_manual(guide='legend',values=line_key$line_colors,breaks=line_key$name,
                           labels=line_key$labels,name='Environmental Targets')+
        scale_linetype_manual(guide='legend',values=line_key$line_types,
                              breaks=line_key$name, labels=line_key$labels,name='Environmental Targets')+
        
        new_scale_fill() +
        geom_point(data=flw_target_dat_whitewater_fil %>% filter(name =='peak'),aes(fake_date, value,fill=name),shape =23, size=5, alpha=.7)+
        scale_fill_manual(guide='legend',values='yellow',name='Peak Target',label='Peak') +
        scale_x_date(date_labels = '%b', date_breaks = '1 months') +
        new_scale_color() +
        geom_line(data = year1_ww,  linetype='dotdash',
                  #col='#1b9e77',
                  size=1.2,
                  aes(fake_date, q_cfs,group=water_year, col=factor(water_year)
                  ))+
        geom_line(data = year2_ww, linetype='dotdash',
                  #col='#d95f02',
                  size=1.2,
                  aes(fake_date, q_cfs,group=water_year,col=factor(water_year)))+
        scale_color_manual(guide='legend',values=c('black','red'),name='Historical Hydrograph')+
        theme_minimal()+
        scale_y_log10(breaks=c(100, 250, 500,750,1000,1500,2500,5000,10000 ))+
        labs(x='Month', y = "Flow (cfs)")
      
      p
      
      #ggplotly(p, tooltip = 'text')
    })
    
  }) %>%
    bindEvent(input$evalButton)
  
  
  #### Black Canyon Target Graphs  #####
  observe({
    output$BC_tgt_plot<- renderPlot({
  
      peak_target_plot<- tibble(
        Inflow_af = c(0, 372000, 715000, 925000, 1001000, 1050000, 1500000), 
        peak_flow = c(482.95, 1018.63, 6244.94, 6512.88,7609.29, 11034, 15840)
      )
      
      tgts<- peak_targets_year1 %>% 
        bind_rows(peak_targets_year2)
      
      p_peak<- peak_target_plot %>% 
        ggplot(aes(Inflow_af/1000,peak_flow))+geom_line(linewidth=1.05)+
        geom_segment(data=tgts, aes(x = may1, xend=may1, y=0, yend =bc_peak_flow_target , color=factor(water_year)),linewidth=1.1)+
        geom_segment(data=tgts, aes(x = 0, xend=may1, y=bc_peak_flow_target, yend =bc_peak_flow_target, color=factor(water_year)), linetype='dashed',linewidth=1.1)+
        scale_color_manual(values=c('blue','red'))+
        theme_minimal(base_size=15)+
        labs(x='Blue Mesa Apr-Jul \nMay 1st Forecasted Inflow\n[kaf]', y= '24-Hour Peak\nFlow Target [cfs]',color='Water Year')+
       # geom_text(data=year_types, aes(x=0, y = min_calc_peak_flow, label = paste0(excceedance_prob_min_flow, '%')))+
        theme(legend.position = 'bottom')+
        scale_x_continuous(breaks=seq(0, 1500, 250))+
        scale_y_continuous(breaks=seq(0,17500, 2500))
      
      high_flow_target_plot<- tibble(
        Inflow_af = c(0, 561000, 690000, 1000000, 1500000), 
        high_flow = c(300, 300, 649.8, 1000,1000)
      )
      
      p_high_min<- high_flow_target_plot %>% 
        ggplot(aes(Inflow_af/1000,high_flow))+geom_line(linewidth=1.05)+
        geom_segment(data=tgts, aes(x = may1, xend=may1, y=0, yend =bc_high_flow_minimum , color=factor(water_year)),linewidth=1.1)+
        geom_segment(data=tgts, aes(x = 0, xend=may1, y=bc_high_flow_minimum, yend =bc_high_flow_minimum, color=factor(water_year)), linetype='dashed',linewidth = 1.1)+
        scale_color_manual(values=c('blue','red'))+
        theme_minimal(base_size=15)+
        labs(x='Blue Mesa Apr-Jul \nMay 1st Forecasted Inflow\n[kaf]', y= 'High Flow \nMinimum Target\n[cfs]',color='Water Year')+
        # geom_text(data=year_types, aes(x=0, y = min_calc_peak_flow, label = paste0(excceedance_prob_min_flow, '%')))+
        theme(legend.position = 'bottom')+
        scale_x_continuous(breaks=seq(0, 1500, 250))+
        scale_y_continuous(breaks=seq(0,1000, 250))
      
      p_peak+p_high_min+plot_layout(guides='collect')&theme(legend.position = 'bottom')
    })
  }) %>%
    bindEvent(input$evalButton)
  
  ### BC Results by Year Page ####
  
  observe({
    output$BC_results_by_year<- renderGirafe({
      
      minYear <- black_canyon_spring_peak %>% 
        pull(water_year) %>% 
        min()
      maxYear <- black_canyon_spring_peak %>% 
        pull(water_year) %>% 
        max()
      
      p1_May_1st<- black_canyon_spring_peak %>% 
        ggplot(aes(water_year,may1  ))+
        geom_line(linewidth=1.05)+
        geom_point_interactive(aes(
          tooltip= glue(
          "Water Year: {water_year}
          Apr-Jul Inflow Forecast: {may1}")),
          shape=21,size=3,fill='black')+
        #geom_point(shape=21,size=5,fill='black')+
        scale_x_continuous(breaks=c(1991:2023))+
        scale_y_continuous(expand=c(0.1,0.1))+
        theme_minimal(base_size=11)+
        theme(axis.text.x=element_blank(),panel.grid.major = element_line(color='grey75'),
              axis.title.y=element_text(size=8))+  
        scale_fill_manual(values=c('#377eb8','#ffff33'),breaks=c('TRUE','FALSE'),labels=c('Met','Not Met'))+
        labs(x='',y='Apr-Jul Blue Mesa\nInflow Forecast \n[kaf]',fill='')+
        coord_cartesian(xlim=c(minYear-0.5,maxYear+0.5)) 
      
      
      p1_peak<- black_canyon_spring_peak %>% 
        mutate(springPeakQ = round(springPeakQ, 0)) %>%
        ggplot(aes(water_year,springPeakQ))+
        geom_hpline(aes(water_year,bc_peak_flow_target),color='red')+
        geom_line(linewidth=1.05)+
        geom_point_interactive(aes(
          tooltip= glue(
            "Water Year: {water_year}
          Spring Peak Flow [cfs]: {springPeakQ}"),
         fill=peak_flag),
          shape=21,size=3)+
        scale_x_continuous(breaks=c(1991:2023))+
        scale_y_continuous(expand=c(0.1,0.1))+
        theme_minimal(base_size=11)+
        theme(axis.text.x=element_blank(),panel.grid.major = element_line(color='grey75'),
              axis.title.y=element_text(size=8))+          scale_fill_manual(values=c('#377eb8','#ffff33'),breaks=c('TRUE','FALSE'),labels=c('Met','Not Met'))+
        labs(x='',y='Peak Flow\n[CFS]',fill='')+
        coord_cartesian(xlim=c(minYear-0.5,maxYear+0.5)) 
      
      p1_peakmin<- high_flow_min_flow %>% 
        mutate(minQ=ifelse(minQ>1500,1500,minQ)) %>% 
        mutate(minQ = round(minQ, 0)) %>%
        ggplot(aes(water_year,minQ))+
        geom_hpline(aes(water_year,bc_high_flow_minimum),color='red')+
        geom_line(linewidth=1.05)+
        geom_point_interactive(aes(
          tooltip= glue(
            "Water Year: {water_year}
          High Flow Minimum [cfs]: {minQ}"),
          fill=high_min_flag),
          shape=21,size=3)+
        scale_x_continuous(breaks=c(1991:2023))+
        scale_y_continuous(expand=c(0.1,0.1))+
        theme_minimal(base_size=11)+
        scale_fill_manual(values=c('#377eb8','#ffff33'),breaks=c('TRUE','FALSE'),labels=c('Met','Not Met'))+
        theme(axis.text.x=element_blank(),panel.grid.major = element_line(color='grey75'),
              axis.title.y=element_text(size=8))+          labs(x='',y='High Flow\nMinimum [CFS]',fill='')+
        coord_cartesian(xlim=c(minYear-0.5,maxYear+0.5)) 
      
      p1_baseflow_min <- annual_min_flow %>% 
        mutate(minQ = round(minQ, 0)) %>%
        ggplot(aes(water_year,minQ))+
        geom_hpline(aes(water_year,target_min_flow),color='red')+
        geom_line(linewidth=1.05)+
        geom_point_interactive(aes(
          tooltip= glue(
            "Water Year: {water_year}
          Baseflow Minimum [cfs]: {minQ}"),
          fill=achievement_flag),
          shape=21,size=3)+
        scale_x_continuous(breaks=c(1991:2023))+
        scale_y_continuous(expand=c(0.1,0.1))+
        theme_minimal(base_size=11)+
        scale_fill_manual(values=c('#377eb8','#ffff33'),breaks=c('TRUE','FALSE'),labels=c('Met','Not Met'))+
        theme(axis.text.x=element_blank(),panel.grid.major = element_line(color='grey75'),
              axis.title.y=element_text(size=8))+          labs(x='',y='Baseflow\nMinimum [CFS]',fill='')+
        coord_cartesian(xlim=c(minYear-0.5,maxYear+0.5)) 
      
      # p1_ramping<- black_canyon_ramping_raw %>% 
      #   mutate(days_blw_ramping_rate = round(days_blw_ramping_rate, 0)) %>%
      #   ggplot(aes(water_year,days_blw_ramping_rate ))+
      # #  geom_hpline(aes(water_year,target_min_flow),color='red')+
      #   geom_line(linewidth=1.05)+
      #   geom_point_interactive(aes(
      #     tooltip= glue(
      #       "Water Year: {water_year}
      #     Days Blw Ramping Rate [days]: {days_blw_ramping_rate}"),
      #     fill=result),
      #     shape=21,size=3)+
      #   scale_x_continuous(breaks=c(1991:2023))+
      #   scale_y_continuous(breaks = integer_breaks())+
      #   theme_minimal(base_size=11)+
      #   scale_fill_manual(values=c('#377eb8','#ffff33'),breaks=c('TRUE','FALSE'),labels=c('Met','Not Met'))+
      #   theme(axis.text.x=element_blank(),panel.grid.major = element_line(color='grey75'),
      #         axis.title.y=element_text(size=8))+  
      #   labs(x='',y='Days Meeting\nRamping Rate',fill='')+
      #   coord_cartesian(xlim=c(minYear-0.5,maxYear+0.5)) 
      
      p1_BD <- black_canyon_rec_raw %>% 
        mutate(Name= factor(Name,labels=c('Lower Acceptable','Optimal','Upper Acceptable','Total'))) %>% 
        mutate(Value = round(Value, 0)) %>%
        filter(Name!='Total') %>% 
        ggplot(aes(water_year, Value,fill=fct_rev(Name)))+
        geom_col_interactive(
          aes(
            tooltip= glue(
              "Water Year: {water_year}
               User Preference: {Name}
               Boatable Days: {Value}"))
       )+
        scale_x_continuous(breaks=c(1991:2023))+
        scale_y_continuous(expand=c(0.1,0.1))+
        theme_minimal(base_size=11)+
        theme(axis.text.x=element_text(angle=90),panel.grid.major = element_line(color='grey75'),
              axis.title.y=element_text(size=8))+  
        scale_fill_ptol()+
        labs(x='',y='Whitewater\n In-Season\nBoatable Days',fill='User Preference')+
        coord_cartesian(xlim=c(minYear-0.5,maxYear+0.5)) 
      
      p_bda_season<- tunnel_BDA_class %>% 
        filter(black_canyon_WW  !='Not Acceptable') %>% 
        mutate(
          Season= ifelse(month %in% c(5:10), 'In Season','Out of Season'),
          Season = factor(Season,levels=c('Out of Season','In Season'))
        ) %>% 
        group_by(water_year,Season) %>% 
        tally() %>% 
        ggplot(aes(water_year, n))+
        geom_col_interactive(
          aes(fill=Season,
            tooltip= glue(
              "Water Year: {water_year}
               Season: {Season}
               Boatable Days: {n}"))
        )+
        scale_x_continuous(breaks=c(1991:2023))+
        scale_y_continuous(expand=c(0.1,0.1))+
        theme_minimal(base_size=11)+
        theme(axis.text.x=element_text(angle=90),panel.grid.major = element_line(color='grey75'),
              axis.title.y=element_text(size=8))+  
        scale_fill_manual(values=c('#999999','#67a9cf'),breaks=c('Out of Season','In Season'))+
        labs(x='Water Year (Apr - Mar)',y='Whitewater\n Annual Boatable Days',fill='Season')+
        coord_cartesian(xlim=c(minYear-0.5,maxYear+0.5)) 

      girafe(
       # ggobj =(p1_May_1st / p1_peak / p1_peakmin /p1_baseflow_min/ p1_ramping / p1_BD / p_bda_season),
        ggobj =(p1_May_1st / p1_peak / p1_peakmin /p1_baseflow_min/  p1_BD / p_bda_season),
        
        width_svg = 8.16669,
        height_svg=8.16669,
        options = list(
          opts_tooltip(
            opacity = 1, use_fill = FALSE,
            use_stroke = TRUE, 
            css = "padding:5pt;font-family: Open Sans;color:black"),
          opts_hover_inv(css = "opacity:0.5"), 
          opts_hover(
            css = girafe_css(
              css = "fill:#4c6061;",
              text = "stroke:none;fill:white;fill-opacity:1;font-size:larger"
            )
          )
        )
      )
     # # p1<-p1_May_1st / p1_peak / p1_peakmin /p1_baseflow_min/p1_totBD/p1_BD
     #  p1<-p1_May_1st / p1_peak / p1_peakmin /p1_baseflow_min/p1_BD
     #  p1
    
    })
  }) %>%
    bindEvent(input$evalButton)
  
  #### BC Environmental Summary Page ####
  
  observe({
    output$BC_env_results<- renderPlot({
      
      title_name <- paste0('Analysis Period:  ',start_year(),' - ',end_year())
      
      p_peak<- black_canyon_spring_peak %>% 
        ggplot(aes(x=springPeakQ))+geom_histogram(aes(y = after_stat(count / sum(count))), binwidth = 1000,
                                                  fill='#386cb0',
                                                  color='black')+
        scale_y_continuous(labels = scales::percent,breaks=pretty_breaks(n=5))+
        scale_x_continuous(breaks=seq(500,14000,1000),expand = c(0,0))+
       # coord_cartesian(xlim=c(0,14000))+
        labs(x='Spring Peak Flow [cfs]',y='Percent of Years in Record')+
        theme_minimal(base_size=12)
      
      p_high_flow<- high_flow_min_flow %>% 
        ggplot(aes(x=minQ  ))+geom_histogram(aes(y = after_stat(count / sum(count))), binwidth = 100,
                                             fill='#8dd3c7',
                                             color='black')+
        scale_y_continuous(labels = scales::percent,breaks=pretty_breaks(n=5))+
        scale_x_continuous(breaks=seq(0,3600,100))+
        # coord_cartesian(xlim=c(0,14000))+
        labs(x='High Flow Minimum Flow [cfs]',y='Percent of Years in Record')+
        theme_minimal(base_size=12)+
        theme(axis.text.x = element_text(angle = 90))
      
      p_baseflow <- annual_min_flow %>% 
        ggplot(aes(x=minQ  ))+geom_histogram(aes(y = after_stat(count / sum(count))), binwidth = 25,
                                             fill='#fdb462',
                                             color='black')+
        scale_y_continuous(labels = scales::percent,breaks=pretty_breaks(n=5))+
        scale_x_continuous(breaks=seq(0,700,50),expand=c(0,0))+
        # coord_cartesian(xlim=c(0,14000))+
        labs(x='Baseflow Minimum [cfs]',y='Percent of Years in Record')+
        theme_minimal(base_size=12)
      
      p_ramping <- 
        black_canyon_ramping_raw %>% 
        ggplot(aes(x=pct_year_abv_ramping_rate))+
        geom_histogram(aes(y = after_stat(count / sum(count))), binwidth = 5,
                                                             fill='#bebada',
                                                             color='black')+
        scale_y_continuous(labels = scales::percent,breaks=pretty_breaks(n=5))+
        scale_x_continuous(breaks=seq(0,100,5),expand=c(0,0))+
        # coord_cartesian(xlim=c(0,14000))+
        labs(x='Percent Days In Year\nAchieving Ramping Rate',y='Percent of Years in Record')+
        theme_minimal(base_size=12)+
        theme(axis.text.x = element_text(angle = 90))
      p<- p_peak+p_high_flow+p_baseflow+p_ramping+
        plot_annotation(title = title_name, theme = theme(plot.title = element_text(size = 18,hjust=0.5,
                                                                                    margin = margin(0,0,10,0))))
      p
    })
  }) %>%
    bindEvent(input$evalButton)
  
  #### BC Recreational Summary Page ####
  
  observe({
    output$BC_rec_by_User<- renderPlot({
      
      title_name <<- paste0('Analysis Period:  ',start_year(),' - ',end_year())
      
      black_canyon_rec_raw %>% 
        filter(Name !='Total') %>% 
        ggplot(aes(Value))+
        geom_histogram(aes(y = after_stat(count / sum(count)),fill=Name), binwidth = 10,
                       color='black')+
        scale_y_continuous(labels = scales::percent,breaks=pretty_breaks(n=5))+
        scale_x_continuous(breaks=seq(0,365,10),expand=c(0,0))+
        scale_fill_few()+
        # coord_cartesian(xlim=c(0,14000))+
        labs(x='Annual Boatable Days',y='Percent of Years in Record',fill='User Preference',title = title_name)+
        theme_minimal(base_size=15)+
        theme(axis.text.x = element_text(angle = 90))+
        facet_wrap(~Name,ncol=1)+
        theme(legend.position = 'none',plot.title = element_text(size = 18,hjust=0.5))
    })
    
    bc_rec_monthly_bda<- tunnel_BDA_class %>% 
      filter(black_canyon_WW !=  'Not Acceptable') %>% 
      mutate(month = factor(month(date, label=TRUE))) %>% 
      group_by(water_year, month,.drop=FALSE) %>% 
      tally()
    
    output$BC_rec_monthly<- renderPlot({
      
      p1<- bc_rec_monthly_bda %>% 
        filter(month %in% c('May','Jun','Jul','Aug','Sep','Oct' )) %>% 
        ggplot(aes(n))+
        geom_histogram(aes(y = after_stat(count / sum(count)),fill=month), binwidth = 5,
                       color='black')+
        scale_y_continuous(labels = scales::percent,breaks=pretty_breaks(n=3))+
        scale_x_continuous(breaks=seq(0,31,5),expand=c(0,0))+
        scale_fill_ptol()+
        # coord_cartesian(xlim=c(0,14000))+
        labs(x='Monthly Boatable Days',y='Percent of Years in Record',fill='User Preference')+
        theme_few(base_size=15)+
        theme(axis.text.x = element_text(angle = 90))+
        facet_wrap(~month,ncol=1)+
        theme(legend.position = 'none')
      
      
      monthly_bda<- bc_rec_monthly_bda %>% 
        mutate(Season= ifelse(month %in% c('May','Jun','Jul','Aug','Sep','Oct'), 'In Season','Out of Season')) %>% 
        ggplot(aes(month, n))+geom_boxplot(aes(fill=Season),outlier.shape = NA)+
        geom_jitter()+
        scale_fill_manual(values=c('#67a9cf','#999999'))+
        scale_y_continuous(breaks=seq(0,31,5),expand=c(0.1,0.1))+
        labs(y='Monthly\nBoatable Days',x='Month',fill='')+
        theme_few(base_size=15)+
        theme(legend.position = 'right')
      
      median_bda<- bc_rec_monthly_bda %>% 
        group_by(month) %>% 
        summarize(median = median(n)) %>% 
        mutate(Season= ifelse(month %in% c('May','Jun','Jul','Aug','Sep','Oct'), 'In Season','Out of Season')) %>% 
        ggplot(aes(month, median))+geom_col(aes(fill=Season),color='black')+
        scale_fill_manual(values=c('#67a9cf','#999999'))+
        scale_y_continuous(breaks=seq(0,31,5),expand=c(0.1,0.1))+
        labs(y='Median Monthly\nBoatable Days',x='Month',fill='')+
        theme_few(base_size=15)+
        theme(legend.position = 'right')
        
      monthly_bda / median_bda / p1 + plot_layout(heights = c(.25,.25,1))
    })
  }) %>%
    bindEvent(input$evalButton)
  
  ### GG Results by Year Page ####
  
  
  observe({
    output$GG_results_by_year<- renderGirafe({
      
      minYear <- black_canyon_spring_peak %>% 
        pull(water_year) %>% 
        min()
      maxYear <- black_canyon_spring_peak %>% 
        pull(water_year) %>% 
        max()
      
      p1_May_1st<- black_canyon_spring_peak %>% 
        ggplot(aes(water_year,may1  ))+
        geom_line(linewidth=1.05)+
        geom_point_interactive(aes(
          tooltip= glue(
            "Water Year: {water_year}
          Apr-Jul Inflow Forecast: {may1}")),
          shape=21,size=3,fill='black')+
        scale_x_continuous(breaks=c(1991:2023))+
        scale_y_continuous(expand=c(0.1,0.1))+
        theme_minimal(base_size=11)+
        theme(axis.text.x=element_blank(),panel.grid.major = element_line(color='grey75'),
              axis.title.y=element_text(size=7))+     
        scale_fill_manual(values=c('#377eb8','#ffff33'),breaks=c('TRUE','FALSE'),labels=c('Met','Not Met'))+
        labs(x='',y='Apr-Jul Blue Mesa\nInflow Forecast \n[kaf]',fill='')+
        coord_cartesian(xlim=c(minYear-0.5,maxYear+0.5)) 
      
      
      p1_rainbow<- ggnca_max_flow_exceedance_duration_rainbow_raw %>% 
        mutate(days_blw_maximum = round(n_days-duration_days,0)) %>% 
        ggplot(aes(water_year,days_blw_maximum))+
        geom_line(linewidth=1.05)+
        geom_point_interactive(aes(
          tooltip= glue(
            "Water Year: {water_year}
          Days blw Rainbow Recruitment Max: {days_blw_maximum}")),
          shape=21,size=3,fill='black')+
        scale_x_continuous(breaks=c(1991:2023))+
        scale_y_continuous(expand=c(0.1,0.1))+
        theme_minimal(base_size=11)+
        theme(axis.text.x=element_blank(),panel.grid.major = element_line(color='grey75'),
              axis.title.y=element_text(size=7))+     
       # scale_fill_manual(values=c('#377eb8','#ffff33'),breaks=c('Met','Not Met'),labels=c('Met','Not Met'))+
        labs(x='',y='Days blw Rainbow Trout \nRecruitment Max\n(6/1 - 6/30)',fill='')+
        coord_cartesian(xlim=c(minYear-0.5,maxYear+0.5)) 
      
      p1_brown<- ggnca_max_flow_exceedance_duration_brown_raw %>% 
        mutate(days_blw_maximum = round(n_days-duration_days,0)) %>% 
        ggplot(aes(water_year,days_blw_maximum))+
        geom_line(linewidth=1.05)+
        geom_point_interactive(aes(
          tooltip= glue(
            "Water Year: {water_year}
          Days blw Brown Recruitment Max: {days_blw_maximum}")),
          shape=21,size=3,fill='black')+
        scale_x_continuous(breaks=c(1991:2023))+
        scale_y_continuous(expand=c(0.1,0.1))+
        theme_minimal(base_size=11)+
        theme(axis.text.x=element_blank(),panel.grid.major = element_line(color='grey75'),
              axis.title.y=element_text(size=7))+     
       # scale_fill_manual(values=c('#377eb8','#ffff33'),breaks=c('Met','Not Met'),labels=c('Met','Not Met'))+
        labs(x='',y='Days blw Brown Trout\nRecruitment Max\n(4/15 - 6/1)',fill='')+
        coord_cartesian(xlim=c(minYear-0.5,maxYear+0.5)) 
      
      p_optimal<- ggnca_fish_range_raw %>% 
        mutate(duration_days = round(duration_days, 0)) %>% 
        ggplot(aes(water_year,duration_days))+
        geom_line(linewidth=1.05)+
        geom_point_interactive(aes(
          tooltip= glue(
            "Water Year: {water_year}
          Days within Optimal Trout Flow Range: {duration_days}")),
          shape=21,size=3,fill='black')+
        scale_x_continuous(breaks=c(1991:2023))+
        scale_y_continuous(expand=c(0.1,0.1))+
        theme_minimal(base_size=11)+
        theme(axis.text.x=element_blank(),panel.grid.major = element_line(color='grey75'),
              axis.title.y=element_text(size=7))+     
        # scale_fill_manual(values=c('#377eb8','#ffff33'),breaks=c('Met','Not Met'),labels=c('Met','Not Met'))+
        labs(x='',y='Days within\nOptimal Trout \nFlow Range ',fill='')+
        coord_cartesian(xlim=c(minYear-0.5,maxYear+0.5)) 
    
      # p1_totBD_WW <- ggnca_rec_raw %>% 
      #   filter(Type=='Whitewater Recreation', 
      #          Name=='WW Total') %>% 
      #   mutate(legend_name = 'Average Total\nBoatable Days') %>% 
      #   ggplot(aes(water_year, Result))+geom_line()+
      #   geom_point(shape=21,size=5,fill='black')+
      #   scale_x_continuous(breaks=c(1991:2023))+
      #   scale_y_continuous(expand=c(0.1,0.1))+
      #   theme_minimal(base_size=11)+
      #   theme(axis.text.x=element_blank(),panel.grid.major = element_line(color='grey75'))+
      #   scale_fill_ptol()+
      #   geom_hline(aes(yintercept = `Long Term Average`,color=legend_name),linetype='dotted')+
      #   scale_color_manual(values='black')+
      #   labs(x='',y='Whitwater: Total\nBoatable Days',color='')+
      #   coord_cartesian(xlim=c(minYear-0.5,maxYear+0.5)) 
      # 
      p1_BD_WW <- ggnca_rec_raw %>% 
        filter(Name%in% c('WW Lower Acceptable','WW Optimal','WW Upper Acceptable','WW Total')) %>% 
        mutate(Name= factor(Name,labels=c('WW Lower Acceptable','WW Optimal','WW Upper Acceptable','WW Total'))) %>% 
        mutate(Value = round(Value, 0)) %>% 
        filter(Name!='WW Total') %>% 
        ggplot(aes(water_year, Value,fill=fct_rev(Name)))+
        geom_col_interactive(aes(
            tooltip= glue(
              "Water Year: {water_year}
              User Preference: {Name}
              Whitewater Boatable Days: {Value}"))
        )+
        scale_x_continuous(breaks=c(1991:2023))+
        scale_y_continuous(expand=c(0.1,0.1))+
        theme_minimal(base_size=11)+
        theme(axis.text.x=element_blank(),panel.grid.major = element_line(color='grey75'),
              axis.title.y=element_text(size=7))+        
        scale_fill_ptol()+
        labs(x='',y='Whitewater:\nIn-Season\nBoatable Days',fill='User Preference')+
        coord_cartesian(xlim=c(minYear-0.5,maxYear+0.5)) 
      
      p_bda_season_ww<- tunnel_BDA_class %>% 
        filter(gunnison_gorge_WW  !='Not Acceptable') %>% 
        mutate(
          Season= ifelse(month %in% c(5:10), 'In Season','Out of Season'),
          Season = factor(Season,levels=c('Out of Season','In Season'))
        ) %>% 
        group_by(water_year,Season) %>% 
        tally() %>% 
        ggplot(aes(water_year, n))+
        geom_col_interactive(
          aes(fill=Season,
              tooltip= glue(
                "Water Year: {water_year}
               Season: {Season}
               Boatable Days: {n}"))
        )+
        scale_x_continuous(breaks=c(1991:2023))+
        scale_y_continuous(expand=c(0.1,0.1))+
        theme_minimal(base_size=11)+
        theme(axis.text.x=element_blank(),panel.grid.major = element_line(color='grey75'),
              axis.title.y=element_text(size=7))+  
        scale_fill_manual(values=c('#999999','#67a9cf'),breaks=c('Out of Season','In Season'))+
        labs(x='',y='Whitewater\n Annual\nBoatable Days',fill='Season')+
        coord_cartesian(xlim=c(minYear-0.5,maxYear+0.5)) 
      
      # 
      # p1_totBD_Ang <- ggnca_rec_raw %>% 
      #   filter(Type=='Angling', 
      #          Name=='Ang Total') %>% 
      #   mutate(legend_name = 'Average Total\nBoatable Days') %>% 
      #   ggplot(aes(water_year, Result))+geom_line()+
      #   geom_point(shape=21,size=5,fill='black')+
      #   scale_x_continuous(breaks=c(1991:2023))+
      #   scale_y_continuous(expand=c(0.1,0.1))+
      #   theme_minimal(base_size=11)+
      #   theme(axis.text.x=element_blank(),panel.grid.major = element_line(color='grey75'))+
      #   scale_fill_ptol()+
      #   geom_hline(aes(yintercept = `Long Term Average`,color=legend_name),linetype='dotted')+
      #   scale_color_manual(values='black')+
      #   labs(x='',y='Angling: Total\nBoatable Days',color='')+
      #   coord_cartesian(xlim=c(minYear-0.5,maxYear+0.5)) 
      
      p1_BD_Ang <- ggnca_rec_raw %>% 
        filter(Name%in% c('Ang Lower Acceptable','Ang Optimal','Ang Upper Acceptable')) %>% 
        mutate(Name= factor(Name,labels=c('Ang Lower Acceptable','Ang Optimal','Ang Upper Acceptable'))) %>% 
        mutate(Value = round(Value, 0)) %>% 
        filter(Name!='Ang Total') %>% 
        ggplot(aes(water_year, Value,fill=fct_rev(Name)))+
        geom_col_interactive(aes(
          tooltip= glue(
            "Water Year: {water_year}
              User Preference: {Name}
              Angling Boatable Days: {Value}"))
        )+
        scale_x_continuous(breaks=c(1991:2023))+
        scale_y_continuous(expand=c(0.1,0.1))+
        theme_minimal(base_size=11)+
        theme(axis.text.x=element_blank(),panel.grid.major = element_line(color='grey75'),
              axis.title.y=element_text(size=7))+        
        scale_fill_ptol()+
        labs(x='',y='Angling:\nIn-Season\nBoatable Days',fill='User Preference')+
        coord_cartesian(xlim=c(minYear-0.5,maxYear+0.5)) 
      
      p_bda_season_ang<- tunnel_BDA_class %>% 
        filter(gunnison_gorge_fishing  !='Not Acceptable') %>% 
        mutate(
          Season= ifelse(month %in% c(5:10), 'In Season','Out of Season'),
          Season = factor(Season,levels=c('Out of Season','In Season'))
        ) %>% 
        group_by(water_year,Season) %>% 
        tally() %>% 
        ggplot(aes(water_year, n))+
        geom_col_interactive(
          aes(fill=Season,
              tooltip= glue(
                "Water Year: {water_year}
               Season: {Season}
               Angling Boatable Days: {n}"))
        )+
        scale_x_continuous(breaks=c(1991:2023))+
        scale_y_continuous(expand=c(0.1,0.1))+
        theme_minimal(base_size=11)+
        theme(axis.text.x=element_text(angle=90),panel.grid.major = element_line(color='grey75'),
              axis.title.y=element_text(size=7))+  
        scale_fill_manual(values=c('#999999','#67a9cf'),breaks=c('Out of Season','In Season'))+
        labs(x='Water Year (Apr - Mar)',y='Angling\nAnnual\nBoatable Days',fill='Season')+
        coord_cartesian(xlim=c(minYear-0.5,maxYear+0.5)) 
      
      # p1<-p1_May_1st / p1_rainbow / p1_brown / p_optimal /p1_BD_WW /p1_BD_Ang
      # p1
      girafe(
        ggobj =(p1_May_1st / p1_rainbow / p1_brown / p_optimal /p1_BD_WW /p1_BD_Ang / p_bda_season_ww / p_bda_season_ang),
        width_svg = 8.16666,
        height_svg=9,
        options = list(
          opts_tooltip(
            opacity = 1, use_fill = FALSE,
            use_stroke = TRUE, 
            css = "padding:5pt;font-family: Open Sans;color:black"),
          opts_hover_inv(css = "opacity:0.5"), 
          opts_hover(
            css = girafe_css(
              css = "fill:#4c6061;",
              text = "stroke:none;fill:white;fill-opacity:1;font-size:larger"
            )
          )
        )
      )
      
    })
  }) %>%
    bindEvent(input$evalButton)
  
  observe({
    output$GG_env_results<- renderPlot({
      #### GG Environmental Summary Page####
      
      title_name <- paste0('Analysis Period:  ',start_year(),' - ',end_year())
      
    
    p_rainbow<- ggnca_max_flow_exceedance_duration_rainbow_raw %>% 
      mutate(days_blw_maximum = n_days-duration_days ) %>% 
      ggplot(aes(x=days_blw_maximum))+geom_histogram(aes(y = after_stat(count / sum(count))), binwidth = 5,
                                                fill='#386cb0',
                                                color='black')+
      scale_y_continuous(labels = scales::percent,breaks=pretty_breaks(n=5))+
      scale_x_continuous(breaks=seq(0,30,5),expand = c(0,0))+
      # coord_cartesian(xlim=c(0,14000))+
      labs(x='Days Below Rainbow Trout\nRecuitment Maximum\n(6/1-6/30)',y='Percent of Years in Record')+
      theme_minimal(base_size=12)
    
    p_brown<- ggnca_max_flow_exceedance_duration_brown_raw %>% 
      mutate(days_blw_maximum = n_days-duration_days ) %>% 
      ggplot(aes(x=days_blw_maximum))+geom_histogram(aes(y = after_stat(count / sum(count))), binwidth = 5,
                                                     fill='#8dd3c7',
                                                     color='black')+
      scale_y_continuous(labels = scales::percent,breaks=pretty_breaks(n=5))+
      scale_x_continuous(breaks=seq(0,50,5),expand = c(0,0))+
      # coord_cartesian(xlim=c(0,14000))+
      labs(x='Days Below Brown Trout\nRecuitment Maximum\n(4/15-6/1)',y='Percent of Years in Record')+
      theme_minimal(base_size=12)
    
    p_trout_optimal<- ggnca_fish_range_raw %>% 
      #mutate(days_blw_maximum = n_days-duration_days ) %>% 
      ggplot(aes(x=duration_days ))+geom_histogram(aes(y = after_stat(count / sum(count))), binwidth = 40,
                                                     fill='#fdb462',
                                                     color='black')+
      scale_y_continuous(labels = scales::percent,breaks=pretty_breaks(n=5))+
      scale_x_continuous(breaks=seq(0,360,40),expand = c(0,0))+
      # coord_cartesian(xlim=c(0,14000))+
      labs(x='Days Within Optimal Trout Range',y='Percent of Years in Record')+
      theme_minimal(base_size=12)
    
    p_baseflow <- annual_min_flow %>% 
      ggplot(aes(x=minQ  ))+geom_histogram(aes(y = after_stat(count / sum(count))), binwidth = 25,
                                           fill='#fdb462',
                                           color='black')+
      scale_y_continuous(labels = scales::percent,breaks=pretty_breaks(n=5))+
      scale_x_continuous(breaks=seq(0,700,50),expand=c(0,0))+
      # coord_cartesian(xlim=c(0,14000))+
      labs(x='Baseflow Minimum [cfs]',y='Percent of Years in Record')+
      theme_minimal(base_size=12)
    
    
    p<- p_rainbow+p_brown+p_trout_optimal+ p_baseflow+
      plot_annotation(title = title_name, theme = theme(plot.title = element_text(size = 18,hjust=0.5,
                                                                                  margin = margin(0,0,10,0))))
    p
    p
    })
  }) %>%
  bindEvent(input$evalButton)
  
  #### GG Recreational Summary Page  ####
  observe({
    output$GG_rec_by_User<- renderPlot({
      
      title_name <- paste0('Analysis Period:  ',start_year(),' - ',end_year())
      
      p<- ggnca_rec_raw %>% 
        filter(Name != 'Camping/Trails Max Flow') %>% 
        mutate(Name = str_trim(str_sub(Name, 4)), Type = str_trim(Type)) %>% 
        filter(Name %in% c('Lower Acceptable','Optimal','Upper Acceptable')) %>% 
        mutate(Name= factor(Name,labels=c('Lower Acceptable','Optimal','Upper Acceptable'))) %>% 
        ggplot(aes(Value))+
        geom_histogram(aes(y = after_stat(count / sum(count)),fill=Name), binwidth = 10,
                       color='black')+
        scale_y_continuous(labels = scales::percent,breaks=pretty_breaks(n=5))+
        scale_x_continuous(breaks=seq(0,365,10),expand=c(0,0))+
        scale_fill_few()+
        # coord_cartesian(xlim=c(0,14000))+
        labs(x='Annual Boatable Days',y='Percent of Years in Record',fill='User Preference',title=title_name)+
        theme_minimal(base_size=12)+
        theme(axis.text.x = element_text(angle = 90))+
        facet_grid(rows=vars(Name), cols=vars(Type))+
        theme(legend.position = 'none',plot.title = element_text(size = 18,hjust=0.5))
      p
    })
    
    gg_rec_monthly_bda<- tunnel_BDA_class %>% 
      dplyr::select(date, water_year, month, `Whitewater Recreation`=gunnison_gorge_WW , Angling=gunnison_gorge_fishing) %>% 
      pivot_longer(`Whitewater Recreation`:Angling) %>% 
      filter(value!=  'Not Acceptable') %>% 
      mutate(month = factor(month(date, label=TRUE)),name=factor(name),water_year = factor(water_year)) %>% 
      group_by(water_year, month,name, .drop=FALSE ) %>% 
      tally()
    
    output$GG_rec_monthly<- renderPlot({
      
      p1<- gg_rec_monthly_bda %>% 
        filter(month %in% c('May','Jun','Jul','Aug','Sep','Oct')) %>% 
        ggplot(aes(n))+
        geom_histogram(aes(y = after_stat(count / sum(count)),fill=month), binwidth = 5,
                       color='black')+
        scale_y_continuous(labels = scales::percent,breaks=pretty_breaks(n=3))+
        scale_x_continuous(breaks=seq(0,31,5),expand=c(0,0))+
        scale_fill_ptol()+
        # coord_cartesian(xlim=c(0,14000))+
        labs(x='Monthly Boatable Days',y='Percent of Years in Record',fill='User Preference')+
        theme_few(base_size=14)+
        theme(axis.text.x = element_text(angle = 90))+
        facet_grid(rows=vars(month), cols=vars(name))+
        theme(legend.position = 'none')
      
      
      monthly_bda<- gg_rec_monthly_bda %>% 
        mutate(Season= ifelse(month %in% c('May','Jun','Jul','Aug','Sep','Oct'), 'In Season','Out of Season')) %>% 
        ggplot(aes(month, n))+geom_boxplot(aes(fill=Season),outlier.shape = NA)+
        geom_jitter()+
        scale_fill_manual(values=c('#67a9cf','#999999'))+
        scale_y_continuous(breaks=seq(0,31,5),expand=c(0.1,0.1))+
        labs(y='Monthly Boatable Days',x='Month',fill='')+
        theme_few(base_size=14)+
        theme(legend.position = 'top')+
        facet_grid(cols=vars(name))
      
      
      median_bda<- gg_rec_monthly_bda %>% 
        group_by(month,name) %>% 
        summarize(median = median(n)) %>% 
        mutate(Season= ifelse(month %in% c('May','Jun','Jul','Aug','Sep','Oct'), 'In Season','Out of Season')) %>% 
        ggplot(aes(month, median))+geom_col(aes(fill=Season),color='black')+
        scale_fill_manual(values=c('#67a9cf','#999999'))+
        scale_y_continuous(breaks=seq(0,31,5),expand=c(0.1,0.1))+
        labs(y='Median Boatable Days',x='Month',fill='')+
        theme_few(base_size=14)+
        theme(legend.position = 'top')+
        facet_grid(cols=vars(name))
      
      monthly_bda / p1 + plot_layout(heights = c(.25,1.25))
      
      
    })
  }) %>%
    bindEvent(input$evalButton)
  
  #### LG Results by Year Page ####
  
  observe({
    output$LG_results_by_year<- renderGirafe({
      
      minYear <- black_canyon_spring_peak %>% 
        pull(water_year) %>% 
        min()
      maxYear <- black_canyon_spring_peak %>% 
        pull(water_year) %>% 
        max()
      
      p1_May_1st<- black_canyon_spring_peak %>% 
        ggplot(aes(water_year,may1)) +
        geom_line(linewidth=1.05)+
        geom_point_interactive(aes(
          tooltip= glue(
            "Water Year: {water_year}
          Apr-Jul Inflow Forecast: {may1}")),
          shape=21,size=3,fill='black')+
        scale_x_continuous(breaks=c(1991:2023))+
        scale_y_continuous(expand=c(0.15,0.15))+
        theme_minimal(base_size=11)+
        theme(axis.text.x=element_blank(),panel.grid.major = element_line(color='grey75'),
              axis.title.y=element_text(size=8))+  
        scale_fill_manual(values=c('#377eb8','#ffff33'),breaks=c('TRUE','FALSE'),labels=c('Met','Not Met'))+
        labs(x='',y='Apr-Jul Blue Mesa\nInflow Forecast \n[kaf]',fill='')+
        coord_cartesian(xlim=c(minYear-0.5,maxYear+0.5)) 
      
      
      p1_peak<- whitewater_spring_peak_raw_yrtype %>% 
        mutate(value = round(value, 0)) %>% 
        ggplot(aes(water_year,value))+
        geom_hpline(aes(water_year,min_target),color='red')+
        geom_line(linewidth=1.05)+
        geom_point_interactive(aes(
          tooltip= glue(
            "Water Year: {water_year}
          Spring Peak Flow [cfs]: {value}"),
          fill=achievement_flag),
          shape=21,size=3)+
        scale_x_continuous(breaks=c(1991:2023))+
        scale_y_continuous(expand=c(0.1,0.1))+
        theme_minimal(base_size=11)+
        theme(axis.text.x=element_blank(),panel.grid.major = element_line(color='grey75'),
              axis.title.y=element_text(size=8))+  
        scale_fill_manual(values=c('#377eb8','#ffff33'),breaks=c('TRUE','FALSE'),labels=c('Met','Not Met'))+
        labs(x='',y='Peak Flow\n[CFS]',fill='')+
        coord_cartesian(xlim=c(minYear-0.5,maxYear+0.5)) 
      
      p1_maintenance<- whitewater_high_flow_duration_yrType %>% 
        mutate(`Maintenance (>8070 cfs)` = round(`Maintenance (>8070 cfs)`, 0)) %>% 
        ggplot(aes(water_year,`Maintenance (>8070 cfs)`))+
        geom_hpline(aes(water_year,maintenance_min_target),color='red')+
        geom_line(linewidth=1.05)+
        geom_point_interactive(aes(
          tooltip= glue(
            "Water Year: {water_year}
          Maintenance Flow Duration [days]: {`Maintenance (>8070 cfs)`}"),
          fill=maintenance_achievement_flag),
          shape=21,size=3)+
        scale_x_continuous(breaks=c(1991:2023))+
        scale_y_continuous(expand=c(0.15,0.15))+
        theme_minimal(base_size=11)+
        scale_fill_manual(values=c('#377eb8','#ffff33'),breaks=c('TRUE','FALSE'),labels=c('Met','Not Met'))+
        theme(axis.text.x=element_blank(),panel.grid.major = element_line(color='grey75'),
              axis.title.y=element_text(size=8))+  
        labs(x='',y='Maintenance Flows\n(>8070cfs)\n(Days)',fill='')+
        coord_cartesian(xlim=c(minYear-0.5,maxYear+0.5)) 
      
      p1_improvement<- whitewater_high_flow_duration_yrType %>% 
        mutate(`Improvement (>14350)` = round(`Improvement (>14350)`, 0)) %>% 
        ggplot(aes(water_year,`Improvement (>14350)`))+
        geom_hpline(aes(water_year,improvement_min_target),color='red')+
        geom_line(linewidth=1.05)+
        geom_point_interactive(aes(
          tooltip= glue(
            "Water Year: {water_year}
          Improvement Flow Duration [days]: {`Improvement (>14350)`}"),
          fill=improvement_achievement_flag),
          shape=21,size=3)+
        scale_x_continuous(breaks=c(1991:2023))+
        scale_y_continuous(expand=c(0.15,0.15))+
        theme_minimal(base_size=11)+
        scale_fill_manual(values=c('#377eb8','#ffff33'),breaks=c('TRUE','FALSE'),labels=c('Met','Not Met'))+
        theme(axis.text.x=element_blank(),panel.grid.major = element_line(color='grey75'),
              axis.title.y=element_text(size=8))+  
        labs(x='',y='Improvement Flows\n(>14350cfs)\n(Days)',fill='')+
        coord_cartesian(xlim=c(minYear-0.5,maxYear+0.5)) 
      
      p1_baseflow_min_days <- whitewater_base_eval_raw_yrType %>% 
        mutate(blw_base_flow = round(blw_base_flow, 0)) %>% 
        ggplot(aes(water_year,blw_base_flow ))+
      # geom_hpline(aes(water_year,target_min_flow),color='red')+
         geom_line(linewidth=1.05)+
        geom_point_interactive(aes(
          tooltip= glue(
            "Water Year: {water_year}
          Days Below Baseflow Minimum [days]: {blw_base_flow}"),
          fill=flag_achievement),
          shape=21,size=3)+
        scale_x_continuous(breaks=c(1991:2023))+
        scale_y_continuous(expand=c(0.15,0.15))+
        theme_minimal(base_size=11)+
        scale_fill_manual(values=c('#377eb8','#ffff33'),breaks=c('TRUE','FALSE'),labels=c('Met','Not Met'))+
        theme(axis.text.x=element_blank(),panel.grid.major = element_line(color='grey75'),
              axis.title.y=element_text(size=8))+  
        labs(x='',y='Days Below\nBaseflow Target',fill='')+
        coord_cartesian(xlim=c(minYear-0.5,maxYear+0.5)) 
      
      p1_baseflow_min_flow <- whitewater_base_eval_raw_yrType %>% 
        mutate(min_flow = round(min_flow, 0)) %>% 
        ggplot(aes(water_year,min_flow))+
        # geom_hpline(aes(water_year,target_min_flow),color='red')+
        geom_line(linewidth=1.05)+
        geom_point_interactive(aes(
          tooltip= glue(
            "Water Year: {water_year}
          Baseflow Minimum [cfs]: {min_flow}"),
          fill=flag_achievement),
        shape=21,size=3)+
        scale_x_continuous(breaks=c(1991:2023))+
        scale_y_continuous(expand=c(0.1,0.15))+
        theme_minimal(base_size=11)+
        scale_fill_manual(values=c('#377eb8','#ffff33'),breaks=c('TRUE','FALSE'),labels=c('Met','Not Met'))+
        theme(axis.text.x=element_blank(),panel.grid.major = element_line(color='grey75'),
              axis.title.y=element_text(size=8))+  
        labs(x='',y='Minimum Baseflow\n(cfs)',fill='')+
        coord_cartesian(xlim=c(minYear-0.5,maxYear+0.5)) 
      
  
      # p1_totBD <- lower_gunnison_rec_raw %>% 
      #   filter(Type=='Whitewater Recreation', 
      #          Name=='Total') %>% 
      #   mutate(legend_name = 'Average Total\nBoatable Days') %>% 
      #   ggplot(aes(water_year, Result))+geom_line()+geom_point(shape=21,size=5,fill='black')+
      #   scale_x_continuous(breaks=c(1991:2023))+
      #   scale_y_continuous(expand=c(0.1,0.1), breaks=c(0,120,200,240,300,360))+
      #   theme_minimal(base_size=11)+
      #   theme(axis.text.x=element_blank(),panel.grid.major = element_line(color='grey75'))+
      #   scale_fill_ptol()+
      #   geom_hline(aes(yintercept = `Long Term Average`,color=legend_name),linetype='dotted')+
      #   scale_color_manual(values='black')+
      #   labs(x='',y='WW: Total\nBoatable Days',color='')+
      #   coord_cartesian(xlim=c(minYear-0.5,maxYear+0.5)) 
      # 
      p1_BD <- lower_gunnison_rec_raw %>% 
        mutate(Name= factor(Name,labels=c('Lower Acceptable','Optimal','Upper Acceptable','Total'))) %>% 
        filter(Name!='Total') %>% 
        mutate(Value = round(Value, 0)) %>% 
        ggplot(aes(water_year, Value,fill=fct_rev(Name)))+
        geom_col_interactive(
          aes(
            tooltip= glue(
              "Water Year: {water_year}
               User Preference: {Name}
               Whitewater Boatable Days: {Value}"))
        )+
        scale_x_continuous(breaks=c(1991:2023))+
        scale_y_continuous(expand=c(0.05,0.05), breaks=c(0,120,240,360))+
        theme_minimal(base_size=11)+
        theme(axis.text.x=element_text(angle=90),panel.grid.major = element_line(color='grey75'),
              axis.title.y=element_text(size=8))+  
        scale_fill_ptol()+
        labs(x='Water Year (Apr - Mar)',y='WW:\nBoatable Days',fill='User Preference')+
        coord_cartesian(xlim=c(minYear-0.5,maxYear+0.5)) 
      
   
      girafe(
        ggobj =(p1_May_1st /p1_peak / p1_maintenance /p1_improvement / p1_baseflow_min_days /p1_baseflow_min_flow/p1_BD),
        width_svg = 8,
        height_svg=8,
        options = list(
          opts_tooltip(
            opacity = 1, use_fill = FALSE,
            use_stroke = TRUE, 
            css = "padding:5pt;font-family: Open Sans;color:black"),
          opts_hover_inv(css = "opacity:0.5"), 
          opts_hover(
            css = girafe_css(
              css = "fill:#4c6061;",
              text = "stroke:none;fill:white;fill-opacity:1;font-size:larger"
            )
          )
        )
      )
    })
  }) %>%
    bindEvent(input$evalButton)
  
  #### LG Environmental Summary Page ####

  observe({
    output$LG_env_results<- renderPlot({
      
      title_name <- paste0('Analysis Period:  ',start_year(),' - ',end_year())
      
      
      p_year_type <- black_canyon_spring_peak %>% 
        mutate(year_type = factor(year_type, levels =c('Dry','Mod Dry','Avg Dry','Avg Wet','Mod Wet','Wet'))) %>% 
        group_by(year_type,.drop=FALSE) %>% 
        tally() %>% 
        mutate(pct_n = 100*n/sum(n)) %>% 
        mutate(
          exp_freq = case_when(
            year_type == 'Dry' ~ 10,
            year_type == 'Mod Dry' ~ 20,
            year_type == 'Avg Dry' ~ 20,
            year_type == 'Avg Wet' ~ 20,
            year_type == 'Mod Wet' ~ 20,
            year_type == 'Wet' ~ 10
          )
        ) %>% 
        ggplot(aes(year_type, pct_n/100))+geom_col(fill='#6a3d9a',color='black')+
        geom_col(aes(year_type,exp_freq/100),color='red',fill=NA)+
        scale_y_continuous(labels = scales::percent,breaks=pretty_breaks(n=5))+
        #scale_x_continuous(breaks=seq(500,14000,1000),expand = c(0,0))+
        # coord_cartesian(xlim=c(0,14000))+
        labs(x='Year Type Based on\n Apr-Jul Blue Mesa Inflow\n(May 1st Forecast)',y='Percent of Years in Record')+
        theme_minimal(base_size=12)
      
      p_peak_dat<- whitewater_spring_peak_raw_yrtype %>%
        mutate(
          flow_bin = case_when(
            value <900 ~ 'Below Target',
            value >=900 & value <2600 ~ 'Dry\n(900-2600 CFS)',
            value >=2600 & value <8070 ~ 'Mod Dry\n(2600-8070 CFS)',
            value >=8070 & value <14350 ~ 'Avg Dry\n(8070-14350 CFS)',
            value >=14350 & value <15000 ~ 'Avg Wet & Mod Wet\n(14350-15000 CFS)', 
            value >=15000 ~ 'Wet\n(>=15000 CFS)',
            .default = NA 
            ),
          flow_bin=factor(flow_bin, levels=c('Below Target','Dry\n(900-2600 CFS)','Mod Dry\n(2600-8070 CFS)',
                                             'Avg Dry\n(8070-14350 CFS)','Avg Wet & Mod Wet\n(14350-15000 CFS)',
                                             'Wet\n(>=15000 CFS)'))
        ) %>% 
        group_by(flow_bin,.drop = FALSE) %>% 
        summarize(n=n()) %>% 
        mutate(pct_n = 100*n/sum(n)) %>% 
        mutate(
          expecteded_freq= case_when(
            flow_bin== 'Below Target'~0,
            flow_bin== 'Dry\n(900-2600 CFS)'~ 10,
            flow_bin=='Mod Dry\n(2600-8070 CFS)'~ 20,
            flow_bin== 'Avg Dry\n(8070-14350 CFS)'~20,
            flow_bin== 'Avg Wet & Mod Wet\n(14350-15000 CFS)'~40,
            flow_bin == 'Wet\n(>=15000 CFS)'~10
          )
        )
        
      p_peak<- p_peak_dat %>% 
        ggplot(aes(flow_bin, pct_n/100))+geom_col(fill='#386cb0',color='black')+
        geom_col(aes(flow_bin,expecteded_freq/100),color='red',fill=NA)+
        scale_y_continuous(labels = scales::percent,breaks=pretty_breaks(n=5))+
        #scale_x_continuous(breaks=seq(500,14000,1000),expand = c(0,0))+
        # coord_cartesian(xlim=c(0,14000))+
        labs(x='Spring Peak Flow [cfs]',y='Percent of Years in Record')+
        theme_minimal(base_size=12)
      
      p_maint_dat<- whitewater_high_flow_duration_yrType %>% 
        mutate(
          maint_bin = case_when(
            `Maintenance (>8070 cfs)` >=60 ~ '>60\n(Wet)',
            `Maintenance (>8070 cfs)` >=40 ~ '40 - 60\n(Mod Wet)',
            `Maintenance (>8070 cfs)` >=20 ~ '20 - 40\n(Avg Wet)',
            `Maintenance (>8070 cfs)` >=10 ~'10 - 20\n(Avg Dry)',
            `Maintenance (>8070 cfs)` >=0 ~ '0 - 10\n(Mod Dry & Dry)',
          ),
          maint_bin=factor(maint_bin, levels=c('0 - 10\n(Mod Dry & Dry)','10 - 20\n(Avg Dry)', 
                                               '20 - 40\n(Avg Wet)','40 - 60\n(Mod Wet)','>60\n(Wet)'))
          ) %>%
        group_by(maint_bin,.drop=FALSE) %>% 
       tally() %>% 
        mutate(pct_n = 100*n/sum(n)) %>% 
        mutate(
          maint_freq= case_when(
            maint_bin== '>60\n(Wet)'~10,
            maint_bin== '40 - 60\n(Mod Wet)'~ 20,
            maint_bin=='20 - 40\n(Avg Wet)'~ 20,
            maint_bin== '10 - 20\n(Avg Dry)'~20,
            maint_bin== '0 - 10\n(Mod Dry & Dry)'~30)
        )
      
      p_maint <- p_maint_dat %>% 
        ggplot(aes(maint_bin, pct_n/100))+geom_col(fill='#8dd3c7',color='black')+
        geom_col(aes(maint_bin,maint_freq/100),color='red',fill=NA)+
        scale_y_continuous(labels = scales::percent,breaks=pretty_breaks(n=5))+
        #scale_x_continuous(breaks=seq(500,14000,1000),expand = c(0,0))+
        # coord_cartesian(xlim=c(0,14000))+
        labs(x='Maintenance Flow (>8070 CFS)\n Duration [days]',y='Percent of Years in Record')+
        theme_minimal(base_size=12)
            
      p_improve_dat<- whitewater_high_flow_duration_yrType %>% 
        mutate(
          improve_bin = case_when(
            `Improvement (>14350)` >=15 ~ '>15\n(Wet)',
            `Improvement (>14350)` >=10 ~ '10 - 15\n(Mod Wet)',
            `Improvement (>14350)` >=2 ~ '2 - 10\n(Avg Wet)',
            `Improvement (>14350)` >=0 ~ '0 - 1\n(Avg Dry, Mod Dry, Dry)'),
          improve_bin=factor(improve_bin, levels=c('0 - 1\n(Avg Dry, Mod Dry, Dry)','2 - 10\n(Avg Wet)',
                                                   '10 - 15\n(Mod Wet)','>15\n(Wet)'))
        ) %>%
        group_by(improve_bin,.drop=FALSE) %>% 
        summarize(n=n()) %>% 
        mutate(pct_n = 100*n/sum(n)) %>% 
        mutate(
          improve_freq= case_when(
            improve_bin== '>15\n(Wet)'~10,
            improve_bin== '10 - 15\n(Mod Wet)'~ 20,
            improve_bin=='2 - 10\n(Avg Wet)'~ 20,
            improve_bin== '0 - 1\n(Avg Dry, Mod Dry, Dry)'~50
          )
        )  
        
      p_improve <- p_improve_dat %>% 
        ggplot(aes(improve_bin, pct_n/100))+geom_col(fill='#fdb462',color='black')+
        geom_col(aes(improve_bin,improve_freq/100),color='red',fill=NA)+
        scale_y_continuous(labels = scales::percent,breaks=pretty_breaks(n=5))+
        #scale_x_continuous(breaks=seq(500,14000,1000),expand = c(0,0))+
        # coord_cartesian(xlim=c(0,14000))+
        labs(x='Improvement Flow (>14350 CFS) \n Duration [days]',y='Percent of Years in Record')+
        theme_minimal(base_size=12) 
      
      p_base <- whitewater_base_eval_raw_yrType %>% 
        ggplot(aes(blw_base_flow))+
        geom_histogram(aes(y = after_stat(count / sum(count))), binwidth = 25,
                                                 fill='#bebada',color='black')+
        scale_y_continuous(labels = scales::percent,breaks=pretty_breaks(n=3))+
        scale_x_continuous(breaks=seq(0,225,25),expand=c(0.05,0.05))+
        scale_fill_ptol()+
        # coord_cartesian(xlim=c(0,14000))+
        labs(x='Days Below Baseflow Target',y='Percent of Years in Record')+
        theme_few(base_size=12)+
        theme(axis.text.x = element_text(angle = 90,vjust=0.5))+
        theme(legend.position = 'none')#+
        #coord_cartesian(xlim=c(0,225))
      

       p<- p_year_type+p_peak + p_maint+p_improve+p_base + plot_layout(ncol=2)+
         plot_annotation(title = title_name, theme = theme(plot.title = element_text(size = 18,hjust=0.5,
                                                                                     margin = margin(0,0,10,0))))
       p
    })
  }) %>%
    bindEvent(input$evalButton)

  #### LG Recreational Summary Page ####

  observe({

    output$LG_rec_by_User<- renderPlot({




      lower_gunnison_rec_raw %>%
        filter(Name !='Total') %>%
        ggplot(aes(Value))+
        geom_histogram(aes(y = after_stat(count / sum(count)),fill=Name), binwidth = 10,
                       color='black')+
        scale_y_continuous(labels = scales::percent,breaks=pretty_breaks(n=5))+
        scale_x_continuous(breaks=seq(0,365,10),expand=c(0,0))+
        scale_fill_few()+
        # coord_cartesian(xlim=c(0,14000))+
        labs(x='Annual Boatable Days',y='Percent of Years in Record',fill='User Preference',title=title_name)+
        theme_minimal(base_size=12)+
        theme(axis.text.x = element_text(angle = 90))+
        facet_wrap(~Name,ncol=1)+
        theme(legend.position = 'none',plot.title = element_text(size = 18,hjust=0.5))
    })

    lg_rec_monthly_bda<- whitewater_BDA_class %>%
      filter(lg_WW !=  'Not Acceptable') %>%
      mutate(month = month(date, label=TRUE),
             water_year=factor(water_year))%>%
      group_by(water_year, month,.drop=FALSE) %>%
      tally()

    output$LG_rec_monthly<- renderPlot({

      p1<- lg_rec_monthly_bda %>%
        ungroup() %>% 
        ggplot(aes(n))+
        geom_histogram(aes(y = after_stat(count / sum(count)),fill=month), binwidth = 5,
                       color='black')+
        scale_y_continuous(labels = scales::percent,breaks=pretty_breaks(n=3))+
        scale_x_continuous(breaks=seq(0,31,5),expand=c(0,0))+
        scale_fill_ptol()+
        # coord_cartesian(xlim=c(0,14000))+
        labs(x='Monthly Boatable Days',y='Percent of Years in Record',fill='User Preference')+
        theme_few(base_size=15)+
        theme(axis.text.x = element_text(angle = 90))+
        facet_wrap(~month,ncol=1)+
        theme(legend.position = 'none')

      
      monthly_bda<- lg_rec_monthly_bda %>% 
        #mutate(Season= ifelse(month %in% c('May','Jun','Jul','Aug','Sep','Oct'), 'In Season','Out of Season')) %>% 
        ggplot(aes(month, n))+geom_boxplot(aes(fill=month),outlier.shape = NA)+
        geom_jitter()+
        scale_fill_ptol()+
        #scale_fill_manual(values=c('#67a9cf','#999999'))+
        scale_y_continuous(breaks=seq(0,31,5),expand=c(0.1,0.1))+
        labs(y='Monthly\nBoatable Days',x='Month',fill='')+
        theme_few(base_size=15)+
        theme(legend.position = 'none')
      
      median_bda<- lg_rec_monthly_bda %>% 
        group_by(month) %>% 
        summarize(median = median(n)) %>% 
       # mutate(Season= ifelse(month %in% c('May','Jun','Jul','Aug','Sep','Oct'), 'In Season','Out of Season')) %>% 
        ggplot(aes(month, median))+geom_col(aes(fill=month),color='black')+
        scale_fill_ptol()+
        scale_y_continuous(breaks=seq(0,31,5),expand=c(0.1,0.1))+
        labs(y='Median\nBoatable Days',x='Month',fill='')+
        theme_few(base_size=15)+
        theme(legend.position = 'none')
      
      (monthly_bda + median_bda) / p1 + plot_layout(heights = c(.3,1))

      #p1 / plot_spacer()+ monthly_bda + plot_layout(heights = c(.8,.1,.2))
    })
  }) %>%
    bindEvent(input$evalButton)


  
  
  
  #### Calculate Custom Results  #####
  
  
  ##### Prep Analysis #####
  
  observe({
    bm_forecast<<- tibble(forecast_kaf=input$blue_mesa_forecast) %>% 
      mutate(
        year_type = 
          case_when(
           forecast_kaf < 381 ~ 'Dry',
           forecast_kaf >= 381 & forecast_kaf <= 516 ~ 'Mod Dry',
           forecast_kaf > 516 & forecast_kaf <= 709 ~ 'Avg Dry',
           forecast_kaf > 709 & forecast_kaf <= 831 ~ 'Avg Wet',
           forecast_kaf > 831 & forecast_kaf <= 1123 ~ 'Mod Wet',
           forecast_kaf > 1123 ~ 'Wet'),
           
       bc_peak_flow_target = 
          case_when(
            forecast_kaf <= 372 ~ 482.95+1.44*(forecast_kaf),
            forecast_kaf>372 & forecast_kaf<=715 ~ 15.24*forecast_kaf - 4651.66,
            forecast_kaf> 715 & forecast_kaf<=925 ~5449.13+1.15*forecast_kaf,
            forecast_kaf>925 & forecast_kaf<=1001 ~ 14.57*forecast_kaf-6975.28,
            forecast_kaf>1001 & forecast_kaf<=1050 ~ 70.4*forecast_kaf-62886,
            forecast_kaf>= 1050 ~ 10.68*forecast_kaf-180),
        
       bc_high_flow_minimum = 
          case_when(
            forecast_kaf<=561 ~ 300,
            forecast_kaf>561 & forecast_kaf<= 690 ~ 2.692*forecast_kaf -1207.69,
            forecast_kaf > 690 & forecast_kaf<=1000 ~ 1.129 * forecast_kaf - 129,
            forecast_kaf>1000 ~ 1000
        )
  )
  }) %>% 
    bindEvent(input$runButton_custom)
  
  
  
  ##### Black Canyon ######
  
  observe({
    #load user Q for Tunnel Gauge
    userQ_Tunnel <<- as.data.frame(hot_to_r(input$customQ)) %>% 
      dplyr::rename(q_cfs = Gunnison.BelowTunnel) %>% 
      mutate(
        month = match(Month,month.abb),
        year= ifelse(month>3, 2049,2050),
        date = as.Date(paste0(year,'/',month, '/',Day)),
        water_year =2049,
        name = 'Custom Hydrograph'
      ) 
    
    tunnel_BDA_class_custom<<- userQ_Tunnel %>% 
     # filter(month %in% 5:10) %>%     
      mutate(
        black_canyon_WW = case_when(
          q_cfs < 600 ~ 'Not Acceptable',
          q_cfs >= 600 & q_cfs < 800 ~ 'Lower Acceptable',
          q_cfs >= 800 & q_cfs < 1600 ~ 'Optimal',
          q_cfs >= 1600 & q_cfs < 3000 ~ 'Upper Acceptable',
          q_cfs > 3000 ~'Not Acceptable'
        ),
        gunnison_gorge_WW = case_when(
          q_cfs <600 ~ 'Not Acceptable',
          q_cfs >=600 & q_cfs < 800 ~ 'Lower Acceptable',
          q_cfs >=800 & q_cfs < 3000 ~ 'Optimal',
          q_cfs >=3000 & q_cfs < 15000 ~ 'Upper Acceptable',
          q_cfs > 15000 ~'Not Acceptable'
        ),
        gunnison_gorge_fishing= case_when(
          q_cfs <400 ~ 'Not Acceptable',
          q_cfs >=400 & q_cfs < 500 ~ 'Lower Acceptable',
          q_cfs >=500 & q_cfs < 1000 ~ 'Optimal',
          q_cfs >=1000 & q_cfs < 3000 ~ 'Upper Acceptable',
          q_cfs > 3000 ~'Not Acceptable'
        ))
  
    # Create plot that includes custom hydrograph and E&R Targets
    output$BC_hydro_plot_customQ<- renderPlot({

      flw_target_dat_black_canyon_fil <<- flw_target_dat_black_canyon %>%
        filter(name !='hydrograph') %>% 
        mutate(fake_date_customQ= if_else(month(date)>3, fake_date_customQ, fake_date_customQ+years(1)))

      #Create Target Timeseries for Both Years
      
      custom_bc_targets <<- userQ_Tunnel %>%
        dplyr::select(-name) %>% 
        mutate(
          `Peak Target` =
            ifelse(date==ymd('2049/06/01'),
                   bm_forecast$bc_peak_flow_target[1], NA),
          `High Flow Minimum`=
            ifelse(date>=ymd('2049/05/01') & date<=ymd('2049/07/25'),
                   bm_forecast$bc_high_flow_minimum [1], NA),
          Baseflow= 300
        ) %>%
        pivot_longer(cols=c(`Peak Target`:Baseflow),names_to='name') %>% 
        mutate(
          fake_date_customQ = date
        )
      
      line_key<- tibble(
        name = custom_bc_targets$name %>% unique(.),
        line_colors= c('yellow','#01665e','orange'),
        line_types=c('solid','solid','solid'),
      ) %>%
        filter(name != 'Peak Target') %>%
        mutate(
          name = factor(name, levels=c('Baseflow','High Flow Minimum','Peak Target','Hydrograph'))
        ) %>%
        arrange(name)
      
        

      p<- plot_BC_hydro_plot_customQ_updated(Q= userQ_Tunnel,linekey = line_key,dat=flw_target_dat_black_canyon_fil )
      p
    })
  }) %>% 
      bindEvent(input$runButton_custom)
    
  observe({
    # Spring Peak Instantaneous Flow 
    user_black_canyon_spring_peak<<- evaluate_peak_targets_for_plot(
      Q_df=userQ_Tunnel,
      target_meta_df=black_canyon_peak_target_meta,
      roll_statistic='mean', start_month=5,
      start_day =01, end_month=6,
      end_day=30, flow_tolerance= flow_tol
    )
    
    user_black_canyon_spring_peak_result <- ifelse(
      user_black_canyon_spring_peak$value[1]>bm_forecast$bc_peak_flow_target[1],
      paste0('Peak flows are ', user_black_canyon_spring_peak$value[1],' cfs. Peak flows have met the target peak flow of ',
             bm_forecast$bc_peak_flow_target[1], ' cfs'),
      'No Peak Flow Target Met')
    
    # Spring Peak High Flow Minimum 
    Q_filter<- userQ_Tunnel %>% 
      filter(date >= as.Date(paste(year(date), 05, 04, sep = "-")),
             date <= as.Date(paste(year(date), 07, 01, sep = "-"))) 
    
    out<- Q_filter %>% 
      group_by(water_year) %>% 
      mutate(target_flows_adj = bm_forecast$bc_high_flow_minimum[1]-flow_tol) %>% 
      summarize(
        blw_base_flow_days = sum(q_cfs <=(target_flows_adj))/(24*4)
      ) %>% 
      mutate(high_min_flag= ifelse(blw_base_flow_days<=duration_tol, TRUE,FALSE))
    
    # annual minimum daily flow during high flow period. 
    start_month=05
    start_day =04
    end_month=07
    end_day=25
    high_flow_min_flow <<- userQ_Tunnel%>% 
      filter(date >= as.Date(paste(year(date), start_month, start_day, sep = "-")),
             date <= as.Date(paste(year(date), end_month, end_day, sep = "-"))) %>% 
      group_by(water_year) %>% 
      summarize(minQ = min(q_cfs))
    
    user_black_canyon_high_flow_min_result <- ifelse(
      out$high_min_flag==TRUE,
      paste0('The High Flow Minimum  was ',high_flow_min_flow$minQ[1], ' cfs. High flow minimums met the target of ',bm_forecast$bc_high_flow_minimum[1],' cfs.'),
      paste0('The High Flow Minimum  was ',high_flow_min_flow$minQ[1], ' cfs. Flows are below the target of ',bm_forecast$bc_high_flow_minimum[1],' cfs for ', out$blw_base_flow_days,
             ' days'))
    
    # Evaluate Baseflow Minimum Target
    user_black_canyon_baseflow_avg_days_raw <- evaluate_duration_in_target_range(
      Q_df=userQ_Tunnel,
      target_meta_df=black_canyon_baseflow_duration_target,
      start_month=01,start_day =01,end_month=12, end_day=31,
      flow_tolerance=flow_tol
    ) %>%
      mutate(result = duration_days>=365,
             value=duration_days)
    
    user_black_canyon_baseflow_avg_days_result <- paste0('Minimum Baseflows of 300 cfs were met for ',
                                                         user_black_canyon_baseflow_avg_days_raw$duration_days[1],' days')
    
    #Evaluate ramping rate
    user_black_canyon_ramping_raw<- evaluate_ramping_rate(Q_df=userQ_Tunnel) %>%
      mutate(
        target_name = 'Maximum Ramping Rate',
        value=days_blw_ramping_rate
      ) %>% 
      mutate(days_blw_ramping_rate = sum(days_blw_ramping_rate )) %>% 
      dplyr::select(-pct_year_abv_ramping_rate) %>% 
      slice(1) %>% 
      mutate(days_abv_ramping = days_in_year -days_blw_ramping_rate)
    
    user_black_canyon_ramping_result <- paste0('Ramping Rates were above target rates for ',
                                               user_black_canyon_ramping_raw$days_abv_ramping[1],' days')
    
    #Recreation
    user_black_canyon_rec<- evaluate_duration_in_target_range(Q_df=userQ_Tunnel,
                                                              target_meta_df=black_canyon_recreation_targets,
                                                              start_month=05,start_day =01,end_month=10, end_day=31,
                                                              flow_tolerance=0) %>%
      left_join(black_canyon_recreation_targets) %>% 
      mutate(target_name =
               factor(target_name, levels=c('Lower Acceptable',
                                            'Optimal',
                                            'Upper Acceptable',
                                            'Total')),
             target_type='Whitewater Recreation',
             status = ifelse(duration_days >= target_goal, 'Above or Equal', 'Below'),
             value= duration_days
      ) %>% arrange(target_name) %>%
      dplyr::select(water_year ,Type= target_type, Name = target_name, Metric = metric,
                    `Long Term Average`=target_goal, Result= duration_days , Status =status,Value=value) %>% 
      mutate(result_desc = paste0(Name,' Boatable Days are: ',Result,'. The long term average number of ',
                                  Name,' Boatable Days is ',`Long Term Average`))
    
    user_bc_table <-
      tibble(
        `Target Type` = c('24 Hour Instantaneous Peak Flow','High Flow Minimum','Baseflow Minimum','Ramping Rate',
                          'Whitewater: Total','Whitewater: Upper Acceptable','Whitewater: Optimal','Whitewater: Lower'),
        Result = c(user_black_canyon_spring_peak_result,user_black_canyon_high_flow_min_result,
                   user_black_canyon_baseflow_avg_days_result,user_black_canyon_ramping_result,
                   user_black_canyon_rec$result_desc[4],user_black_canyon_rec$result_desc[3],
                   user_black_canyon_rec$result_desc[2],
                   user_black_canyon_rec$result_desc[1])
      )
    
    output$BC_customQ_dt <-DT::renderDT({user_bc_table %>%
        datatable(rownames = FALSE,filter = 'top',extensions = 'RowGroup',
                  options=(list(pageLength = 15,rowsGroup = list(0))))
    })
    
    bc_rec_monthly_bda_custom<<- tunnel_BDA_class_custom %>% 
      filter(black_canyon_WW !=  'Not Acceptable') %>% 
      mutate(month = month(date, label=TRUE),
             black_canyon_WW=factor(black_canyon_WW, 
                                    levels=c('Lower Acceptable','Optimal','Upper Acceptable'))) %>% 
      group_by(water_year,black_canyon_WW , month,.drop=FALSE) %>% 
      tally()
    
    output$bc_rec_plot_custom<- renderPlot({
      bc_rec_monthly_bda_season <- bc_rec_monthly_bda_custom %>% 
        mutate(
          Season= ifelse(month %in% c('May','Jun','Jul','Aug','Sep','Oct'), 'In Season','Out of Season'),
          black_canyon_WW_adj  = ifelse(Season == 'In Season',as.character(black_canyon_WW), 'Out of Season'),
          black_canyon_WW_adj = factor(black_canyon_WW_adj, levels=c('Lower Acceptable', 'Optimal','Upper Acceptable','Out of Season')),
          month = factor(month, levels=c('Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec','Jan','Feb','Mar'))
        ) %>%  
        group_by(month,black_canyon_WW_adj,.drop=FALSE ) %>% 
        summarise(n=sum(n)) %>% 
       # filter(as.numeric(month) %in% 5:10) %>% 
        ggplot(aes(month,n,fill=fct_rev(black_canyon_WW_adj)))+
        geom_col()+
        labs(x='Month',y='Monthly\nBoatable Days',fill='')+
        theme_minimal(base_size=15)+
        theme(axis.text.x = element_text(angle = 90,vjust=0.5))+
        scale_fill_manual(values=c('grey50','#00BA38','#619Cff','#F8766D'))+
        scale_y_continuous(limits=c(0,31))
      bc_rec_monthly_bda_season
    })
    
    
    
    output$bc_rec_plot_total<- renderPlot({
      bc_rec_monthly_bda_custom %>% 
        mutate(
          Season= ifelse(month %in% c('May','Jun','Jul','Aug','Sep','Oct'), 'In Season','Out of Season'),
          black_canyon_WW_adj  = ifelse(Season == 'In Season',as.character(black_canyon_WW), 'Out of Season'),
          black_canyon_WW_adj = factor(black_canyon_WW_adj, levels=c('Lower Acceptable', 'Optimal','Upper Acceptable','Out of Season')),
          month = factor(month, levels=c('Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec','Jan','Feb','Mar'))
        ) %>%  
       # filter(as.numeric(month) %in% 5:10) %>% 
        group_by(black_canyon_WW_adj,.drop=FALSE) %>% 
        summarize(n=sum(n)) %>% 
        ggplot(aes(x=1,n,fill=fct_rev(black_canyon_WW_adj)))+
        geom_col()+
        labs(x='',y='Annual Boatable Days',fill='')+
        theme_minimal(base_size=15)+
        theme(axis.text.x = element_text(angle = 90,vjust=0.5),
              axis.text.y = element_blank())+
        scale_fill_manual(values=c('grey50','#00BA38','#619Cff','#F8766D'))+
        scale_y_continuous(breaks=pretty_breaks(n=10),limits=c(0,366))+
        coord_flip()
    })
  }) %>% 
    bindEvent(input$runButton_custom)
  
  ##### Gunnison Gorge #####
  observe({  
    
    output$GG_hydro_plot_customQ<- renderPlot({
      
      line_key<- tibble(
        name = (flw_target_dat_gunnison_gorge$name %>% unique(.))[1:5],
        line_colors= c('orange','#6a3d9a','#b15928','#33a02c','#33a02c'),
        type = c('Aquatic','Aquatic','Aquatic','Aquatic','Aquatic'),
        
        line_types=c('solid','solid','solid','solid','solid'),
        labels=c('Baseflow','Rainbow Recruitment Max','Brown Recruitment Max','Optimal Trout Min','Optimal Trout Max')
      )
      
      flw_target_dat_gunnison_gorge_fil <- flw_target_dat_gunnison_gorge  %>% 
        mutate(fake_date_customQ= if_else(month(date)>3, fake_date_customQ, fake_date_customQ+years(1)))
      
      
      p<- plot_GG_hydro_plot_customQ(Q= userQ_Tunnel,linekey = line_key,dat=flw_target_dat_gunnison_gorge_fil )
      p
    })
    
    
    # Evaluation of Maximum Limits for Peak Flows Suitable For Trout 
    ggnca_max_flow_exceedance_duration_rainbow_raw <<-
      evaluate_duration_in_target_range(Q_df=userQ_Tunnel, target_meta_df=ggnca_peak_duration_target,
                                        start_month=6,start_day =01,end_month=6, end_day=30,
                                        flow_tolerance=flow_tol) 
    
    ggnca_max_flow_exceedance_duration_rainbow_result <<- 
      paste0('Flows exceeded the Rainbow Trout spring maximum flow target for ',
             ggnca_max_flow_exceedance_duration_rainbow_raw$duration_days[1],' days.')
    
    ggnca_max_flow_exceedance_duration_brown_raw <<-
      evaluate_duration_in_target_range(Q_df=userQ_Tunnel,target_meta_df=ggnca_peak_duration_target,
                                        start_month=4,start_day =15,end_month=6, end_day=01,
                                        flow_tolerance=5) 
    
    ggnca_max_flow_exceedance_duration_brown_result <<- 
      paste0('Flows exceeded the Brown Trout spring maximum flow target for ',
             ggnca_max_flow_exceedance_duration_brown_raw$duration_days[1],' days.')
    
    
    #Evaluation of Ideal Flow Range for TrouT
    ggnca_fish_range_raw <<- evaluate_duration_in_target_range(Q_df=userQ_Tunnel,target_meta_df=ggnca_range_duration_target,
                                                              start_month=01,start_day =01,end_month=12, end_day=31,
                                                              flow_tolerance=flow_tol)
    
    ggnca_fish_range_result <- 
      paste0('Flows were within the ideal flow range of 400-1200 cfs for trout habitat for ',
             ggnca_fish_range_raw$duration_days[1],' days.')
    
    #Evaluation of Baseflows
    ggnca_baseflow_avg_days_raw <<- evaluate_duration_in_target_range(Q_df=userQ_Tunnel,target_meta_df=ggnca_baseflow_duration_target,
                                                                     start_month=01,start_day =01,end_month=12, end_day=31,
                                                                     flow_tolerance=flow_tol)
    ggnca_baseflow_avg_days_result <- 
      paste0('Flows were above minimum flows of 300 cfs for ',
             ggnca_baseflow_avg_days_raw$duration_days[1],' days.')
    
    
    user_ggnca_rec<<- evaluate_duration_in_target_range(Q_df=userQ_Tunnel,
                                                       target_meta_df=ggcna_recreation_targets,
                                                       start_month=05,start_day =01,end_month=10, end_day=31,
                                                       flow_tolerance=0) %>%
      summarize_evaluate_duration_in_target_range(target_meta_df =ggcna_recreation_targets ) %>%
      mutate(
        target_name =
          factor(target_name,levels=c('WW Lower Acceptable','WW Optimal',
                                      'WW Upper Acceptable', 'WW Total',
                                      'Ang Lower Acceptable','Ang Optimal',
                                      'Ang Upper Acceptable', 'Ang Total)',
                                      'Max Flow','Max'))) %>% arrange(target_name) %>%
      mutate(
        target_type= c('Whitewater','Whitewater',
                       'Whitewater', 'Whitewater',
                       'Angling','Angling ',
                       'Angling', 'Angling',
                       'Camping/Trail Access','Safety'),
        
        target_name = factor(target_name,labels=c('Lower Acceptable','Optimal',
                                                  'Upper Acceptable', 'Total',
                                                  'Lower Acceptable','Optimal',
                                                  'Upper Acceptable', 'Total',
                                                  'Max Flow','Max'))
      ) %>%
      mutate(result_desc = paste0(target_type,' ',target_name,' Boatable Days are: ',result,'. The long term average number of ',
                                  target_name,' Boatable Days is ',target_goal))
    
    user_gg_table <<-
      tibble(
        `Target Type` = c('Maximum Spring Flow for Rainbow Trout','Maximum Spring Flow for Brown Trout','Trout Habitat Ideal Flows','Baseflow',
                          'Whitewater: Total','Whitewater: Upper Acceptable','Whitewater: Optimal','Whitewater: Lower',
                          'Angling: Total','Angling: Upper Acceptable','Angling: Optimal','Angling: Lower'),
        Result = c(ggnca_max_flow_exceedance_duration_rainbow_result,ggnca_max_flow_exceedance_duration_brown_result,
                   ggnca_fish_range_result,ggnca_baseflow_avg_days_result,
                   user_ggnca_rec$result_desc[4],
                   user_ggnca_rec$result_desc[3],
                   user_ggnca_rec$result_desc[2],
                   user_ggnca_rec$result_desc[1],
                   user_ggnca_rec$result_desc[8], 
                   user_ggnca_rec$result_desc[7],
                   user_ggnca_rec$result_desc[6], 
                   user_ggnca_rec$result_desc[5])
      )
    
    output$GG_customQ_dt <-DT::renderDT({
      user_gg_table %>%
        filter(`Target Type` !='Safety') %>% 
        datatable(rownames = FALSE,filter = 'top',extensions = 'RowGroup',
                  options=(list(pageLength = 15,rowsGroup = list(0))))
    })
  }) %>% 
    bindEvent(input$runButton_custom)
  
  observe({
    
    gg_rec_monthly_bda_custom_ww<<- tunnel_BDA_class_custom %>% 
      filter(gunnison_gorge_WW !=  'Not Acceptable') %>% 
      mutate(month = month(date, label=TRUE),
             gunnison_gorge_WW=factor(gunnison_gorge_WW, 
                                    levels=c('Lower Acceptable','Optimal','Upper Acceptable'))) %>% 
      group_by(water_year,gunnison_gorge_WW , month,.drop=FALSE) %>% 
      tally()
    
    output$gg_rec_plot_custom_ww<- renderPlot({
     gg_rec_monthly_bda_custom_ww %>% 
        mutate(
          Season= ifelse(month %in% c('May','Jun','Jul','Aug','Sep','Oct'), 'In Season','Out of Season'),
          gunnison_gorge_WW_adj  = ifelse(Season == 'In Season',as.character(gunnison_gorge_WW), 'Out of Season'),
          gunnison_gorge_WW_adj = factor(gunnison_gorge_WW_adj, levels=c('Lower Acceptable', 'Optimal','Upper Acceptable','Out of Season')),
          month = factor(month, levels=c('Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec','Jan','Feb','Mar'))
        ) %>%  
        group_by(month,gunnison_gorge_WW_adj,.drop=FALSE ) %>% 
        summarise(n=sum(n)) %>% 
        ggplot(aes(month,n,fill=fct_rev(gunnison_gorge_WW_adj)))+
        geom_col()+
        labs(x='Month',y='Whitewater Boatable Days',fill='')+
        theme_minimal(base_size=13)+
        theme(axis.text.x = element_text(angle = 90,vjust=0.5))+
        scale_fill_manual(values=c('grey50','#00BA38','#619Cff','#F8766D'))+
        scale_y_continuous(limits=c(0,31))
    })
    
    output$gg_rec_plot_total_ww<- renderPlot({
      gg_rec_monthly_bda_custom_ww %>% 
        mutate(
          Season= ifelse(month %in% c('May','Jun','Jul','Aug','Sep','Oct'), 'In Season','Out of Season'),
          gunnison_gorge_WW_adj  = ifelse(Season == 'In Season',as.character(gunnison_gorge_WW), 'Out of Season'),
          gunnison_gorge_WW_adj = factor(gunnison_gorge_WW_adj, levels=c('Lower Acceptable', 'Optimal','Upper Acceptable','Out of Season')),
          month = factor(month, levels=c('Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec','Jan','Feb','Mar'))
        ) %>%  
        # filter(as.numeric(month) %in% 5:10) %>% 
        group_by(gunnison_gorge_WW_adj,.drop=FALSE) %>% 
        summarize(n=sum(n)) %>% 
        ggplot(aes(x=1,n,fill=fct_rev(gunnison_gorge_WW_adj)))+
        geom_col()+
        labs(x='',y='Annual Whitewater\nBoatable Days',fill='')+
        theme_minimal(base_size=15)+
        theme(axis.text.x = element_text(angle = 90,vjust=0.5),
              axis.text.y = element_blank())+
        scale_fill_manual(values=c('grey50','#00BA38','#619Cff','#F8766D'))+
        scale_y_continuous(breaks=pretty_breaks(n=10),limits=c(0,366))+
        coord_flip()
    })
    gg_rec_monthly_bda_custom_angling<<- tunnel_BDA_class_custom %>% 
      filter(gunnison_gorge_fishing !=  'Not Acceptable') %>% 
      mutate(month = month(date, label=TRUE),
             gunnison_gorge_fishing=factor(gunnison_gorge_fishing, 
                                      levels=c('Lower Acceptable','Optimal','Upper Acceptable'))) %>% 
      group_by(water_year,gunnison_gorge_fishing , month,.drop=FALSE) %>% 
      tally()
    
    output$gg_rec_plot_custom_ang<- renderPlot({
     gg_rec_monthly_bda_custom_angling %>% 
        mutate(
          Season= ifelse(month %in% c('May','Jun','Jul','Aug','Sep','Oct'), 'In Season','Out of Season'),
          gunnison_gorge_fishing_adj  = ifelse(Season == 'In Season',as.character(gunnison_gorge_fishing), 'Out of Season'),
          gunnison_gorge_fishing_adj = factor(gunnison_gorge_fishing_adj, levels=c('Lower Acceptable', 'Optimal','Upper Acceptable','Out of Season')),
          month = factor(month, levels=c('Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec','Jan','Feb','Mar'))
        ) %>%  
        group_by(month,gunnison_gorge_fishing_adj,.drop=FALSE ) %>% 
        summarise(n=sum(n)) %>% 
        ggplot(aes(month,n,fill=fct_rev(gunnison_gorge_fishing_adj)))+
        geom_col()+
        labs(x='Month',y='Angling Boatable Days',fill='')+
        theme_minimal(base_size=13)+
        theme(axis.text.x = element_text(angle = 90,vjust=0.5))+
        scale_fill_manual(values=c('grey50','#00BA38','#619Cff','#F8766D'))
    })
    
    output$gg_rec_plot_total_ang<- renderPlot({
      
      gg_rec_monthly_bda_custom_angling %>% 
        mutate(
          Season= ifelse(month %in% c('May','Jun','Jul','Aug','Sep','Oct'), 'In Season','Out of Season'),
          gunnison_gorge_fishing_adj  = ifelse(Season == 'In Season',as.character(gunnison_gorge_fishing), 'Out of Season'),
          gunnison_gorge_fishing_adj = factor(gunnison_gorge_fishing_adj, levels=c('Lower Acceptable', 'Optimal','Upper Acceptable','Out of Season')),
          month = factor(month, levels=c('Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec','Jan','Feb','Mar'))
        ) %>%  
        # filter(as.numeric(month) %in% 5:10) %>% 
        group_by(gunnison_gorge_fishing_adj,.drop=FALSE) %>% 
        summarize(n=sum(n)) %>% 
        ggplot(aes(x=1,n,fill=fct_rev(gunnison_gorge_fishing_adj)))+
        geom_col()+
        labs(x='',y='Annual Angling\nBoatable Days',fill='')+
        theme_minimal(base_size=15)+
        theme(axis.text.x = element_text(angle = 90,vjust=0.5),
              axis.text.y = element_blank())+
        scale_fill_manual(values=c('grey50','#00BA38','#619Cff','#F8766D'))+
        scale_y_continuous(breaks=pretty_breaks(n=10),limits=c(0,366))+
        coord_flip()
      
      
    })
  }) %>% 
    bindEvent(input$runButton_custom)
  
  ##### Lower Gunnison #####
  observe({  
    
    userQ_whitewater <<- as.data.frame(hot_to_r(input$customQ)) %>% 
      dplyr::rename(q_cfs = Gunnison.Whitewater) %>% 
      mutate(
        month = match(Month,month.abb),
        year= ifelse(month>3, 2049,2050),
        date = as.Date(paste0(year,'/',month, '/',Day)),
        water_year =2049,
        name = 'Custom Hydrograph'
      ) 
    
    whitewater_BDA_class_custom<<- userQ_whitewater %>% 
      # filter(month %in% 5:10) %>%     
      mutate(
        lg_WW = case_when(
          q_cfs < 800 ~ 'Not Acceptable',
          q_cfs >= 800 & q_cfs < 1000 ~ 'Lower Acceptable',
          q_cfs >= 1000 & q_cfs < 15000 ~ 'Optimal',
          q_cfs >= 15000 & q_cfs < 20000 ~ 'Upper Acceptable'
        )
      )
    
    output$LG_hydro_plot_customQ<- renderPlot({
      
      flw_target_dat_whitewater_fil <<- flw_target_dat_whitewater %>%
        mutate(year_type = case_when(
          year_type == 'Wet' ~ '10% frequency (Wet)',
          year_type == 'Mod Wet' ~'30% frequency (Mod Wet)',
          year_type == 'Avg Wet' ~ '50% frequency (Avg Wet)',
          year_type == 'Avg Dry'~ '70% frequency (Avg Dry)',
          year_type == 'Mod Dry' ~ '90% frequency (Mod Dry)',
          year_type == 'Dry' ~ '100% frequency (Dry)')
        ) %>% 
        #filter(year_type=='50% frequency (Avg Wet)') %>%
        filter(year_type   ==as.character(input$select_target_yr_type_lg_customQ)) %>%
        filter(!(name %in% c('hydrograph_improvement','hydrograph_maintenance'))) %>% 
        mutate(fake_date_customQ= if_else(month>3, fake_date_customQ, fake_date_customQ+years(1)))
      
      line_key<- tibble(
        name = flw_target_dat_whitewater$name %>% unique(.),
        line_colors= c('orange','#01665e','#8c510a','#5ab4ac','#d8b365','yellow','black','black'),
        line_types=c('solid','solid','solid','solid','solid','solid','dashed','dotted'),
        labels=c('Baseflow','Half Fullbank Maintenance','Fullbank Maintenance',
                 'Half Fullbank Improvement','Fullbank Improvement',
                 'Instaneous Peak','Maintenance Hydrograph','Improvement Hydrograph')
      ) %>%
        filter(name != 'peak') %>%
        mutate(
          name = factor(name, levels=c('base', 'half_fullbank_improvement','half_fullbank_maintenance',
                                       'fullbank_improvement', 'fullbank_maintenance',
                                       'hydrograph_maintenance', 'hydrograph_improvement'))
        ) %>%
        arrange(name)
      
      
      p <- plot_LG_hydro_plot_customQ(Q= userQ_whitewater,linekey = line_key,dat=flw_target_dat_whitewater_fil )
      p
      #ggplotly(p, tooltip = 'text')
    })
  }) %>% 
    bindEvent(input$runButton_custom)
  
  observe({
    #instantaneous peak 
    user_whitewater_spring_peak_raw <<- evaluate_peak_targets_for_plot(Q_df=userQ_whitewater,
                                                                      target_meta_df=whitewater_peak_target_meta,
                                                                      peak_window_size=1, roll_statistic='mean',
                                                                      start_month=5,start_day =01,end_month=6, end_day=30,
                                                                      flow_tolerance=flow_tol) %>% 
      separate(target_name, into=c('target_name','target_flows'),sep='\\:') %>% 
      mutate(target_flows=as.numeric(str_trim(target_flows)))   %>% 
      mutate(year_type = case_when(
        target_flows == 900 ~ 'Dry',
        target_flows == 2600 ~ 'Mod Dry',
        target_flows == 8070 ~ 'Avg Dry',
        target_flows == 14350 ~ 'Avg Wet / Mod Wet',
        target_flows == 15000 ~ 'Wet'
      ))
    

    user_whitewater_spring_peak_raw <<- 
      user_whitewater_spring_peak_raw %>%
      filter(result==TRUE) %>%
      slice_max(target_flows)
    
    user_whitewater_spring_peak_result<- ifelse(
      nrow(user_whitewater_spring_peak_raw>0),
      paste0('Peak flow is ', user_whitewater_spring_peak_raw$value [1],' cfs. Target for ',user_whitewater_spring_peak_raw$year_type[1],' year type is met.'),
      'Peak flow is below Dry year type target.')
    
    user_whitewater_high_flow_duration_result <-evaluate_duration_in_target_range(Q_df=userQ_whitewater,
                                                                                  target_meta_df=whitewater_peak_duration_target,
                                                                                  start_month=05,start_day =01,
                                                                                  end_month=06, end_day=30,
                                                                                  flow_tolerance = flow_tol) %>% 
      slice(1:2) %>% 
      dplyr::select(duration_days,target_min_flow) %>% 
      pivot_wider(names_from = target_min_flow,values_from=duration_days ) %>% 
      mutate( target_flow_type =case_when(
        (`8070` >100) &  (`14350` >25) ~ 'Wet Year Improvement Flow',
        (`8070` >60) &  (`14350` >20) ~ 'Moderately Wet Year Improvement Flow',
        (`8070` >60) &  (`14350` >15) ~ 'Wet Year Maintenence Flow',
        (`8070` >40) &  (`14350` >10) ~ 'Moderately Wet Year Maintenence Flow',
        (`8070` >25) &  (`14350` >3) ~ 'Average Wet Year Improvement Flow',
        (`8070` >20) &  (`14350` >2) ~ 'Average Wet Year Maintenence Flow',
        (`8070` >15) &  (`14350` >0) ~ 'Average Dry Year Improvement Flow',
        (`8070` >10) &  (`14350` >0) ~ 'Average Dry Year Maintenence Flow / Moderately Dry Year Improvement Flow',
        (`8070` >=0) &  (`14350` >=0) ~ 'Dry Year Flow / Moderately Dry Year Maintenence Flow')
      ) %>% 
      mutate(result_desc=
               paste0('There is a duration of ',`8070`,' days >8070 cfs and ',`14350`,' days > 14350 cfs'))
    
    user_whitewater_base_eval_raw<- evaluate_min_flow_tgt_by_yr_type(Q_df=userQ_whitewater, target_meta_df=whitewater_base_meta,
                                                                     start_month=1,start_day =01,end_month=12,
                                                                     end_day=31, deficit_days_tolerance = duration_tol,
                                                                     flow_tolerance= flow_tol) %>%
      mutate(target_name = case_when(
        target_name == "wet & mod_wet" ~ 'Baseflow Minimums (Wet / Mod Wet Year Target)',
        target_name == "avg wet & avg dry" ~ 'Baseflow Minimums (Avg Wet / Avg Dry Year Target)',
        target_name == "mod dry" ~ 'Baseflow Minimums (Mod Dry Year Target)',
        target_name == "dry" ~ 'Baseflow Minimums (Dry Year Target)'),
        value=blw_base_flow
      )
    
    user_whitewater_base_eval_raw <<- 
      user_whitewater_base_eval_raw %>%
      ungroup() %>% 
      filter(result==TRUE) %>%
      slice_min(target_goal)
    
    user_whitewater_base_eval_result <- ifelse(
      nrow(user_whitewater_base_eval_raw>0),
      paste0('The ',user_whitewater_base_eval_raw$target_name[1],' target is met. This target is expected to be met in ',
             user_whitewater_base_eval_raw$target_goal[1], '% of years'),
      'No Baseflow Flow Minimum Is Met')
    
    #Recreation
    user_lg_rec<<- evaluate_duration_in_target_range(Q_df=userQ_whitewater,
                                                    target_meta_df=lower_gunnison_recreation_targets,
                                                    start_month=01,start_day =01,end_month=12, end_day=31,
                                                    flow_tolerance=0) %>%
      left_join(lower_gunnison_recreation_targets) %>% 
      mutate(target_name =
               factor(target_name, levels=c('Lower Acceptable',
                                            'Optimal',
                                            'Upper Acceptable',
                                            'Total')),
             target_type='Whitewater Recreation',
             status = ifelse(duration_days >= target_goal, 'Above or Equal', 'Below'),
             value= duration_days
      ) %>% arrange(target_name) %>%
      dplyr::select(water_year ,Type= target_type, Name = target_name, Metric = metric,
                    `Long Term Average`=target_goal, Result= duration_days , Status =status,Value=value) %>% 
      mutate(result_desc = paste0(Name,' Boatable Days are: ',Result,'. The long term average number of ',
                                  Name,' Boatable Days is ',`Long Term Average`))
    
    lg_rec_monthly_bda_custom<<- whitewater_BDA_class_custom %>% 
      filter(lg_WW !=  'Not Acceptable') %>% 
      mutate(month = month(date, label=TRUE),
             lg_WW=factor(lg_WW, 
                                    levels=c('Lower Acceptable','Optimal','Upper Acceptable'))) %>% 
      group_by(water_year,lg_WW , month,.drop=FALSE) %>% 
      tally()
    
    output$lg_rec_plot_custom<- renderPlot({
      lg_rec_monthly_bda_custom %>% 
        mutate(month = factor(month, levels=c('Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec','Jan','Feb','Mar'))) %>% 
        #filter(as.numeric(month) %in% 5:10) %>% 
        ggplot(aes(month,n,fill=fct_rev(lg_WW)))+
        geom_col()+
        labs(x='Month',y='Boatable Days',fill='User Preference')+
        theme_minimal(base_size=14)+
        theme(axis.text.x = element_text(angle = 90,vjust=0.5))+
        scale_fill_manual(values=c('#00BA38','#619Cff','#F8766D'))+
        scale_y_continuous(limits=c(0,31))
    })
    
    output$lg_rec_plot_total<- renderPlot({
      lg_rec_monthly_bda_custom %>% 
        group_by(lg_WW) %>% 
        summarize(n=sum(n)) %>% 
        #filter(as.numeric(month) %in% 5:10) %>% 
        ggplot(aes(x=1,n,fill=fct_rev(lg_WW)))+
        geom_col()+
        labs(x='',y='Total Boatable Days',fill='User Preference')+
        theme_minimal(base_size=14)+
        theme(axis.text.x = element_text(angle = 90,vjust=0.5),
              axis.text.y = element_blank())+
        scale_fill_manual(values=c('#00BA38','#619Cff','#F8766D'))+
        scale_y_continuous(breaks=pretty_breaks(n=10),limits=c(0,366))+
        coord_flip()
    })
    
    user_lg_table <-
      tibble(
        `Target Type` = c('24 Hour Instantaneous Peak Flow','High Flow Duration','Baseflows',
                          'Whitewater: Total','Whitewater: Upper Acceptable','Whitewater: Optimal','Whitewater: Lower'),
        Result = c(user_whitewater_spring_peak_result,
                   user_whitewater_high_flow_duration_result$result_desc[1],
                   user_whitewater_base_eval_result,
                   user_lg_rec$result_desc[4],user_lg_rec$result_desc[3],
                   user_lg_rec$result_desc[2],
                   user_lg_rec$result_desc[1])
      )
    
    output$LG_customQ_dt <-DT::renderDT({user_lg_table %>%
        datatable(rownames = FALSE,filter = 'top',extensions = 'RowGroup',
                  options=(list(pageLength = 15,rowsGroup = list(0))))
    })
  }) %>% 
    bindEvent(input$runButton_custom)
  
  
  observeEvent(input$evalButton, {
    show_spinner()
    Sys.sleep(1)
    hide_spinner()
  })
  
  output$clock <- renderText({
    invalidateLater(5000)
    Sys.time()
  })
}

# Run the application
shinyApp(ui = ui, server = server)

