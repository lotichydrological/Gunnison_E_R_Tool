
plot_GG_hydro_plot_customQ <- function(Q, linekey, dat){

  flw_target_dat_gunnison_gorge_wide <- dat %>%
    pivot_wider(names_from = name, values_from = value )
  
  ggplot(Q)+
    geom_blank( aes(date, q_cfs))+
    geom_ribbon_pattern(
      data=flw_target_dat_gunnison_gorge_wide %>%
        mutate(fill_name = 'Lower Acceptable Angling') %>%
        filter(month %in% 5:10),pattern_alpha=1,pattern_key_scale_factor = 0.5,
      aes(fake_date_customQ  ,ymin=min_acceptable_fishing,ymax=min_optimal_fishing,fill=fill_name,pattern=fill_name,group=year(fake_date_customQ)), alpha=1
    ) +
    geom_ribbon_pattern(
      data=flw_target_dat_gunnison_gorge_wide %>%
        mutate(fill_name = 'Optimal Angling') %>%
        filter(month %in% 5:10),pattern_alpha=1,pattern_angle=0,pattern_density=1,pattern_key_scale_factor = 0.5,
      aes(fake_date_customQ  ,ymin=min_optimal_fishing,ymax=max_optimal_fishing,fill=fill_name,pattern=fill_name,group=year(fake_date_customQ)), alpha=1
    ) +
    geom_ribbon_pattern(
      data=flw_target_dat_gunnison_gorge_wide %>%
        mutate(fill_name = 'Upper Acceptable Angling') %>%
        filter(month %in% 5:10),pattern_alpha=1,pattern_key_scale_factor = 0.5,
      aes(fake_date_customQ  ,ymin=max_optimal_fishing,ymax=max_acceptable_fishing,fill=fill_name,pattern=fill_name,group=year(fake_date_customQ)), alpha=1
    ) +
    geom_ribbon_pattern(
      data=flw_target_dat_gunnison_gorge_wide %>%
        mutate(fill_name = 'Lower Acceptable Boating') %>%
        filter(month %in% 5:10),pattern_alpha=.5,
      aes(fake_date_customQ  ,ymin=min_acceptable_whitewater,ymax=min_optimal_whitewater,fill=fill_name,pattern=fill_name,group=year(fake_date_customQ)), alpha=.2
    ) +
    geom_ribbon_pattern(
      data=flw_target_dat_gunnison_gorge_wide %>%
        mutate(fill_name = 'Optimal Boating') %>%
        filter(month %in% 5:10),pattern_alpha=.5,
      aes(fake_date_customQ  ,ymin=min_optimal_whitewater,ymax=max_optimal_whitewater,fill=fill_name,pattern=fill_name,group=year(fake_date_customQ)), alpha=.2
    ) +
    geom_ribbon_pattern(
      data=flw_target_dat_gunnison_gorge_wide %>%
        mutate(fill_name = 'Upper Acceptable Boating') %>%
        filter(month %in% 5:10),pattern_alpha=.5,
      aes(fake_date_customQ  ,ymin=max_optimal_whitewater,ymax=max_acceptable_whitewater,fill=fill_name,pattern=fill_name,group=year(fake_date_customQ)), alpha=.2
    )+
    scale_fill_manual(guide='legend',values=c('white','white','white','red','blue','green'),
                      #breaks=c('low_fish','opt_fish','high_fish','low_ww','opt_ww','high_ww'),
                      breaks = c(
                        'Lower Acceptable Angling','Optimal Angling','Upper Acceptable Angling',
                        'Lower Acceptable Boating','Optimal Boating','Upper Acceptable Boating'),
                      name='Recreational Preferences')+
    scale_pattern_manual(guide='legend',values=c('crosshatch',"wave", "circle",'none','none','none'),
                         #breaks=c('low_fish','opt_fish','high_fish','low_ww','opt_ww','high_ww'),
                         breaks = c(
                           'Lower Acceptable Angling','Optimal Angling','Upper Acceptable Angling',
                           'Lower Acceptable Boating','Optimal Boating','Upper Acceptable Boating'),
                         name='Recreational Preferences')+
    new_scale_fill() +
    geom_line(data=
                dat %>%
                filter(name %in% linekey$name),
              aes(fake_date_customQ, y=value, color=name, linetype=name),size=1.2)+
    scale_color_manual(guide='legend',values=linekey$line_colors,breaks=linekey$name,
                       labels=linekey$labels,name='Environmental Targets')+
    scale_linetype_manual(guide='legend',values=linekey$line_types,
                          breaks=linekey$name, labels=linekey$labels,name='Environmental Targets')+
    scale_x_date(date_labels = '%b', date_breaks = '1 months') +
    new_scale_color() +
    geom_line(linetype='solid',
              #col='#1b9e77',
              size=1.2,
              aes(date, q_cfs,group=name,col=factor(name)
              ))+
    scale_color_manual(guide='legend',values=c('black'),name='Custom Hydrograph')+
    theme_minimal()+
    scale_y_log10(breaks=c(100, 250, 500,750,1000,1500,2500,5000,10000 ))+
    labs(x='Month', y = "Flow (cfs)")
  
  }
